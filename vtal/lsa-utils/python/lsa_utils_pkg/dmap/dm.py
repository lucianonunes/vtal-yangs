# -*- mode: python; python-indent: 4 -*-
from typing import Any, Iterator, List, Tuple
from ncs import RUNNING
from ncs import maagic, maapi
import traceback

from .dm_notification import send_dmap_update_notif
from ..alarmsink import Alarm, Severity
from ..namespaces.lsaUtils_ns import ns as lsa_ns


def process_dd_change(log: Any, added: List[str], deleted: List[str]) -> None:
    """Processes the devices device changes.

    based on the lsa role, local dispatch-map will be updated or a notification will be sent to
    remote upper-level nso.
    """
    '''
    Truth Table:
    +--------+-----------+--------------+
    | Role   | Local Map | Notification |
    +--------+-----------+--------------+
    | CFS    |  Yes      |  No          |
    +--------+-----------+--------------+
    | RFS    |  No       |  Yes         |
    +--------+-----------+--------------+
    '''
    with maapi.single_read_trans("", "system", db=RUNNING) as th:
        root = maagic.get_root(th)
        if root.lsa_utils__lsa.role in [lsa_ns.lsa_utils_upper_layer_, lsa_ns.lsa_utils_non_lsa_]:
            write_to_dmap(log, added, deleted)
        else:
            send_dmap_update_notif(log, added, deleted)


def write_to_dmap(log: Any, added: List[Tuple[str, str]], deleted: List[str] = [],
                  remote_node: str = None) -> None:
    '''Writes to the devices/lsa/dispatch-map/device

    set :param remote_node: to None if device(s) are managed locally,
    otherwise provide the remote_node device name.
    '''
    log.info("Writing to the dispatch-map")
    log.info(f"{len(added)} device(s) are scheduled to add to dm")
    # Add devices
    with maapi.single_write_trans("", "system", db=RUNNING) as th:
        root = maagic.get_root(th)
        dm = root.ncs__devices.lsa_utils__lsa.dispatch_map.device
        for device, ned_id in added:
            # If the deivce exist in dm then check if this is a duplicate.
            if device in dm:
                dm_entry = dm[device]
                # If the managed nso details doesn't match then there are two devices
                #  in the environment with the same name but managed by different NSO's
                if not _match(dm_entry, remote_node):
                    # Raise dupdn alarm
                    _gen_dupdn_alarm(log, device, dm_entry.remote_node, remote_node)
                    # Device is duplicate, duplicate-device-name alarm is raised
                    # Move on to the next device in the queue
                    continue

            entry = dm.create(device)
            entry.ned_id = ned_id
            if remote_node:
                entry.remote_node = remote_node
        th.apply()
    # Remove devices
    _delete_dm_entry(log, deleted, remote_node)


def write_to_dmap2(log: Any, dd_tree_devices: Iterator[Tuple[str, str]],
                   local_dm_devices: List[str], remote_node: str = None) -> None:
    '''An overlaod function to write_to_dmap that takes devices in devices/device tree and
    devics in local dispatch-map for a given remote_node and updates the dispatch-map to sync
    with devices/deivce.
    '''
    log.info("Writing2 to the dispatch-map")
    # Add devices
    with maapi.single_write_trans("", "system", db=RUNNING) as th:
        root = maagic.get_root(th)
        dm = root.ncs__devices.lsa_utils__lsa.dispatch_map.device
        for device, ned_id in dd_tree_devices:
            # If the deivce exist in dm then check if this is a duplicate.
            if device in dm:
                dm_entry = dm[device]
                # If the managed nso details doesn't match then there are two devices
                #  in the environment with the same name but managed by different NSO's
                if not _match(dm_entry, remote_node):
                    # Raise dupdn alarm
                    _gen_dupdn_alarm(log, device, dm_entry.remote_node, remote_node)
                    # Device is duplicate, duplicate-device-name alarm is raised
                    # Make sure you dont remove
                    if device in local_dm_devices:
                        local_dm_devices.remove(device)
                    # Move on to the next device in the queue
                    continue

            entry = dm.create(device)
            entry.ned_id = ned_id
            if remote_node:
                entry.remote_node = remote_node
            # Filter out local dm entries for the remote_node. (calculate the diff)
            if device in local_dm_devices:
                local_dm_devices.remove(device)
        th.apply()
    # Remove devices
    _delete_dm_entry(log, local_dm_devices, remote_node)


def _delete_dm_entry(log, del_list: List[str], remote_node: str = None) -> None:
    '''Internal Function : delete the devices from dispatch-map
    '''
    log.info(f"{len(del_list)} device(s) are scheduled to remove from dm")
    log.debug(f"removed devices {del_list}")
    for device in del_list:
        try:
            with maapi.single_write_trans("", "system", db=RUNNING) as th:
                root = maagic.get_root(th)
                dm = root.ncs__devices.lsa_utils__lsa.dispatch_map.device
                if device in dm:
                    dm_entry = dm[device]
                    # If the managed nso details doesn't match then there are two
                    # devices in the environment with the same name but managed by different NSO's
                    # In which case this check will avoid deleting wrong device
                    if _match(dm_entry, remote_node):
                        del dm[device]
                    else:
                        # This is a duplicate device delete, so clear if we have alarm for it.
                        _gen_dupdn_alarm(log, device, dm_entry.remote_node,
                                         remote_node, Severity.CLEARED)
                th.apply()
        except Exception as exp:
            log.error(traceback.format_exc())
            _gen_dm_per_failure(log, device, str(exp))
        else:
            # If there are any persistance alarms for the device clear them
            # NOTE: future workflow, currently operator has to manually clear and purge the alarm
            _gen_dm_per_failure(log, device, f"device {device} deleted from dispatch-map",
                                Severity.CLEARED)


def _match(dm_entry: maagic.Node, remote_node: str) -> bool:
    '''Internal Function : Check if the device is managed by the same NSO node
    '''
    return (dm_entry.remote_node and dm_entry.remote_node == remote_node) \
        or (not dm_entry.remote_node and not remote_node)


def _gen_dupdn_alarm(log: Any, device: str, orig_remote_node: str, new_remote_node: str,
                     severity: Severity = Severity.MAJOR) -> None:
    '''Generate duplicate-device-name alarm
    '''
    # Prepare an alarm and submit
    al_type = lsa_ns.prefix + ":" + lsa_ns.lsa_utils_duplicate_device_name_

    man_obj = "local"
    if new_remote_node:
        man_obj = f"/ncs:devices/ncs:device[ncs:name='{new_remote_node}']"

    spec_msg = f"{device} name is a duplicate"

    if orig_remote_node:
        orig_remote_node = f"/ncs:devices/ncs:device[ncs:name='{orig_remote_node}']"
    else:
        orig_remote_node = "local"

    al_txt = (f"Newly added {device} device on {man_obj} NSO has the same name as"
              f" the device {device} managed by {orig_remote_node} NSO.")
    if severity == Severity.CLEARED:
        al_txt = f"The duplicate device {device} added on {man_obj} NSO has been deleted."

    impct_obj = ("/ncs:devices/lsa-utils:lsa/lsa-utils:dispatch-map"
                 f"/lsa-utils:device[lsa-utils:name='{device}']")

    Alarm(al_type, man_obj, spec_msg).set_severity(severity) \
        .set_alarm_text(al_txt) \
        .set_impacted_objects([impct_obj]) \
        .flush()

    if severity != Severity.CLEARED:
        log.error(al_txt)


def _gen_dm_per_failure(log: Any, device: str, al_txt: str,
                        severity: Severity = Severity.MAJOR) -> None:
    '''Generates dispatch-map-persist-failure alarm.
    '''

    # Prepare an alarm and submit
    al_type = lsa_ns.prefix + ":" + lsa_ns.lsa_utils_dispatch_map_persist_failure_

    man_obj = ("/ncs:devices/lsa-utils:lsa/lsa-utils:dispatch-map"
               f"/lsa-utils:device[lsa-utils:name='{device}']")

    Alarm(al_type, man_obj).set_severity(severity) \
        .set_alarm_text(al_txt) \
        .flush()

    if severity != Severity.CLEARED:
        log.error(al_txt)
