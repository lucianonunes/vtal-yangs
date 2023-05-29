from typing import Iterator, List, Tuple
from ncs.dp import Action
from ncs import maapi, maagic
import _ncs
from .dm import write_to_dmap, write_to_dmap2
from ..namespaces.lsaUtils_ns import ns as lsa_ns

import traceback

"""
    TODO: Schedule a thread to update the dispatch map.

    Note : Test to see if the core schedules the callback action in a thread, before converting
    the logic into a thread based.
"""


class DispatchMapUpdateNotifCallback(Action):
    """Dispatch-map-update notification handler callback
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Received notification on path: {input.path}")
        _ncs.dp.action_set_timeout(uinfo, 7200)
        with maapi.single_read_trans("", "system") as th:
            event = maagic.get_node(th, input.path)
            notification = maagic.cd(event, "../../")
            remote_node = maagic.cd(event, "../../../../../ncs:name")
            added = []
            deleted = []

            for device in notification.data.dispatch_map_update.device:
                if device.operation == "created":
                    added.append(tuple((device.name, device.ned_id)))
                if device.operation == "deleted":
                    deleted.append(device.name)

            write_to_dmap(self.log, added, deleted, remote_node)
            self.log.info(f"Dispatch Map Updated for: {input.path}")


"""
    TODO : Update the action to:
    1) Schedule the action in a thread (T1).
    2) Join the action thread (T1) for 15 sec.
    3) If the thread T1 is done with-in the time limit of 15 sec.
    return the sync done with success message.
        - - else - -
    return the sync done with success with a message informing sync action is moved to background
        since its taking more time.
    4) If the thread T1 is marked as background thread, then update scheduled tasks yang model
        with the status of the work.
    Note: handle race conditions.
        q) What happens T1 is done when join has returned because of the timeout ?
"""


class SyncDispatchMap(Action):
    """Action handler to sync dispatch-map
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info("Start Dispatch Map Sync")
        # time in seconds
        _ncs.dp.action_set_timeout(uinfo, 7200)
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            try:
                # This action should only be allowed on RFS nodes.
                if root.lsa_utils__lsa.role in [lsa_ns.lsa_utils_upper_layer_,
                                                lsa_ns.lsa_utils_non_lsa_]:
                    loc_devs = self.filter_localdm(input.remote_nso, th)
                    if input.remote_nso:
                        write_to_dmap2(self.log, self.remote_sysdevices(root, input.remote_nso),
                                       loc_devs, input.remote_nso)
                    else:
                        write_to_dmap2(self.log, self.sysdevices(root), loc_devs, input.remote_nso)

                    output.success = True
                    output.detail = "Dispatch Map Synced Successfully"
                else:
                    output.success = False
                    output.detail = (
                        "Sync is not allowed on RFS node. "
                        "'sync-dispatch-map' is a CFS node operation."
                    )
            except Exception as e:
                self.log.error(traceback.format_exc())
                output.success = False
                output.detail = f"Failed Dispatch Map Sync {str(e)}"

            self.log.info("Done Dispatch Map Sync")

    def filter_localdm(self, r_node: str, trans: int) -> List[str]:
        '''Identifies devices in dispatch-map for a given remote_node
        '''
        ldm_devices = []
        query = ('/ncs:devices/lsa-utils:lsa/lsa-utils:dispatch-map'
                 f'/lsa-utils:device[remote-node="{r_node}"]')
        if not r_node:
            query = ("/ncs:devices/lsa-utils:lsa/lsa-utils:dispatch-map/"
                     "lsa-utils:device[not(remote-node)]")

        qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th, query,
                                    "/ncs:devices/lsa-utils:lsa", 0, 1, _ncs.QUERY_STRING,
                                    ["name"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)

        for r in res:
            ldm_devices.append(r[0])

        _ncs.maapi.query_stop(trans.maapi.msock, qh)
        self.log.info(f"found {len(ldm_devices)} device(s) in dm_map associated with {r_node}")
        return ldm_devices

    def remote_sysdevices(self, root: maagic.Root, r_nso: str) -> Iterator[Tuple[str, str]]:
        '''Generator function for remotly managed device list
        '''
        for device in root.ncs__devices.device[r_nso].live_status.ncs__devices.device:
            yield tuple((device.name, self.get_device_ned_id(device)))

    def sysdevices(self, root: maagic.Root) -> Iterator[Tuple[str, str]]:
        '''Generator function for managed device list
        '''
        for device in root.ncs__devices.device:
            yield tuple((device.name, self.get_device_ned_id(device)))

    def get_device_ned_id(self, device: maagic.Node) -> str:
        '''Identifies device API type and returns the ned-id
        '''
        device_type_node = device.device_type
        if device_type_node.ne_type == "cli":
            return device_type_node.cli.ned_id
        elif device_type_node.ne_type == "generic":
            return device_type_node.generic.ned_id
        elif device_type_node.ne_type == "netconf":
            return device_type_node.netconf.ned_id
        elif device_type_node.ne_type == "snmp":
            return device_type_node.snmp.ned_id
