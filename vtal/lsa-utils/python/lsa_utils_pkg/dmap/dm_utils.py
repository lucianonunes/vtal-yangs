'''Utility APIs for work with dispatch-map
'''

from typing import Dict, List, Set, Union
import ncs
import _ncs


class DispatchMapException(Exception):
    '''Dispatch-Map exceptions base class
    '''
    def __init__(self, msg: str):
        super().__init__(msg)


class DeviceNotFound(DispatchMapException):
    '''Device is not found in dispatch-map
    '''
    pass


class DeviceMountedLocally(DispatchMapException):
    '''Device is local to the NSO.
    '''
    pass


def get_remote_nso(device: str) -> Union[None, str]:
    '''Find the remote nso managing the device.

    Returns None if the device is managed locally;
    Returns remote-nso name if the device is managed remotly.
    Throws DeviceNotFound if device is missing from dispatch-map.
    '''
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
        root = ncs.maagic.get_root(trans)
        return _get_remote_nso(root, device)


def _get_remote_nso(root: ncs.maagic.Root, device: str) -> Union[None, str]:
    '''Internal function.

    Find the remote nso managing the device,
    Returns None if the device is managed locally;
    Returns remote-nso name if the device is managed remotly.
    Throws DeviceNotFound if device is missing from dispatch-map.
    '''
    if device in root.ncs__devices.lsa_utils__lsa.dispatch_map.device:
        dm_entry = root.ncs__devices.lsa_utils__lsa.dispatch_map.device[device]
        if dm_entry.remote_node:
            return dm_entry.remote_node
        else:
            return None
    else:
        raise DeviceNotFound(f"deivce ({device}) not found in the devices/lsa/dispatch-map/device")


def get_remote_nso_live_status(root: ncs.maagic.Root, device: str) -> ncs.maagic.Container:
    '''Find the remote nso managing the device and retrieve remote-nso live-status node.

    Returns live-status container if the device is managed remotly.
    Throws DeviceNotFound if device is missing from dispatch-map.
    Throws DeviceMountedLocally exception if the device is managed by the local nso.
    '''
    rfs_node = _get_remote_nso(root, device)
    if rfs_node:
        return root.ncs__devices.device[rfs_node].live_status
    else:
        raise DeviceMountedLocally(f"device({device}) is managed locally.")


def get_all_remote_nso() -> Set[str]:
    '''Find all remote nso devices in the dispatch-map.

    Identifies and returns all the remote-nso devices identified by dispatch-map data.
    Returns a set of remote-nso device names
    '''
    rfs_nodes = []
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
        qh = _ncs.maapi.query_start(
            trans.maapi.msock,
            trans.th,
            "/ncs:devices/lsa-utils:lsa/lsa-utils:dispatch-map/lsa-utils:device[not(remote-node)]",
            "/ncs:devices/lsa-utils:lsa/lsa-utils:dispatch-map/",
            0,
            1,
            _ncs.QUERY_STRING,
            ["name"],
            [],
        )

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)
        root = ncs.maagic.get_root(trans)
        for r in res:
            # remote-node is a choice, this check avoids empty values.
            if r[0]:
                d_node = root.ncs__devices.device[r[0]]
                dt_node = d_node.device_type
                # If the device ned-id is netconf and has tailf-ncs module, it must be remote nso
                # currently it is not possible to identify if ned-id base identity is lsa-netconf
                # through APIs. This is a workaround
                if dt_node.ne_type == "netconf" and "http://tail-f.com/ns/ncs" in d_node.capability:
                    rfs_nodes.append(r[0])

        _ncs.maapi.query_stop(trans.maapi.msock, qh)
    return set(rfs_nodes)


def get_device_remote_nso(devices: List[str]) -> Dict[str, List[str]]:
    '''Find all remote-nso's for the list of devices.

    For each of the device in the devices list identifies remote-nso managing it and returns
    a dictionary(remote-nso, list[device]).
    Throws DeviceNotFound if device is missing from dispatch-map.
    '''
    rfs_node_dev_dict = {}
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
        root = ncs.maagic.get_root(trans)
        for device in devices:
            rfs_node = _get_remote_nso(root, device)
            if rfs_node not in rfs_node_dev_dict:
                rfs_node_dev_dict[rfs_node] = [device]
            else:
                rfs_node_dev_dict[rfs_node].append(device)
    return rfs_node_dev_dict
