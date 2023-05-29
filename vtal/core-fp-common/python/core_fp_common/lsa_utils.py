'''
Common methods related to LSA:
  get_rfs_node
  get_rfs_node_live_status
  get_all_rfs_nodes
  get_device_rfs_nodes
'''
import ncs
import _ncs


def get_rfs_node(root, device):
    return root.core_fp_common__dispatch_map[device].rfs_node


def get_rfs_node_live_status(root, device):
    rfs_node = root.core_fp_common__dispatch_map[device].rfs_node
    return root.ncs__devices.device[rfs_node].live_status


def get_all_rfs_nodes():
    rfs_nodes = []
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
        qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                    "/core-fp-common:dispatch-map",
                                    '/', 0, 1,
                                    _ncs.QUERY_STRING, ["rfs-node"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)

        for r in res:
            rfs_nodes.append(r[0])

        _ncs.maapi.query_stop(trans.maapi.msock, qh)
    return set(rfs_nodes)


def get_device_rfs_nodes(root, devices):
    rfs_node_dev_dict = {}
    for device in devices:
        rfs_node = root.core_fp_common__dispatch_map[device].rfs_node
        if rfs_node not in rfs_node_dev_dict:
            rfs_node_dev_dict[rfs_node] = [device]
        else:
            rfs_node_dev_dict[rfs_node].append(device)
    return rfs_node_dev_dict
