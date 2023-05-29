'''
Common utils, avaliable APIs:
    get_local_user()
'''
import ncs
import _ncs
from _ncs import cdb
import socket


def get_local_user():
    '''
        Reads cfp-local-user from core-fp-common.yang.
        local-user should be used for all maapi sessions.
    '''
    with ncs.maapi.single_read_trans("", "system") as th:
        root = ncs.maagic.get_root(th)
        return root.core_fp_common__cfp_local_user


def create_cdb_session():
    # WRITE OPER DATA
    sock_cdb_oper = socket.socket()

    # If a custom NCS listening port is set with the environment variable
    # NCS_IPC_PORT, the constant _ncs.PORT will be set to that value.
    cdb.connect(sock_cdb_oper, type=cdb.DATA_SOCKET, ip='127.0.0.1', port=_ncs.PORT)
    cdb.start_session(sock_cdb_oper, cdb.OPERATIONAL)
    return sock_cdb_oper


def end_cdb_session(sock_cdb_oper):
    try:
        cdb.end_session(sock_cdb_oper)
    except Exception:
        return
    return


def safe_create(sock_cdb_oper, path):
    if not cdb.exists(sock_cdb_oper, path):
        cdb.create(sock_cdb_oper, path)
    return


def safe_set_elem(sock_cdb_oper, value, path):
    if value is not None:
        cdb.set_elem(sock_cdb_oper, value, path)
    return


def set_elem(sock_cdb_oper, value, path):
    cdb.set_elem(sock_cdb_oper, value, path)
    return


def safe_delete(sock_cdb_oper, path):
    if cdb.exists(sock_cdb_oper, path):
        cdb.delete(sock_cdb_oper, path)
    return


def get_device_ned_id(device):
    with ncs.maapi.single_read_trans("", "system") as th:
        root = ncs.maagic.get_root(th)
        device_type_node = root.devices.device[device].device_type
        if device_type_node.ne_type == "cli":
            return device_type_node.cli.ned_id

        elif device_type_node.ne_type == "generic":
            return device_type_node.generic.ned_id

        # Default
        elif device_type_node.ne_type == "netconf":
            return device_type_node.netconf.ned_id

        elif device_type_node.ne_type == "snmp":
            return device_type_node.snmp.ned_id

        else:
            return None


def get_ncs_major_version(root=None):
    ncs_vsn = None
    if root:
        ncs_vsn = str(root.ncs_state.version)
    else:
        with ncs.maapi.single_read_trans("", "system") as th:
            root = ncs.maagic.get_root(th)
            ncs_vsn = str(root.ncs_state.version)
    if ncs_vsn:
        vsn_major = ncs_vsn[:ncs_vsn.index(".") + 1]
        ncs_vsn = ncs_vsn[len(vsn_major):]
        if "." in ncs_vsn:
            vsn_minor = ncs_vsn[:ncs_vsn.index(".")]
        else:
            vsn_minor = ncs_vsn
        ncs_vsn = float(vsn_major + vsn_minor)
    return ncs_vsn


def get_service_operation(op):
    if op == _ncs.dp.NCS_SERVICE_CREATE:
        return "SERVICE_CREATE"
    elif op == _ncs.dp.NCS_SERVICE_UPDATE:
        return "SERVICE_UPDATE"
    else:
        return "SERVICE_DELETE"
