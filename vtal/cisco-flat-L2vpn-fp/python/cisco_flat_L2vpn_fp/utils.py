import ncs
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import (
    get_status_code_class,
)

is_lsa = False
l2vpn_servicepoint = "flat-L2vpn-external"
l2vpn_validation_callpoint = "flat-L2vpn-validation"


def is_lsa_setup():
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as th:
        global is_lsa
        try:
            root = ncs.maagic.get_root(th)
            # If this internal service path exists, it is not LSA
            non_lsa_path = root.\
                cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_internal_local_site_service
            if non_lsa_path:
                is_lsa = False
        except Exception:
            is_lsa = True


def save_status_code(error_msg, plan, component, log, device_name):
    # populate plan with status code details
    if "out of sync" in error_msg:
        status_code = StatusCodes[StatusCodes.DEVICE_OUT_OF_SYNC.name]
    elif "connection refused" in error_msg:
        status_code = StatusCodes[StatusCodes.CONNECTION_FAILURE.name]
    else:
        status_code = StatusCodes[StatusCodes.CONFIG_FAILURE.name]

    status_class = get_status_code_class(status_code)
    e = status_class(log, status_code, error_msg)
    e = e.set_context(e.statusCode.reason, error_msg).finish()
    e.save_to_plan(plan, component, device_name)


def get_internal_plan_path(root, is_local_site):
    if is_local_site is None:
        return get_site_internal_plan_path(root)
    elif is_local_site:
        return get_local_site_internal_plan_path(root)
    else:
        return get_remote_site_internal_plan_path(root)


def get_site_internal_plan_path(root):
    if is_lsa:
        internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L2vpn_site_plan
    else:
        internal_plan_path = root.\
            cisco_flat_L2vpn_fp_internal_site__flat_L2vpn_internal_site.\
            cisco_flat_L2vpn_fp_internal_site__flat_L2vpn_plan
    return internal_plan_path


def get_local_site_internal_plan_path(root):
    if is_lsa:
        internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L2vpn_local_site_plan
    else:
        internal_plan_path = root.\
            cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_internal_local_site.\
            cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_plan
    return internal_plan_path


def get_remote_site_internal_plan_path(root):
    if is_lsa:
        internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L2vpn_remote_site_plan
    else:
        internal_plan_path = root.\
            cisco_flat_L2vpn_fp_internal_remote_site__flat_L2vpn_internal_remote_site.\
            cisco_flat_L2vpn_fp_internal_remote_site__flat_L2vpn_plan
    return internal_plan_path
