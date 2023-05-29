# -*- mode: python; python-indent: 4 -*-
import ncs
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import (
    get_status_code_class,
)

is_lsa = False
l3vpn_servicepoint = "flat-L3vpn-external"
l3vpn_validation_callpoint = "flat-L3vpn-validation"


def is_lsa_setup():
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as th:
        global is_lsa
        try:
            root = ncs.maagic.get_root(th)
            # If this path exists, it is not LSA
            non_lsa_path = root.cisco_flat_L3vpn_fp_internal__flat_L3vpn
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


# Moved from router module
def getIpAddress(addr):
    """Return the Ip part of a 'Ip/Net' string."""
    parts = addr.split("/")
    return parts[0]


def get_keys_by_values(item_dict, values_list):
    keys = []
    dict_items = item_dict.items()
    for item in dict_items:
        if item[1] in values_list:
            keys.append(item[0])
    return keys
