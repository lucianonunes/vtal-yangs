# -*- mode: python; python-indent: 4 -*-
import ncs
from cisco_tsdn_core_fp_common.constants import (
    L3NM_SERVICE_PATH,
    L3NM_PLAN_PATH,
    ZOMBIE_PATH
)

is_lsa = False
ietf_l3vpn_servicepoint = "ietf-l3vpn-ntw-servicepoint"
ietf_l3vpn_validation_callpoint = "ietf-l3vpn-nm-validation"


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


def get_l3nm_service_kp(service_name):
    return f"{L3NM_SERVICE_PATH}{{{service_name}}}"


def get_l3nm_plan_kp(service_name):
    return (f"{L3NM_PLAN_PATH}{{{service_name}}}")


def get_l3nm_zombie_kp(service_name):
    zombie_service_path = (f"{L3NM_SERVICE_PATH}[vpn-id='{service_name}']")
    return f'{ZOMBIE_PATH}{{"{zombie_service_path}"}}'
