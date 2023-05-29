from .namespaces.ciscoZrCfpNanoPlanServices_ns import ns as zrplan_ns


class StrConstants:
    # All Plan related
    ncs_self = "ncs:self"
    ncs_init = "ncs:init"
    ncs_ready = "ncs:ready"
    com_router = (
        zrplan_ns.prefix + ":" + zrplan_ns.cisco_zr_cfp_nano_plan_services_router_
    )
    state_config_apply = (
        zrplan_ns.prefix + ":" + zrplan_ns.cisco_zr_cfp_nano_plan_services_config_apply_
    )

    self_cn = "self"

    zr_service_xpath = "/zr/dco[name='{}'][router='{}']"
    zr_service_path = "/cisco-zr-cfp:zr/dco{{{} {}}}"

    asr9k_flex_cards = ["A9K-8HG-FLEX-TR", "A9K-8HG-FLEX-SE",
                        "A9K-20HG-FLEX-TR", "A9K-20HG-FLEX-SE"]
    a99_flex_cards = ["A99-10X400GE-X-TR", "A99-10X400GE-X-SE"]
    asr9903_flex_cards = ["A9903-20HG-PEC"]
    nc57_vigor_flex_cards = ["NC57-18DD-SE", "NC57-36H6D-S"]
    nc57_mpa_flex_cards = ["NC57-MPA-2D4H-S"]
    ncs540_pids = ["N540-24Q8L2DD-SYS"]
