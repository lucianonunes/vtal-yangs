from .namespaces.ciscoRonCfpNanoPlanServices_ns import ns as ronplan_ns


class StrConstants:
    # All Plan related
    ncs_self = "ncs:self"
    ncs_init = "ncs:init"
    ncs_ready = "ncs:ready"
    com_optical_controller = (
        ronplan_ns.prefix
        + ":"
        + ronplan_ns.cisco_ron_cfp_nano_plan_services_optical_controller_
    )
    com_router = (
        ronplan_ns.prefix + ":" + ronplan_ns.cisco_ron_cfp_nano_plan_services_router_
    )
    state_conf_apply = (
        ronplan_ns.prefix
        + ":"
        + ronplan_ns.cisco_ron_cfp_nano_plan_services_config_apply_
    )
    post_action_states_to_avoid = ["create-init", "failed"]

    #### SERVICE PATHS ####
    zombie_service = '/ncs:zombies/ncs:service{{"{}"}}'

    ron_service_xpath = "/ron/ron-ml[name='{}']"
    zr_service_xpath = "/zr/dco[name='{}'][router='{}']"
    zr_plan_path = "/cisco-zr-cfp:zr/dco-plan{{{} {}}}/plan/component{{ncs:self self}}"
    #### Miscellaneous ####
    system = "system"
    self_cn = "self"
    optical_cn = "Optical-Controller"

    ##### Opaque Keys #####
    validation_error_key = "VALIDATION_ERROR"
    edit_key = "EDIT"
    cq_data_key = "CQ_DATA"
    zrs_ready_key = "ZRS_READY"
    cs_uuid_key = "CS_UUID"
    cs_done_key = "CS_DONE"
    past_create_state_key = "PAST_CREATE_STATE"
    optical_controller_key = "OPTICAL_CONTROLLER_KEY"
