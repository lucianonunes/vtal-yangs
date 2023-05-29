from .namespaces.tapiConnectivityNanoPlanServices_ns import ns as tapiplan_ns


class StrConstants:
    # All Plan related
    ncs_self = "ncs:self"
    ncs_init = "ncs:init"
    ncs_ready = "ncs:ready"
    state_installed = (
        tapiplan_ns.prefix
        + ":"
        + tapiplan_ns.tapi_connectivity_nano_plan_services_installed_
    )
    state_config_apply = (
        tapiplan_ns.prefix
        + ":"
        + tapiplan_ns.tapi_connectivity_nano_plan_services_config_apply_
    )

    #### SERVICE PATHS ####
    zombie_service = '/ncs:zombies/ncs:service{{"{}"}}'

    cs_zombie_path = "/context/tapi-connectivity:connectivity-context" + \
        "/connectivity-service[uuid='{}']"
    cs_service_path = "/tapi-common:context/tapi-connectivity:connectivity-context" + \
        "/connectivity-service{{{}}}"

    # LifeCycle States
    planned = "PLANNED"
    potential_available = "POTENTIAL_AVAILABLE"
    potential_busy = "POTENTIAL_BUSY"
    installed = "INSTALLED"
    pending_removal = "PENDING_REMOVAL"

    ## NOTIFICATION REG EX ##
    lc_state = (
        r"/tapi-common\:context/tapi-connectivity\:connectivity-context"
        r"/tapi-connectivity\:connectivity-service\[tapi-connectivity\:uuid='(.*)'\]"
        r"/tapi-connectivity\:lifecycle-state"
    )
    cs_del = (
        r"/tapi-common\:context/tapi-connectivity\:connectivity-context"
        r"/tapi-connectivity\:connectivity-service\[tapi-connectivity\:uuid='(.*)'\]"
    )
    #### Miscellaneous ####
    system = "system"
