from collections import namedtuple
from enum import Enum

Code = namedtuple(
    "Code",
    [
        "code",
        "reason",
        "category",
        "severity",
        "recommended_actions",
    ],
    defaults=["INFO", "Undefined"],
)


class StatusCodes(Enum):
    """
    Status code(s) describe a particular action or an event that occurred in the system.
    When used with context settings in exceptions, it helps identify the scope and type of
    the problem encountered. Core FP standardization calls for the following scopes:

    100-199     |   Informational
    200-299     |   Success
    300-399     |   Device config errors
    400-499     |   User errors
    500-599     |   Custom action errors
    600-699     |   Device/Vnf on-boarding Errors
    700-799     |   Resouce allocation errors
    800-999     |   Service/RFM Business logic errors
    1000-above  |   Extension Errors
    """

    _ignore_ = "error warning fatal"
    error = "ERROR"
    warning = "WARNING"
    fatal = "FATAL"
    ## PROGRESS and READY CODES.
    INPROGRESS = Code(
        101,
        "Service intent received and deployment config is in-progress",
        "Informational",
    )
    CQ_INPROGRESS = Code(
        102,
        "Service intent received. Device modifications are submitted to"
        " commit-queue and commit-queue is in-progress",
        "Informational",
    )
    DELETE_INPROGRESS = Code(
        110,
        "Intent to delete service received and device config rollback is in-progress",
        "Informational",
    )
    ROLLBACK_TRIGGERED = Code(
        198,
        "Service config has been rolled back because of ZR Configuration failure",
        "Informational",
        error,
        "The ZR configuration has failed either due to connectivity issues or"
        " incorrect configuration parameters. Debug the issue and set the clear-rollback"
        " flag to indicate the intent to re-instantiate the RON-ml service."
        " The clear-rollback flag takes a random integer number."
        " Example: set ron ron-ml RON_ML_E2E clear-rollback 15."
        " Check the ncs-python-vm-cisco-zr-cfp.log file for error details."
    )
    MULTIPLE_COMPONENT_FAILURE = Code(
        199, "Multiple-components have failed", "Informational"
    )
    READY_SUCCESS = Code(
        200, "Service deployment is successful", "Informational"
    )

    ## 300 - 399 : Device Config Errors
    # All errors while pushing configs to devices/VNFs through templates in CFP.
    CONNECTION_FAILURE = Code(
        301,
        "Device unreachable",
        "device",
        error,
        "Check device connectivity from NSO and perform the recovery steps."
    )
    DEVICE_OUT_OF_SYNC = Code(
        302,
        "Device out of sync",
        "device",
        error,
        "Check sync between the device and NSO and perform recovery steps."
    )
    CONFIG_FAILURE = Code(
        303,
        "Config push failed",
        "device",
        error,
        "Device configuration has been rejected."
        " Fix the service payload and perform recovery steps."
    )
    CS_FAILED_ON_ONC = Code(
        304,
        "Connectivity-service initiated on the optical-controller has failed",
        "device",
        error,
        "Unknown reason for the connectivity-service failure. Debug the optical-controller server."
    )
    DATA_RETRIVAL_ERROR = Code(
        305,
        "Failed to read endpoint optics data from the optical-controller after"
        " successful connectivity-service deployment.",
        "device",
        error,
        "The error may be because of a device reachability issue, or "
        "the data on the owned-node-edge-point of the optical controller is missing or corrupt,"
        " or any other issue."
        " Check ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    ## 400 - 499 : User Errors
    # All model validation errors though validate callback action in CFP.
    STATUS_CODE_NOT_LOADED = Code(
        400,
        "Status code mapping has not been loaded for the function pack "
        "during installation",
        "user",
        error,
        "Bootstrap status code mapping for the CFP is missing."
        " Load merge the RON-status-codes.xml file."
    )
    DYNAMIC_CLASS_NOT_FOUND = Code(
        407,
        "dynamic-device-mapping implementation for device not found",
        "user",
        error,
        "Set the correct path to the python implementation for the device ned-id "
        "in cfp-configurations/dynamic-device-mapping list"
    )
    NED_NOT_SUPPORTED = Code(
        408,
        "Unsupported NED. cfp-configuration setting for the ned-id is missing.",
        "user",
        error,
        "Ensure cfp-configurations/dynamic-device-mapping is set for the device ned-id."
    )
    DYNAMIC_METHOD_NOT_FOUND = Code(
        409,
        "Required API method is missing in dynamic-device-mapping python implementation.",
        "user",
        error,
        "Ensure all the CFP required APIs are implemented in"
        " the dynamic-device-mapping python implementation for the device ned-id."
    )
    PLUGGABLE_CAPABILITY_NOT_SUPPORTED = Code(
        410,
        "Hardware transceiver capability does not support the configured mode and bandwidth.",
        "user",
        error,
        "Ensure the hardware transceiver capability on the device supports"
        " the configured mode and bandwidth."
    )
    INTERFACE_INDEX_OUT_OF_BOUND = Code(
        412,
        "Interface index is out-of-bound for the configured mode and bandwidth.",
        "user",
        error,
        "Use an interface index within the allowed range for a given mode and bandwidth."
    )
    INTER_LAYER_LINK_NOT_FOUND = Code(
        413,
        "Cannot find inter-layer-link entry",
        "user",
        error,
        "Create an inter-layer-link entry matching the router/port and optical controller"
    )
    OPTICS_SIP_NOT_FOUND = Code(
        415,
        "Optics SIP is not found on the optical controller",
        "user",
        error,
        "Optical controller does not have information for SIP."
        " Check on the optical-controller and fix the inter-layer-link entry."
        " Redeploy the service to re-trigger the deployment."
    )
    OPTICAL_CONTROLLER_NOT_CONFIGURED = Code(
        416,
        "Optical Controller is not configured under inter-layer-link entry",
        "user",
        error,
        "Configure either a default optical controller that is applicable to all"
        " the inter-layer-link entries or a specific optical controller for each"
        " inter-layer-link entry and redeploy the service."
    )
    NO_DOMAIN_SET = Code(
        417,
        "No domain is selected to deploy the RON-ml service."
        " Either ron-ml/end-point/terminal-device-packet, or ron-ml/ols-domain, "
        "or both must be set on the service definition.",
        "user",
        error,
        "Ron-ml service can be deployed in router-only, ols-only, or both"
        "(router and ols) modes. The service definition is missing the mode in which"
        " to deploy the RON-ml service. Verify the service payload and redeploy the service."
    )
    MULTIPLE_CONTROLLERS_FOUND = Code(
        418,
        "Multiple optical controllers detected for the link stated in the service.",
        "user",
        error,
        "Correct the inter-layer-link such that a single optical controller"
        " manages the SIPs involved in the service."
    )
    SERVICE_PAST_INIT_STATE = Code(
        419,
        "RON-ml service has passed the service init-state. You cannot change from"
        " a ZR only/Optical only service to an end-to-end service or update the mode,"
        " bandwidth, and line-port information.",
        "user",
        error,
        "To modify any configuration parameters, delete and re-create the service with"
        " the updated configurations."
    )
    CHANGES_AFFECT_ONC_DEPLOYMENT = Code(
        420,
        "Allowed service configuration changes are srlg,"
        " terminal-device-packet/bundle, terminal-device-packet/interface, and"
        " terminal-device-packet/custom-templates (if applicable).",
        "user",
        error,
        "RON-ml service has passed the optical service creation stage"
        " and the allowed service configuration changes are srlg,"
        " terminal-device-packet/bundle, terminal-device-packet/interface, and"
        " terminal-device-packet/custom-templates (if applicable)."
        " To modify any configuration parameters, delete and re-create the service with"
        " updated configurations."
    )
    INTER_LAYER_LINK_DB_IN_USE = Code(
        421,
        "Cannot edit or delete the inter-layer-link.",
        "user",
        error,
        "A RON-ml service is using the inter-layer-link."
        " Delete the service to edit or delete the inter-layer-link."
    )
    MODE_AND_BANDWIDTH_NOT_SUPPORTED_ON_THE_MODULE = Code(
        422,
        "Deployment mode and bandwidth are currently not supported by the line-card module",
        "user",
        error,
        "Select the allowed deployment mode and bandwidth for the module in use."
    )
    LINE_PORT_ALREADY_IN_USE = Code(
        423,
        "The line-port requested for configuration on device is already in use for another service",
        "user",
        error,
        "Change the line-port for the requested service and try again"
    )
    ONC_NETCONF_NOTIFICATION_ERROR = Code(
        424,
        "Netconf Subscription for requested ONC is not configured or running",
        "user",
        error,
        "Register a netconf subscription for the ONC device and ensure that it is running"
    )
    CHANGES_AFFECT_ZR_DEPLOYMENT = Code(
        425,
        "Allowed service configuration changes are srlg,"
        " terminal-device-packet/bundle, terminal-device-packet/interface, and"
        " terminal-device-packet/custom-templates (if applicable).",
        "user",
        error,
        " Ron-ml service has passed the ZR service creation stage"
        " and allowed service configuration changes are srlg,"
        " terminal-device-packet/bundle, terminal-device-packet/interface, and"
        " terminal-device-packet/custom-templates (if applicable)."
        " If some of the blocked configuration parameters needed"
        " to be changed, please delete and re-create the ron-ml service with"
        " updated configurations."
    )
    INVALID_DEVICE_ERROR = Code(
        426,
        "The device specified in the service or action is invalid",
        "user",
        error,
        " The device specified in the service or action is a NSO node."
    )

    ## 500 - 599 : Custom Action Errors
    # All errors during custom request actions in CFP.
    CLEANUP_ERROR = Code(
        503,
        "Cleanup failed",
        "custom-action",
        error
    )
    RECOVERY_ERROR = Code(
        504,
        "Failed to recover ron-ml service",
        "custom-action",
        error,
        "Check ncs-python-vm-cisco-ron-cfp.log for detailed error."
    )
    FETCH_SIP_ERROR = Code(
        505,
        "Fetch Optics failed",
        "custom-action",
        error
    )
    FETCH_OPTICS_ERROR = Code(
        506,
        "Fetch Optics failed",
        "custom-action",
        error
    )
    SUBSCRIPTION_ERROR = Code(
        507,
        "Failed to carry out subscription activity",
        "custom-action",
        error,
        " Check ncs-python-vm-cisco-optical-cfp.log for detailed error."
    )
    ZOMBIE_CLEAN_SYNC_ERROR = Code(
        508,
        "Zombie Clean and Sync failed",
        "custom-action",
        error
    )
    INTERNAL_REDEPLOY_ACTION_ERROR = Code(
        510,
        "Redeploy custom action error",
        "custom-action",
        error,
        "If the RON-ml service is in create mode, check the ncs-python-vm-cisco-ron-cfp.log"
        " file for error details. If the RON-ml service is in delete mode,"
        " the init rollback may have failed or the parent is deleted."
        " Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    PROCESS_FAILURE_ACTION_ERROR = Code(
        511,
        "Process failure action error",
        "custom-action",
        error,
        "Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    REMOTE_KICKER_HANDLER_ERROR = Code(
        512,
        "Remote kicker handler",
        "custom-action",
        error,
        "Either the RFS has become unreachable or"
        " the internal plan data on the remote node has changed or"
        " the parent ron-ml service has been deleted."
        " Check ncs-python-vm-cisco-ron-cfp.log for detailed error."
    )
    PLAN_FAILURE_CALLBACK_ERROR = Code(
        513,
        "Plan failure callback has failed",
        "custom-action",
        error,
        " Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    ## 600 - 699 : Device/VNF On-boarding Errors
    # All errors during VNF Manager and VNF deployment and on-boarding through NFVO, device
    # on-boarding through PnP etc..
    ## 700 - 799 : Resource Allocation Errors
    # All errors during ip and id allocations through resource manager (RM) and placement errors
    # through resource orchestration (RO) in CFP.

    ## 800 - 899 : Service Errors
    # All other CFP errors except the above 6 which may include, errors in RFM, Subscribers,
    # Utility methods, CDB, Plan etc.. CFP can sub divide the range in service errors range.
    # General Service Status Codes
    # 850-859 : CDB OPER ERRORS
    # 860-869 : CDB CONFIG ERRORS
    PRE_MOD_CALLBACK_ERROR = Code(
        801,
        "Service pre-modification callback has failed",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    ZR_FP_PRE_MOD_CALLBACK_ERROR = Code(
        802,
        "Service pre-modification callback has failed",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-zr-cfp.log file for error details."
    )
    OP_FP_PRE_MOD_CALLBACK_ERROR = Code(
        803,
        "Service pre-modification callback has failed",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    OP_FP_POST_MOD_CALLBACK_ERROR = Code(
        804,
        "Service post-modification callback has failed",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    SELF_CALLBACK_ERROR = Code(
        810,
        "Self component callback has failed",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    OP_INIT_RB_ERROR = Code(
        811,
        "Failed to handle optical component init rollback",
        "service",
        error,
        "The init state rollback depends on the internal optical stacked-service"
        " plan. Debug the internal plan data for any missing or faulty entries."
        " Check ncs-python-vm-cisco-ron-cfp.log for detailed error."
    )
    OPTICAL_FP_INIT_RB_ERROR = Code(
        812,
        "Failed to handle connectivity-service init rollback",
        "service",
        error,
        "Check the ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    ZR_INIT_RB_ERROR = Code(
        813,
        "Failed to handle router component init rollback",
        "service",
        error,
        "The init state rollback depends on the internal terminal-device"
        " stacked-service plan. Debug the internal plan data for any missing or incorrect entries."
        " Check ncs-python-vm-cisco-ron-cfp.log for detailed error."
    )
    OP_READY_ERROR = Code(
        814,
        "Failed to process optical ready create",
        "service",
        error,
        "The ready state depends on the internal optical stacked-service "
        " plan. Debug the internal plan data for any missing or incorrect entries."
        " Check ncs-python-vm-cisco-ron-cfp.log for detailed error."
    )
    OPTICAL_FP_READY_ERROR = Code(
        815,
        "Failed to process connectivity service ready create",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    ZR_READY_ERROR = Code(
        816,
        "Failed to process terminal-device ready create",
        "service",
        error,
        "The ready state depends on the internal terminal-device stacked-service"
        " plan. Debug the internal plan data for any missing or incorrect entries."
        " Check ncs-python-vm-cisco-ron-cfp.log for detailed error."
    )
    ZR_FP_READY_ERROR = Code(
        817,
        "Failed to process dco ready create",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-zr-cfp.log file for error details."
    )
    OPTICAL_CONFIG_ERROR = Code(
        821,
        "Failed to process and initiate ols-domain config on optical cfp",
        "service",
        error,
        "Verify the service payload for any missing dependencies and data validity checks."
        " Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    OPTICAL_FP_CONFIG_ERROR = Code(
        822,
        "Service config-apply state callback has failed",
        "service",
        fatal,
        "Verify the service payload for any missing dependencies and data validity checks."
        " Check the ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    OPTICAL_FP_INSTALL_ERROR = Code(
        823,
        "Service installed state callback has failed",
        "service",
        fatal,
        "Check the ncs-python-vm-cisco-optical-cfp.log file for error details."
    )
    ZR_CONFIG_ERROR = Code(
        824,
        "Failed to process and initiate terminal-device config on zr cfp",
        "service",
        error,
        "Verify the service payload for any missing dependencies and data validity checks."
        " Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    ZR_FP_CONFIG_ERROR = Code(
        825,
        "Service config-apply state callback has failed",
        "service",
        fatal,
        "Verify the service payload for any missing dependencies and data validity checks."
        " Check the ncs-python-vm-cisco-zr-cfp.log file for error details."
    )
    OPTICAL_INIT_ERROR = Code(
        826,
        "Failed to handle init create on optical stacked-service",
        "service",
        error,
        "Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    INTERNAL_DATA_READ_ERR = Code(
        850,
        "Failed to read internal operational data.",
        "service",
        error,
        "Incorrect data format in the tables created or maintained by RON-ml service."
        " Check the ncs-python-vm-cisco-ron-cfp.log file for error details."
    )
    # 870 - 879 : Custom Template Errors
    CUSTOM_TEMPLATE_ERROR = Code(
        870,
        "Failed to apply the custom template",
        "custom-action",
        error,
        "Verify the applied custom template name against the available"
        " custom templates in the system. Make sure the"
        " variables used in the applied template are populated."
    )

    OPTICAL_FAILUER = Code(
        990,
        "Optical configuration has failed for an unknown reason.",
        "service",
        error,
        " Check ncs-python-vm-cisco-optical-cfp.log for detailed error.",
    )

    @property
    def cfp_code(self):
        return "RON-" + str(self.value.code)


def get_status_by_code(code):
    for status_code in StatusCodes:
        if status_code.value[0] == int(code):
            return status_code
    return None
