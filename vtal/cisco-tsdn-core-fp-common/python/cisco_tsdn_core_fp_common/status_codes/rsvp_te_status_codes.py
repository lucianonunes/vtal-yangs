from enum import Enum


class Severity:
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    FATAL = "FATAL"


class StatusCodes(Enum):
    # (CODE, REASON, CATEGORY, SEVERITY, RECOMMENDED ACTIONS)

    # 100 - 199 : Informational
    # For all in-progress status updates on plan

    # 200 - 299 : Success
    # All success events in CFP.
    # 300 - 399 : Device Config Errors
    # All errors while pushing configs to devices/VNFs through templates in CFP.
    CONNECTION_FAILURE = (
        301,
        "Device unreachable",
        "device",
        Severity.ERROR,
        "Check device connectivity from NSO and perform recovery steps.",
    )
    DEVICE_OUT_OF_SYNC = (
        302,
        "Device out of sync",
        "device",
        Severity.ERROR,
        "Check sync between device and NSO, and perform recovery steps.",
    )
    CONFIG_FAILURE = (
        303,
        "Config push failed",
        "device",
        Severity.ERROR,
        "Device configuration rejected, fix the service payload "
        "and perform recovery steps.",
    )
    # 400 - 499 : User Errors
    # All model validation errors though validate callback action in CFP.
    STATUS_CODE_NOT_LOADED = (
        400,
        "Status code mapping has not been loaded for function pack " "during install",
        "user",
        Severity.ERROR,
        "Bootstrap status code mapping",
    )
    VALUE_NOT_SUPPORTED = (
        404,
        "The value for the input element is not supported",
        "validation",
        Severity.ERROR,
        "Verify that input element's value is suppoprted in the payload",
    )
    DYNAMIC_CLASS_NOT_FOUND = (
        407,
        "Dynamic class for device not found",
        "user",
        Severity.ERROR,
        "Load correct classpath for the ned-id in CFP dynamic device mapping list",
    )
    NED_NOT_SUPPORTED = (
        408,
        "Router NED not supported",
        "user",
        Severity.ERROR,
        "Ensure dynamic device mapping is set for the ned-id",
    )
    DYNAMIC_METHOD_NOT_FOUND = (
        409,
        "Dynamic class method for device not found",
        "user",
        Severity.ERROR,
        "Ensure dynamic method is implemented in multi-vendor implementation",
    )

    # 500 - 599 : Custom Action Errors
    # All errors during custom request actions in CFP.
    SELF_TEST_ERROR = (
        501,
        "Self test failed",
        "custom-action",
        Severity.ERROR
    )
    SELF_TEST_STATUS_ERROR = (
        502,
        "Unsupported status returned for self test",
        "custom-action",
        Severity.ERROR,
    )
    CLEANUP_ERROR = (
        503,
        "Cleanup failed",
        "custom-action",
        Severity.ERROR
    )
    RECOVERY_ERROR = (
        504,
        "Service Recovery failed",
        "custom-action",
        Severity.ERROR
    )

    # 600 - 699 : Device/VNF On-boarding Errors
    # All errors during VNF Manager and VNF deployment and on-boarding through NFVO, device
    # on-boarding through PnP etc..

    # 700 - 799 : Resource Allocation Errors
    # All errors during ip and id allocations through resource manager (RM) and placement errors
    # through resource orchestration (RO) in CFP.

    # 800 - 899 : Service Errors
    # All other CFP errors except the above 6 which may include, errors in RFM, Subscribers,
    # Utility methods, CDB, Plan etc.. CFP can sub divide the range in service errors range.
    # General Service Status Codes
    # 850-859 : CDB OPER ERRORS
    # 860-869 : CDB CONFIG ERRORS

    # 870 - 879 : Custom Template Errors
    CUSTOM_TEMPLATE_ERROR = (
        870,
        "Custom Template apply failed",
        "custom-action",
        Severity.ERROR,
    )

    # 900 - inf : Extension Errors
    # All errors in extension packages if any in CFP.

    def getNativeId(self):
        return self.value[0]


def get_status_by_code(code):
    for status_code in StatusCodes:
        if status_code.value[0] == int(code):
            return status_code
    return None
