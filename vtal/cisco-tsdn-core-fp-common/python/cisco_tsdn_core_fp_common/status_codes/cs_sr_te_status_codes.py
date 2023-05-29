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

    # 400 - 499 : User Errors
    # All model validation errors though validate callback action in CFP.
    STATUS_CODE_NOT_LOADED = (
        400,
        "Status code mapping has not been loaded for function pack " "during install",
        "user",
        Severity.ERROR,
        "Bootstrap status code mapping",
    )
    MISSING_INPUT = (
        407,
        "Missing input",
        "validation",
        Severity.ERROR,
        "Verify that missing input elements are provided in the payload",
    )

    # 500 - 599 : Custom Action Errors
    # All errors during custom request actions in CFP.
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
    ALLOCATION_FAILED = (
        701,
        "Allocation has failed",
        "resource",
        Severity.ERROR
    )
    ALLOCATION_REQUEST_FAILED = (
        702,
        "Allocation request failed",
        "resource",
        Severity.ERROR,
    )

    # 800 - 899 : Service Errors
    # All other CFP errors except the above 6 which may include, errors in RFM, Subscribers,
    # Utility methods, CDB, Plan etc.. CFP can sub divide the range in service errors range.
    # General Service Status Codes
    GENERATED_CONFIG_CONFLICT = (
        801,
        "Service generated name conflicts with existing config",
        "service",
        Severity.ERROR
    )

    # 850-859 : CDB OPER ERRORS

    # 860-869 : CDB CONFIG ERRORS

    # 870 - 879 : Custom Template Errors
    CUSTOM_TEMPLATE_ERROR = (
        870,
        "Custom Template apply failed",
        "custom-action",
        Severity.ERROR,
    )

    # 880 - 889 : Alarm Errors

    # 900 - inf : Extension Errors
    # All errors in extension packages if any in CFP.

    def getNativeId(self):
        return self.value[0]


def get_status_by_code(code):
    for status_code in StatusCodes:
        if status_code.value[0] == int(code):
            return status_code
    return None
