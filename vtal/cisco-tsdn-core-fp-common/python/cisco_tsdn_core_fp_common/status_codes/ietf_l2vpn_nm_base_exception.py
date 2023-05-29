from core_fp_common.cfp_exception import CoreFunctionPackException
from .ietf_l2vpn_nm_status_codes import StatusCodes


class CfpCommonException(CoreFunctionPackException):
    def __init__(self, log, code, exp_msg=None, child_obj=None):
        if child_obj:
            super(CfpCommonException, self).__init__(
                log,
                child_obj,
                StatusCodes.getNativeId(code),
                "TSDN-IETF-L2NM",
                "IETF-L2NM",
                exp_msg,
            )
        else:
            super(CfpCommonException, self).__init__(
                log,
                self,
                StatusCodes.getNativeId(code),
                "TSDN-IETF-L2NM",
                "IETF-L2NM",
                exp_msg,
            )

    def save_to_plan(self, plan, component):
        # First append status code to component
        plan.component[component].status_code = self.statusCode.code
        # Build and save status code + context to plan
        status_code = plan.status_code_detail.create(*component)
        status_code.code = self.statusCode.code
        # Build context
        for context in self.context:
            ctx = status_code.context.create(context.ctx)
            ctx.context_msg = context.msg
        status_code.severity = self.statusCode.severity
        status_code.recommended_action = self.statusCode.recommendedActions
        status_code.impacted_device = component[1]
        return self.child


# Status Codes 400-499 : User Errors
class UserErrorException(CfpCommonException):
    def __init__(self, log, code, exp_msg=None):
        super(UserErrorException, self).__init__(log, code, exp_msg, self)


# Status Codes 500-599 : Custom Action Errors
class CustomActionException(CfpCommonException):
    def __init__(self, log, code, exp_msg=None):
        super(CustomActionException, self).__init__(log, code, exp_msg, self)


# Status Codes 700-799 : Resource Allocation Errors
class ResourceAllocationException(CfpCommonException):
    def __init__(self, log, code, exp_msg=None):
        super(ResourceAllocationException, self).__init__(log, code, exp_msg, self)


# Returns correct status code exception subclass for StatusCode status_code
def get_status_code_class(status_code):
    code = status_code.getNativeId()
    status_class = CfpCommonException
    if 400 <= code <= 499:
        status_class = UserErrorException
    elif 500 <= code <= 599:
        status_class = CustomActionException
    elif 700 <= code <= 799:
        status_class = ResourceAllocationException
    return status_class