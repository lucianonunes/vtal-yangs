from core_fp_common.cfp_exception import CoreFunctionPackException


class RonMLException(CoreFunctionPackException):
    def __init__(self, log, code, exp_msg=None, child_obj=None):
        if child_obj:
            super().__init__(log, child_obj, code.value.code, "RON", "RON", exp_msg)
        else:
            super().__init__(log, self, code.value.code, "RON", "RON", exp_msg)

    def save_to_plan(self, plan, component, device=None):
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
        # Set affected device
        if device:
            status_code.impacted_device = device
        else:
            status_code.impacted_device = status_code.name
        return self.child


## Status Codes 300-399 : Device Config Errors
class DeviceConfigException(RonMLException):
    def __init__(self, log, code, exp_msg=None):
        super().__init__(log, code, exp_msg, self)


## Status Codes 400-499 : User Errors
class UserError(RonMLException):
    def __init__(self, log, code, exp_msg=None):
        super().__init__(log, code, exp_msg, self)


## Status Codes 500-599 : Custom Action Errors
class CustomActionException(RonMLException):
    def __init__(self, log, code, exp_msg=None):
        super().__init__(log, code, exp_msg, self)


## Status Codes 700-799 : Resource Allocation Errors

## Status Codes 800-899 : Service Errors
class ServiceException(RonMLException):
    def __init__(self, log, code, exp_msg=None):
        super().__init__(log, code, exp_msg, self)


# Status Codes 870-879 : Custom Template Errors
class CustomTemplateException(RonMLException):
    def __init__(self, log, code, exp_msg=None):
        super().__init__(log, code, exp_msg, self)


# Status Codes 880-889 : Alarm Errors

# Returns correct status code exception subclass for StatusCode status_code
def get_status_code_class(status_code):
    code = status_code.code
    status_class = RonMLException
    if 300 <= code <= 399:
        status_class = DeviceConfigException
    elif 400 <= code <= 499:
        status_class = UserError
    elif 500 <= code <= 599:
        status_class = CustomActionException
    elif 870 <= code <= 879:
        status_class = CustomTemplateException
    elif 800 <= code <= 899:
        status_class = ServiceException
    return status_class
