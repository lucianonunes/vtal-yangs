'''
This file need to be copied by the core function pack to it's python package.

@author: abhatta
'''
import ncs.maapi as maapi
import ncs.maagic as maagic
import traceback
import sys


class CoreFunctionPackException(Exception):
    '''
    The CoreFunctionPackException class is the superclass of all exceptions core
    function pack can throw. This imposes that all CFP exceptions are checked
    exceptions. Besides capturing basic information like message, It holds a list
    of context information which allows to enrich the exception by recording relevant
    context/process state data as it moves up along the call stack.
    Any NSO exception should not be propagated, instead it should be wrapped with
    CoreFunctionPackException or its subclasses. In other words the core function
    packs should throw CoreFunctionPackException or its subclasses only.
    CoreFunctionPackException doesn't allow disabling suppressed exceptions.

    By convention, a cfp level common exception subclass of CoreFunctionPackException is defined
        def __init__(self, log, code, exp_msg=None, child_obj=None):
            if child_obj:
                super(CfpCommonException, self).__init__(log, child_obj,
                                                StatusCodes.getNativeId(code), 'SAE','sae', exp_msg)
            else:
                super(CfpCommonException, self).__init__(log, self,
                                            StatusCodes.getNativeId(code), 'SAE', 'sae', exp_msg)

    Other exception classes can inherit this common exception class.
        class ValidationException(CfpCommonException):
            def __init__(self, log, code, exp_msg):
                super(ValidationException, self).__init__(log, code, exp_msg, self)

    Usage example:
        try:
            # do something..
        except Exception as e:
            e = ValidationException(self.log, StatusCodes.FAILED_TO_READ_XYZ, str(e))
                .set_context("Interface creation", "Failed to create WAN interface")
                .add_state("Interface", InterfaceId).add_state("Device", device).finish()
            self.log.error(e)
            raise e
    '''
    def __init__(self, log, child_obj, code, code_prefix, cfp_name, exp_msg=None):
        self.log = log
        self.child = child_obj
        self.value = exp_msg
        self.statusCode = get_status_code(self, code_prefix, cfp_name, code)
        self.context = []

    def set_context(self, ctx_name, msg=None):
        ctx = Context(self.child, ctx_name, msg)
        self.context.append(ctx)
        return ctx

    def get_status_code(self):
        return self.statusCode

    def __str__(self):
        if self.value:
            cfpe_str = self.value + "\n"
        else:
            cfpe_str = ''
        cfpe_str += str(self.statusCode)
        for ctx in reversed(self.context):
            cfpe_str += str(ctx)

        # March 19, 2021 : We cannot include traceback in StatusCode as it can cause string repr to
        #                  exceed alarm error-info string limit, causing alarm to not be raised.
        #                  Instead, we will log the exception with stack trace.
        tb_str = "\n"
        for tb in traceback.format_list(traceback.extract_tb(sys.exc_info()[2])):
            tb_str += tb.strip() + "\n"
        self.log.error(cfpe_str + tb_str)

        return cfpe_str


class Context(object):
    def __init__(self, ret_obj, ctx, msg=None):
        self.ret = ret_obj
        self.ctx = ctx
        self.msg = msg
        self.state = dict()

    def add_state(self, key, value):
        self.state[key] = value
        return self

    def finish(self):
        return self.ret

    def __str__(self):
        ctx_str = "Context [name = " + self.ctx
        if self.msg:
            ctx_str += ", message = " + self.msg
        if len(self.state) > 0:
            ctx_str += "\n state = " + str(self.state)
        ctx_str += "]\n"
        return ctx_str


class StatusCode(object):
    def __init__(self, code_prefix, code):
        self.code = (code_prefix + "-" + str(code)).upper()
        self.reason = None
        self.category = None
        self.severity = None
        self.recommendedActions = None

    def __str__(self):
        return f"STATUS_CODE: {self.code}\nREASON: {self.reason}\nCATEGORY: {self.category}"\
               f"\nSEVERITY: {self.severity}\n"


def get_status_code(self, code_prefix, cfp_name, code):
    status_code = StatusCode(code_prefix, code)
    statusCodePath = '/cfp-common-status-codes:status-codes/core-function-pack{' + cfp_name + \
                     '}/status-code{' + str(code) + '}'
    try:
        with maapi.single_read_trans('admin', 'system') as th:
            statusNode = maagic.get_node(th, statusCodePath)
            status_code.reason = statusNode.reason
            status_code.category = statusNode.category
            sever_ident = statusNode.severity
            status_code.severity = sever_ident[sever_ident.index(':') + 1:]
            if statusNode.recommended_actions:
                status_code.recommendedActions = statusNode.recommended_actions
    except Exception as e:
        self.log.error(e)
    return status_code
