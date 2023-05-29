from . import utils
from . import ct_utils_func as ctu_func


class CTData:
    def __init__(self, root, service, device_name, template_name):
        self.root = root
        self.service = service
        self.template_name = template_name
        self.type = self.__get_ct_type()
        self.ct_variables = self.__get_ct_variables()
        self.dev_var_name = self.__get_ct_device_var_name()
        self.device_name = device_name
        self.variables = {}

    def set_variables(self, variables):
        if variables:
            self.variables.update(variables)

    def __get_ct_type(self):
        try:
            if self.root.custom_template_info[self.template_name]:
                return str(self.root.custom_template_info[self.template_name].type)
        except Exception:
            return utils.get_ct_type(self.root, self.template_name)
        return None

    def __get_ct_variables(self):
        try:
            if self.root.custom_template_info[self.template_name]:
                ct_var = self.root.custom_template_info[self.template_name].variables
                if ct_var:
                    return set(ct_var)
                return set()
        except Exception:
            pass
        return None

    def __get_ct_device_var_name(self):
        try:
            if self.root.custom_template_info[self.template_name]:
                return str(self.root.custom_template_info[self.template_name].device_variable)
        except Exception:
            pass
        return None


def process_ct_node(self, root, service, device_name, ct_node, extra_vars):
    template_name = ct_node.name
    ct_data = CTData(root, service, device_name, template_name)
    self.log.info(f"Custom-template: {template_name}, "
                  f"has variables: {', '.join(ct_data.ct_variables)}")
    if ct_node.iteration:
        for iter in ct_node.iteration:
            self.log.info(f"Applying custom-template: {template_name} "
                          f"on device: {device_name} "
                          f"for iteration: {iter.number}")
            _process_variable(ct_data, extra_vars, input_vars=utils.get_input_vars(iter.variable),
                              itr=iter.number)
            ctu_func.process_apply_template(self, root, service, ct_data)
    elif ct_node.variable:
        self.log.info(f"Applying custom-template: {template_name} "
                      f"on device: {device_name} with input variables")
        _process_variable(ct_data, extra_vars, input_vars=utils.get_input_vars(ct_node.variable))
        ctu_func.process_apply_template(self, root, service, ct_data)
    else:
        self.log.info(f"Applying custom-template: {template_name} "
                      f"on device: {device_name} without input variables")
        _process_variable(ct_data, extra_vars)
        ctu_func.process_apply_template(self, root, service, ct_data)


def process_ct(self, root, service, device_name, template_name, variables):
    ct_data = CTData(root, service, device_name, template_name)
    if ct_data.ct_variables:
        self.log.info(f"Custom-template: {template_name} ,"
                      f" has variables: {', '.join(ct_data.ct_variables)}")
        _process_variable(ct_data, variables)
    else:
        ct_data.set_variables(variables)
    ctu_func.process_apply_template(self, root, service, ct_data)


def _process_variable(ct_data, extra_vars, input_vars=None, itr=None):
    ct_data.variables.clear()
    ct_var = set(ct_data.ct_variables)
    variables = {}
    utils.populate_ct_vars(variables, ct_var, extra_vars)
    utils.populate_ct_vars(variables, ct_var, input_vars)
    for var_name in variables.keys():
        ct_var.discard(var_name)
    if len(ct_var) > 0:
        err_msg = f"For custom-template: {ct_data.template_name}, "\
                  "following variable's value not provided"
        if itr:
            err_msg = err_msg + f" for iteration-{itr}"
        err_msg = err_msg + f": {', '.join(ct_var)}"
        #raise Exception(err_msg)
    ct_data.set_variables(variables)
