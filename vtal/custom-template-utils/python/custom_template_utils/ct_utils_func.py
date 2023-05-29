import ncs.template as template
from . import utils


def process_apply_template(self, root, service, ct_data):
    self.log.info('Custom-template type: ', ct_data.type)
    if ct_data.type == utils.FEATURE_TEMPLATE:
        _apply_feature_template(self, ct_data)
    elif ct_data.type == utils.DEVICE_TEMPLATE:
        _apply_device_template(self, ct_data)
    else:
        raise Exception(f'Type of custom-template: {ct_data.template_name} '
                        'could not be determined.')


def _apply_feature_template(self, ct_data):
    template_vars = _get_config_template_variable(self, ct_data)
    template_obj = template.Template(ct_data.service)
    template_obj.apply(ct_data.template_name, template_vars)
    self.log.info('feature-template is applied.')
    self.log.info('====================================================================')


def _apply_device_template(self, ct_data):
    device_node = ct_data.root.devices.device[ct_data.device_name]
    input = _get_device_template_input(self, device_node, ct_data.template_name, ct_data.variables)
    output = device_node.apply_template(input)
    _process_result(self, output, ct_data.template_name, ct_data.device_name)


def _get_config_template_variable(self, ct_data):
    template_vars = template.Variables()
    for var_name in ct_data.variables:
        template_vars.add(var_name, ct_data.variables[var_name])
    if ct_data.dev_var_name:
        template_vars.add(ct_data.dev_var_name, ct_data.device_name)
    else:
        template_vars.add('CONTROLLER_NAME', ct_data.device_name)
        template_vars.add('DEVICE_NAME', ct_data.device_name)
        template_vars.add('DEVICE', ct_data.device_name)
    self.log.info('variables: ', template_vars)
    return template_vars


def _get_device_template_input(self, device_node, template_name, variables):
    input = device_node.apply_template.get_input()
    input.suppress_positive_result.create()
    input.template_name = template_name
    self.log.info('variables: ', variables)
    for var_name in variables:
        var_node = input.variable.create(str(var_name))
        var_node.value = f"'{variables[var_name]}'"
    return input


def _process_result(self, output, template_name, device_name):
    if len(output.apply_template_result) > 0:
        output_info = output.apply_template_result[device_name].info
        raise Exception(f'Error applying device-template: {template_name}, '
                        f'on device: {device_name} : {output_info}')
    else:
        self.log.info('device-template is applied.')
    self.log.info('====================================================================')
