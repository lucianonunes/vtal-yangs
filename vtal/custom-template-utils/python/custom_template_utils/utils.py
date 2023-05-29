import socket
import _ncs
import _ncs.maapi as _maapi

FEATURE_TEMPLATE = 'FEATURE'
DEVICE_TEMPLATE = 'DEVICE'


def validate_input(root, service, device_name, ct_node=None, template_name=None):
    err_msg = ''
    ret_val = False
    if root is None or root == '' or root._path != '':
        err_msg = err_msg + 'Invalid root node.'
    elif root.apply_custom_template:
        ret_val = True
        err_msg = err_msg + _validate_device(root, device_name)
        if service is None or service == '':
            err_msg = err_msg + ' Invalid service node.'
        if ct_node and (ct_node == '' or 'custom-template' not in ct_node._path):
            err_msg = err_msg + ' Invalid custom-template node.'
        if template_name and template_name == '':
            err_msg = err_msg + ' Invalid custom template name.'

    if len(err_msg) > 0:
        raise ValueError(err_msg)
    return ret_val


def get_input_vars(var_list):
    input_vars = {}
    if var_list:
        for var in var_list:
            input_vars.update({var.name: var.value})
    return input_vars


def populate_ct_vars(variables, ct_vars, input_vars):
    if input_vars:
        for var_name in input_vars:
            if var_name in ct_vars and input_vars[var_name]:
                variables.update({var_name: input_vars[var_name]})


def get_ct_type(root, template_name):
    template_type = None
    try:
        s = socket.socket()
        _maapi.connect(s, ip='127.0.0.1', port=_ncs.NCS_PORT)
        if template_name in _maapi.get_templates(s):
            template_type = FEATURE_TEMPLATE
    finally:
        s.close()
    if template_type is None:
        if root.devices.template.exists(template_name):
            template_type = DEVICE_TEMPLATE
    if template_type is None:
        raise ValueError(template_name + ' is not loaded into NSO.')
    return template_type


def _validate_device(root, device_name):
    err_msg = ''
    if device_name is None or device_name == '':
        err_msg = ' Invalid device name.'
    elif not root.devices.device.exists(device_name):
        err_msg = ' ' + device_name + ' does not exists in nso device tree.'
    return err_msg
