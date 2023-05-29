from . import utils
from . import ct_utils


def apply_custom_template(self, root, service, device_name, ct_node, extra_vars=None):
    '''
    Apply a single custom-template represented by ct-node on a device.
        self: ServiceCallbacks instance
        root: root node
        service: service node and the custom-template will be applied on this context.
        device-name: Name of the device on which the custom-template will be applied.
        ct_node: cutom_template node
        extra_vars: Dictionary {variable_name:variable_value} of extra variables.
            These variables will be over-written by the custom-template
            variables if variable name matches.
    '''
    if utils.validate_input(root, service, device_name, ct_node=ct_node):
        self.log.info('====================================================================')
        self.log.info('custom-template is triggered for ', ct_node._path)
        ct_utils.process_ct_node(self, root, service, device_name, ct_node, extra_vars)
        return True
    else:
        self.log.info('apply-custom-template flag is false.')
        return False


def apply_custom_templates(self, root, service, device_name, ct_list_node, extra_vars=None):
    '''
    Apply multiple custom-templates represented by ct_list_node on a device.
        self: ServiceCallbacks instance
        root: root node
        service: service node and all the custom-templates will be applied on this context.
        device-name: Name of the device on which all the custom-templates will be applied.
        ct_list_node: cutom_template list node
        extra_vars: Dictionary {variable_name:variable_value} of extra variables.
            These variables will be over-written by the custom-template
            variables if variable name matches.
    '''
    if utils.validate_input(root, service, device_name, ct_node=ct_list_node):
        self.log.info('====================================================================')
        self.log.info('custom-templates are triggered for ', ct_list_node._path)
        for ct_node in ct_list_node:
            ct_utils.process_ct_node(self, root, service, device_name, ct_node, extra_vars)
        return True
    else:
        self.log.info('apply-custom-template flag is false.')
        return False


def apply_template(self, root, service, device_name, template_name, variables=None):
    '''
    Apply any template on a device once with provided variables.
        self: ServiceCallbacks instance
        root: root node
        service: service node and the custom-template will be applied on this context.
        device-name: Name of the device on which the custom-template will be applied.
        template_name: Name of the cutom_template which will be applied.
        variables: Dictionary {variable_name:variable_value} of variables in the custom-template.
    '''
    if utils.validate_input(root, service, device_name, template_name=template_name):
        self.log.info('====================================================================')
        self.log.info('Applying custom-template: ', template_name, ' once on device: ', device_name)
        ct_utils.process_ct(self, root, service, device_name, template_name, variables)
        return True
    else:
        self.log.info('apply-custom-template flag is false.')
        return False
