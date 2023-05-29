import ncs


class NativeUMXR:
    def conf_tm_tc(self, device):
        self.log.info("Configuring TMTC on IOSXR NC device {}".format(device))
        tm_tc_vars = ncs.template.Variables()
        tm_tc_template = ncs.template.Template(device)
        tm_tc_template.apply('cisco-tm-tc-native-um-template', tm_tc_vars)

    def check_if_interface_exists(self, root, node, service_interface_name, service_interface_id):
        service_interface = service_interface_name + service_interface_id
        self.log.info("Checking {} on IOSXR NC device {}".format(service_interface, node.name))
        device_interfaces = root.devices.device[node.name].config. \
            Cisco_IOS_XR_ifmgr_cfg__interface_configurations.interface_configuration
        for interface in device_interfaces:
            if service_interface == interface.interface_name:
                return True
        return False

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
