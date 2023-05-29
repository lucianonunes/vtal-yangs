import ncs

class IosXR():
    def conf_tm_tc(self, node):
        self.log.info("Configuring TMTC on IOSXR CLI device {}".format(node.name))
        tm_tc_vars = ncs.template.Variables()
        tm_tc_template = ncs.template.Template(node)
        tm_tc_template.apply('cisco-tm-tc-fp-cli-template', tm_tc_vars)

    def check_if_interface_exists(self, root, device, service_interface_name, service_interface_id):
        service_interface = service_interface_name + service_interface_id
        self.log.info('Checking {} on IOSXR CLI device {}'.format(service_interface, device.name))

        device_interface = root.devices.device[device.name].config.cisco_ios_xr__interface
        ## Check for all supported interface types:
        if service_interface_name == "GigabitEthernet":
            for interface in device_interface.GigabitEthernet:
                if service_interface == ("GigabitEthernet" + interface.id):
                    return True
        elif service_interface_name == "TenGigE":
            for interface in device_interface.TenGigE:
                if service_interface == ("TenGigE" + interface.id):
                    return True
        elif service_interface_name == "FortyGigE":
            for interface in device_interface.FortyGigE:
                if service_interface == ("FortyGigE" + interface.id):
                    return True
        elif service_interface_name == "HundredGigE":
            for interface in device_interface.HundredGigE:
                if service_interface == ("HundredGigE" + interface.id):
                    return True
        elif service_interface_name == "FiftyGigE":
            for interface in device_interface.FiftyGigE:
                if service_interface == ("FiftyGigE" + interface.id):
                    return True
        elif service_interface_name == "TwoHundredGigE":
            for interface in device_interface.TwoHundredGigE:
                if service_interface == ("TwoHundredGigE" + interface.id):
                    return True
        elif service_interface_name == "Bundle-Ether":
            for interface in device_interface.Bundle_Ether:
                if service_interface == ("Bundle-Ether" + interface.id):
                    return True
        elif service_interface_name == "FourHundredGigE":
            for interface in device_interface.FourHundredGigE:
                if service_interface == ("FourHundredGigE" + interface.id):
                    return True
        elif service_interface_name == "TwentyFiveGigE":
            for interface in device_interface.TwentyFiveGigE:
                if service_interface == ("TwentyFiveGigE" + interface.id):
                    return True

        return False

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
