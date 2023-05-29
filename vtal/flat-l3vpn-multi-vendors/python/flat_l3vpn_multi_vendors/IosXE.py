import ncs
import time
import traceback
import re
from .network import getIpAddress, getNetMask
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.utils import is_netsim_device, get_xe_interface_mapping


class IosXE:
    def conf_l3vpn(self, endpoint):
        self.log.info(f"Configuring Flat L3VPN/SR on IosXE device {endpoint.access_pe}")
        if endpoint.if_type == "BVI":
            raise UserErrorException(
                self.log, StatusCodes.INTERFACE_TYPE_NOT_SUPPORTED
            ).set_context(
                "Status Code",
                "BVI is not supported for XE device,"
                + "please use sub-interface & BDI",
            ).add_state(
                "Device", endpoint.access_pe
            ).finish()
        l3vpn_vars = ncs.template.Variables()
        l3vpn_vars.add("IPV4_ADDR", "")
        l3vpn_vars.add("IPV4_ADDR_IP", "")
        l3vpn_vars.add("IPV4_MASK", "")

        if endpoint.pe_ip_addr is not None:
            l3vpn_vars.add('IPV4_ADDR', getIpAddress(endpoint.pe_ip_addr))
            l3vpn_vars.add('IPV4_MASK', getNetMask(endpoint.pe_ip_addr))
            l3vpn_vars.add('IPV4_ADDR_IP', endpoint.pe_ip_addr)

        as_no = ""
        if endpoint.as_no is not None:
            as_no = endpoint.as_no
        elif endpoint.as_no_from_device.exists():
            as_no = self.get_bgp_as_from_device(self.root, endpoint.access_pe)
        l3vpn_vars.add('AS_NO', as_no)

        l3vpn_template = ncs.template.Template(endpoint)
        l3vpn_template.apply("cisco-flat-L3vpn-fp-cli-xe-template", l3vpn_vars)

    def check_if_interface_exists(self, root, endpoint,
                                  service_interface_name, service_interface_id):
        if service_interface_name == "Bundle-Ether":
            service_interface_name = "Port-channel"

        service_interface_name = get_xe_interface_mapping(service_interface_name)

        service_interface = service_interface_name + service_interface_id
        self.log.info(f"Checking {service_interface} on IosXE device {endpoint.access_pe}")

        device_interface = root.devices.device[endpoint.access_pe].config.interface
        device_interfaces = getattr(device_interface,
                                    service_interface_name.replace("-", "_"), None)

        if device_interfaces:
            for interface in device_interfaces:
                if service_interface == str(interface) + str(interface.name):
                    return True

        return False

    def l3vpn_self_test(self, root, service, vrf_name, device, src, dst):
        self.log.info(f"Running L3vpn self test on IosXE device {device}")
        # If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", "netsim")
        else:
            # ping vrf src -> dest
            self_test_command = "ping vrf " + vrf_name + " " + dst + " source " + src

            # Execute self test
            action = root.devices.device[device].live_status.ios_stats__exec.any
            input = action.get_input()
            input.args.create(self_test_command)
            return self.run_self_test(action, input, service)

        return ("failed", "Failed to run self-test")

    def get_interface_shutdown_template(self):
        return "cisco-flat-L3vpn-fp-no-shutdown-iosxe-cli"

    def run_self_test(self, action, input, service):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(input)
                self.log.info(f"L3vpn Self Test result: {output.result}")
                # Return self test result - above 60% is success
                success_percent = ["3/5", "4/5", "5/5"]
                if any(sp in output.result for sp in success_percent):
                    return ("success", "Success" + re.search("Success(.*),",
                                                             output.result).group(1))
                else:
                    return ("failed", f"Unexpected ping result: {output.result}")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (max_retries - 1):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return (
                        "failed", "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request flat-L3vpn " + service.name + " action self-test'",
                    )
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, "
                                  f"will retry self test for: {service.name}")
                else:
                    traceback.print_exc()
                    return ("failed", str(e))
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def is_vrf_address_family_active(self, root, device, bgp_as_no, vrf_name):
        return False

    def get_vrf_rd(self, root, device, vrf_name):
        return root.devices.device[device].config.ios__vrf.definition[vrf_name].rd

    def get_bgp_vrf_rd(self, root, device, bgp_as_no, vrf_name):
        return IosXE.get_vrf_rd(self, root, device, vrf_name)

    def get_bgp_as_from_device(self, root, device):
        bgp_path = root.devices.device[device].config.ios__router.bgp
        for bgp in bgp_path:
            return bgp.as_no

        return ""

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
