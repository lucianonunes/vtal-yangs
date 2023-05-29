import ncs
import traceback
import time
from cisco_tsdn_core_fp_common.utils import is_netsim_device, get_xe_interface_mapping


class IosXE:
    def conf_l2vpn(self, site, local):
        self.log.info(f"Configuring Flat L2VPN/SR on IosXE for service {site.pe}")

        if self.service.service_type == "p2p":
            # Hash pw interface id
            pw_if_id = int(self.service.flat_L2vpn_p2p.pw_id) % 231072

            l2vpn_vars = ncs.template.Variables()
            l2vpn_vars.add("LOCAL_NODE", "true" if local is True else "false")
            l2vpn_vars.add("PW_IF_ID", str(pw_if_id))
            l2vpn_template = ncs.template.Template(site)
            l2vpn_template.apply("cisco-flat-L2vpn-fp-xe-cli-p2p-template", l2vpn_vars)

    def conf_l2vpn_rp(self, rr_parent_route_policy):
        self.log.info("Route Policy is currently not supported on XE, passing")

    def validate_parent_policy_exists(self, root, device, parent_policy):
        self.log.info("Route Policy is currently not supported on XE, passing")
        return True

    def get_original_policy(self, root, device, parent_policy):
        self.log.info("Route Policy is currently not supported on XE, passing")
        return ""

    def check_if_interface_exists(self, root, site, service_interface_name, service_interface_id):
        service_interface_name = get_xe_interface_mapping(service_interface_name)
        service_interface = service_interface_name + service_interface_id
        self.log.info(f"Checking {service_interface} on IosXE device {site.pe}")

        device_interface = root.devices.device[site.pe].config.interface
        device_interfaces = getattr(device_interface,
                                    service_interface_name.replace("-", "_"), None)

        if device_interfaces:
            for interface in device_interfaces:
                if service_interface == str(interface) + str(interface.name):
                    return True

        return False

    def get_interface_shutdown_template(self):
        return "cisco-flat-L2vpn-fp-no-shutdown-iosxe-cli"

    def l2vpn_self_test(self, root, service, device, xc_group, xc_name):
        self.log.info(f"Running L2vpn self test on IosXE device {device}")
        # If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", "netsim")
        else:
            # ping vrf src -> dest
            self_test_command = "show l2vpn service name " + xc_name + " detail"

            # Execute self test
            action = root.devices.device[device].live_status.ios_stats__exec.any
            input = action.get_input()
            input.args.create(self_test_command)
            return self.run_self_test(action, input, service, device)

    def run_self_test(self, action, input, service, device):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(input)
                self.log.info(f"L2vpn Self Test result: {output.result}")

                output_vpws = ""
                output_pw = ""
                for item in output.result.split("\n"):
                    if "VPWS name:" in item:
                        output_vpws = item.strip()
                    if "pw1" in item:
                        output_pw = item.strip()
                self.log.info(f"L2vpn Self Test result output_vpws: {output_vpws} "
                              f"output_pw: {output_pw}")

                if "UPUP" in output_pw.replace(" ", "") and \
                        "State:UP" in output_vpws.replace(" ", ""):
                    return ("success", "success")
                else:
                    return ("failed", "State is down")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (max_retries - 1):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return ("failed", "Device ssh session being used by another transaction. "
                            + "Retry self-test with following command: "
                            + "'request flat-L2vpn " + service.name + " action self-test'")
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, "
                                  f"will retry self test for: {service.name}")
                else:
                    traceback.print_exc()
                    return ("failed", str(e))
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
