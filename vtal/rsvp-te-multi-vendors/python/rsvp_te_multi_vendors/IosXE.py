import ncs
import traceback
import time
from cisco_tsdn_core_fp_common.utils import is_netsim_device


class IosXE:
    def get_interface_loopback(self, root, service):
        self.log.info(f"Getting Loopback for service {service} "
                      f"from IOSXE device {service.head_end}")

        device_address = service.name.split("-")[-2]
        device = root.devices.device[service.head_end]

        for loopback in device.config.interface.Loopback:
            if loopback.ip.address.primary.address == device_address:
                return loopback.name

        return None

    def conf_rsvp_te_tunnel_p2p(self, service, loopback):
        self.log.info(f"Configuring RSVP TE Tunnel on IOSXE device {service.head_end}")

        #  Applying connfig
        vars = ncs.template.Variables()
        vars.add("LOOPBACK", loopback)
        template = ncs.template.Template(service)
        template.apply("cisco-rsvp-te-fp-xe-cli-template", vars)

    def ietf_te_self_test(self, uinfo, root, rsvp_te_service):
        device = rsvp_te_service.head_end
        self.log.info(f"Running IETF-TE self test on IOSXE device {device}")
        #  If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", None)
        else:
            # RSVP Service name format: <Tunnel Name>-<Source IP>-internal
            tunnel_name = rsvp_te_service.name.rsplit("-", 2)[0]

            #  check status of RSVP TE config on device
            self_test_command = "show mpls traffic-eng tunnels name " + tunnel_name

            #  Execute self test
            action = root.devices.device[device].live_status.ios_stats__exec.any
            action_input = action.get_input()
            action_input.args.create(self_test_command)
            return self.run_self_test(action, action_input, tunnel_name,
                                      device, rsvp_te_service)

    def run_self_test(self, action, action_input, tunnel_name, device, service):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(action_input)
                # The output for this action comes with different number of spaces
                # between Admin/ Oper and status so removing all spaces before comparing.
                result = output.result.replace(" ", "")
                self.log.info(f"Self Test result: {output.result}")
                #  Return self test result
                if "Admin:up" in result and "Oper:up" in result:
                    return ("success", None)
                elif "Admin:down" in result:
                    return ("failed", "Admin state is down")
                elif "Oper:down" in result:
                    return ("failed", "Operational state is down")
                elif "Admin:down" in result and "Oper:down" in result:
                    return ("failed", "Both Admin & Operational states are down")
                else:
                    return ("failed", "No rsvp-te status found on device")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (max_retries - 1):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return (
                        "failed", "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request ietf-te te tunnels tunnel "
                        + tunnel_name + " action self-test'")
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, will "
                                  f"retry self test for: {service.name}")
                else:
                    traceback.print_exc()
                    raise e
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
