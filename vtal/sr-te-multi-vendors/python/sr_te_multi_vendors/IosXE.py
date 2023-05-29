import ncs
import time
import traceback
from cisco_tsdn_core_fp_common.utils import is_netsim_device
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes


class IosXE:
    @staticmethod
    def template_sr_segment_list(service, device, name, index, label, ipv4):
        sr_segment_list_vars = ncs.template.Variables()
        sr_segment_list_template = ncs.template.Template(service)
        sr_segment_list_vars.add("device", device)
        sr_segment_list_vars.add("name", name)
        sr_segment_list_vars.add("index", index)
        sr_segment_list_vars.add("label", label)
        sr_segment_list_vars.add("ipv4", ipv4)
        sr_segment_list_template.apply(
            "cisco-sr-te-cfp-cli-sr-xe-segment-list", sr_segment_list_vars
        )

    @staticmethod
    def template_sr_policy(service, endpoint, device):
        sr_policy_vars = ncs.template.Variables()
        sr_policy_vars.add("ENDPOINT", endpoint)
        sr_policy_vars.add("HEAD_END", device)
        sr_policy_template = ncs.template.Template(service)
        sr_policy_template.apply(
            "cisco-sr-te-cfp-cli-sr-xe-non-odn-policy", sr_policy_vars
        )

    @staticmethod
    def template_sr_policy_template_new(service, device):
        sr_policy_vars = ncs.template.Variables()
        sr_policy_template = ncs.template.Template(service)
        sr_policy_vars.add("HEAD_END", device)
        sr_policy_template.apply("cisco-sr-te-cfp-cli-sr-xe-odn-policy", sr_policy_vars)

    def conf_segment_list(self, device):
        self.log.info(f"Configuring SegmentLIST on IosXE device {device}")
        for path in self.service.path:
            # Check if an explicit path needs to be configured on the target Device
            if path.explicit.exists():
                for sid_list in path.explicit.sid_list:
                    for sid in sid_list.sid:
                        IosXE.template_sr_segment_list(
                            service=self.service, device=device,
                            name=sid_list.name, index=sid.index,
                            label=sid.mpls.label if sid.type == "mpls" else "",
                            ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
                        )

    def conf_sr_template(self, device):
        self.log.info(f"Configuring SR ODN policy template on IosXE device {device}")
        IosXE.template_sr_policy_template_new(self.service, device)

    def conf_sr_policy(self, device):
        self.log.info(f"Configuring SR policy on IosXE device {device}")
        endpoint = self.service.tail_end
        IosXE.template_sr_policy(service=self.service, endpoint=endpoint, device=device)

    def get_max_sid_depth(self, root, device):
        self.log.info(f"Getting max-sid-depth on IosXE device {device}")
        return None

    def sr_policy_self_test(self, uinfo, root, service, device):
        self.log.info(f"Running SR Policy self test on IosXE device {device}")
        #  If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", None)
        else:
            #  check status of policy config on device
            self_test_command = "show segment-routing traffic-eng policy name " + str(service.name)

            #  Execute self test
            action = root.devices.device[device].live_status.ios_stats__exec.any
            input = action.get_input()
            input.args.create(self_test_command)
            return self.run_self_test(action, input, service, device)

    def validate_srv6_te(self, root, service, device):
        raise UserErrorException(
            self.log, StatusCodes.SRV6_NOT_SUPPORTED
        ).set_context(
            "SRv6 validation",
            "SRv6-TE is not supported on XE devices",
        ).add_state(
            "Service", service.name
        ).add_state(
            "Device", device
        ).finish()

    def run_self_test(self, action, input, service, device):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(input)
                self.log.info(f"Self Test result: {output.result}")
                #  Return self test result
                if "Admin: up" in output.result and "Operational: up" in output.result:
                    return ("success", None)
                elif "Admin: down" in output.result:
                    return ("failed", "Admin state is down")
                elif "Operational: down" in output.result:
                    return ("failed", "Operational state is down")
                elif ("Admin: down" in output.result and "Operational: down" in output.result):
                    return ("failed", "Both Admin & Operational states are down")
                else:
                    return ("failed", "No policy status found on device")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (max_retries - 1):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return (
                        "failed", "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request sr-te cisco-sr-te-cfp-sr-policies:policies "
                        + "policy " + service.name + " head-end " + device + " action self-test'")
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, "
                                  f"will retry self test for: {service.name}")
                else:
                    traceback.print_exc()
                    raise e
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
