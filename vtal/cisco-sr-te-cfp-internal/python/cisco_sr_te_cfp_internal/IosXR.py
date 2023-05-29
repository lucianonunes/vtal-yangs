import ncs
import re
import time
import traceback
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.utils import is_netsim_device, get_device_ned_id,\
    get_device_platform_version

ipv4_address = re.compile(
    "^(?:(?:[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}"
    "(?:[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
)


class IosXR:
    @staticmethod
    def template_sr_segment_list(service, device, name, index, label, ipv4):
        sr_segment_list_vars = ncs.template.Variables()
        sr_segment_list_template = ncs.template.Template(service)
        sr_segment_list_vars.add("device", device)
        sr_segment_list_vars.add("name", name)
        sr_segment_list_vars.add("index", index)
        sr_segment_list_vars.add("label", label)
        sr_segment_list_vars.add("ipv4", ipv4)
        sr_segment_list_template.apply("cisco-sr-te-cfp-cli-sr-segment-list", sr_segment_list_vars)

    @staticmethod
    def template_sr_policy(service, endpoint, device):
        if ipv4_address.match(service.tail_end):
            ip_type = "ipv4"
        else:
            ip_type = "ipv6"
        sr_policy_vars = ncs.template.Variables()
        sr_policy_vars.add("ENDPOINT", endpoint)
        sr_policy_vars.add("IP_TYPE", ip_type)
        sr_policy_vars.add("HEAD_END", device)
        sr_policy_template = ncs.template.Template(service)
        sr_policy_template.apply("cisco-sr-te-cfp-cli-sr-non-odn-policy", sr_policy_vars)

    @staticmethod
    def template_sr_policy_template_new(service, device):
        sr_policy_vars = ncs.template.Variables()
        sr_policy_template = ncs.template.Template(service)
        sr_policy_vars.add("HEAD_END", device)
        sr_policy_template.apply("cisco-sr-te-cfp-cli-sr-odn-policy", sr_policy_vars)

    def conf_segment_list(self, device):
        self.log.info(f"Configuring SegmentLIST on IOSXR device {device}")
        # For non CS
        for path in self.service.path:
            # Check if an explicit path needs to be configured on the target Device
            if path.explicit.exists():
                for sid_list in path.explicit.sid_list:
                    for sid in sid_list.sid:
                        IosXR.template_sr_segment_list(
                            service=self.service, device=device,
                            name=sid_list.name, index=sid.index,
                            label=sid.mpls.label if sid.type == "mpls" else "",
                            ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
                        )

        # For CS - working/protect/restore
        # TODO: Circuit Style, ETA: TSDN 5.0
        # if self.service.circuit_style.exists():
        #     circuit_style = self.service.circuit_style
        #     # Working
        #     if circuit_style.working_path.explicit:
        #         explicit = circuit_style.working_path.explicit
        #         for sid in explicit.forward_sid:
        #             IosXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.forward_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )
        #         for sid in explicit.reverse_sid:
        #             IosXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.reverse_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )

        #     # Protect
        #     if circuit_style.protect_path.explicit:
        #         explicit = circuit_style.protect_path.explicit
        #         for sid in explicit.forward_sid:
        #             IosXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.forward_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )
        #         for sid in explicit.reverse_sid:
        #             IosXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.reverse_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )

        #     # Restore
        #     if circuit_style.restore_path.explicit:
        #         explicit = circuit_style.restore_path.explicit
        #         for sid in explicit.forward_sid:
        #             IosXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.forward_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )
        #         for sid in explicit.reverse_sid:
        #             IosXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.reverse_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )

    def check_path_constraints(self, device_version):
        # For Netsim Devices, device_version => cisco-iosxr-7.33.11
        # For Real Devices, device_version => 7.33.11
        # split("-")[-1] returns the correct version regardless of input device_version format
        if device_version.split("-")[-1] < "7.2.1":
            for path in self.service.path:
                if path.dynamic and path.dynamic.pce and path.dynamic.constraints and \
                        path.dynamic.constraints.segments and \
                        path.dynamic.constraints.segments.sid_algorithm:
                    raise UserErrorException(self.log,
                                             StatusCodes.PCEP_AND_SID_ALGO_NOT_SUPPORTED) \
                        .set_context("PCEP_AND_SID_ALGO_NOT_SUPPORTED",
                                     "PCEP and Sid algorithm cannot be configured together"
                                     " on XR devices below 7.2.1")\
                        .finish()

    def conf_sr_template(self, device):
        self.log.info(f"Configuring SR ODN policy template on IOSXR device {device}")
        IosXR.template_sr_policy_template_new(service=self.service, device=device)

    def conf_sr_policy(self, device):
        self.log.info(f"Configuring SR policy on IOSXR device {device}")
        endpoint = self.service.tail_end
        self.check_path_constraints(str(self.root.devices.device[device].platform.version))
        IosXR.template_sr_policy(service=self.service, endpoint=endpoint, device=device)

    def get_max_sid_depth(self, root, device):
        self.log.info(f"Getting max-sid-depth on IOSXR device {device}")
        return root.\
            devices.device[device].config.cisco_ios_xr__segment_routing.\
            traffic_eng.maximum_sid_depth

    def validate_srv6_te(self, root, service, device):
        # SRv6-TE is supported on XR 7.3.2 and 7.5.1 and above
        version = get_device_platform_version(self, root, device)
        ned_id = get_device_ned_id(self, root, device)
        if "cisco-iosxr-cli" not in ned_id or not(version == "7.3.2" or version >= "7.5.1"):
            raise UserErrorException(
                self.log, StatusCodes.SRV6_NOT_SUPPORTED
            ).set_context(
                "SRv6 validation",
                "Either device not synced or NED/device image "
                "version is not supported for SRv6-TE",
            ).add_state(
                "Service", service.name
            ).add_state(
                "Device", device
            ).add_state(
                "NED-ID", ned_id
            ).add_state(
                "Version", version
            ).finish()

    def sr_policy_self_test(self, uinfo, root, service, device):
        self.log.info(f"Running SR Policy self test on IOSXR device {device}")
        # If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", None)
        else:
            # check status of policy config on device
            self_test_command = ("show segment-routing traffic-eng policy name srte_c_"
                                 + str(service.color) + "_ep_"
                                 + str(service.tail_end) + ' | include "Admin"')

            # Execute self test
            action = root.devices.device[device].live_status.cisco_ios_xr_stats__exec.any
            input = action.get_input()
            input.args.create(self_test_command)
            return self.run_self_test(action, input, service, device)

    def run_self_test(self, action, input, service, device):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(input)
                self.log.info(f"Self Test result: {output.result}")
                # Return self test result
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
                    self.log.info("Connection reset on device, reached max-retries "
                                  f"for: {service.name}")
                    return (
                        "failed", "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request sr-te cisco-sr-te-cfp-sr-policies:policies "
                        + "policy " + service.name + " head-end " + device + " action self-test'")
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, will retry self "
                                  f"test for: {service.name}")
                else:
                    traceback.print_exc()
                    raise e
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
