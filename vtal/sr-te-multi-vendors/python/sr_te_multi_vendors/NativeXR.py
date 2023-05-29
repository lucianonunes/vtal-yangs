import ncs
import re
import _ncs
import time
import traceback
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.utils import is_netsim_device, get_device_ned_id

ipv4_address = re.compile(
    "^(?:(?:[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}"
    "(?:[0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
)


class NativeXR:
    @staticmethod
    def template_sr_segment_list(service, device, name, index, label, ipv4):
        sr_segment_list_vars = ncs.template.Variables()
        sr_segment_list_template = ncs.template.Template(service)
        sr_segment_list_vars.add("device", device)
        sr_segment_list_vars.add("name", name)
        sr_segment_list_vars.add("index", index)
        sr_segment_list_vars.add("label", label)
        sr_segment_list_vars.add("ipv4", ipv4)
        sr_segment_list_template.apply("cisco-sr-te-cfp-native-sr-segment-list",
                                       sr_segment_list_vars)

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
        sr_policy_template.apply("cisco-sr-te-cfp-native-sr-non-odn-policy",
                                 sr_policy_vars)

    @staticmethod
    def template_sr_policy_template_new(service, device):
        sr_policy_vars = ncs.template.Variables()
        sr_policy_template = ncs.template.Template(service)
        sr_policy_vars.add("HEAD_END", device)
        sr_policy_template.apply("cisco-sr-te-cfp-native-sr-odn-policy", sr_policy_vars)

    def conf_segment_list(self, device):
        self.log.info(f"Configuring SegmentLIST on IOSXR NC device {device}")
        # For non CS
        for path in self.service.path:
            # Check if an explicit path needs to be configured on the target Device
            if path.explicit.exists():
                for sid_list in path.explicit.sid_list:
                    for sid in sid_list.sid:
                        NativeXR.template_sr_segment_list(
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
        #             NativeXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.forward_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )
        #         for sid in explicit.reverse_sid:
        #             NativeXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.reverse_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )

        #     # Protect
        #     if circuit_style.protect_path.explicit:
        #         explicit = circuit_style.protect_path.explicit
        #         for sid in explicit.forward_sid:
        #             NativeXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.forward_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )
        #         for sid in explicit.reverse_sid:
        #             NativeXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.reverse_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )

        #     # Restore
        #     if circuit_style.restore_path.explicit:
        #         explicit = circuit_style.restore_path.explicit
        #         for sid in explicit.forward_sid:
        #             NativeXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.forward_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )
        #         for sid in explicit.reverse_sid:
        #             NativeXR.template_sr_segment_list(
        #                 service=self.service, device=device,
        #                 name=explicit.reverse_sid_list_name, index=sid.index,
        #                 label=sid.mpls.label if sid.type == "mpls" else "",
        #                 ipv4=sid.ipv4.address if sid.type == "ipv4" else "",
        #             )

    def conf_sr_template(self, device):
        self.log.info(f"Configuring SR ODN policy template on IOSXR NC device {device}")
        NativeXR.template_sr_policy_template_new(self.service, device)

    def conf_sr_policy(self, device):
        self.log.info(f"Configuring SR policy on IOSXR NC device {device}")
        endpoint = self.service.tail_end
        NativeXR.template_sr_policy(service=self.service, endpoint=endpoint, device=device)

    def get_max_sid_depth(self, root, device):
        self.log.info(f"Getting max-sid-depth on IOSXR NETCONF device {device}")
        return (root.devices.device[device].config.Cisco_IOS_XR_segment_routing_ms_cfg__sr.
                Cisco_IOS_XR_infra_xtc_agent_cfg__traffic_engineering.maximum_sid_depth)

    def validate_srv6_te(self, root, service, device):
        # SRv6-TE is supported on XR 7.3.2 and 7.5.1 and above
        ned_id = get_device_ned_id(self, root, device)
        if not ((ned_id == "cisco-iosxr-nc-7.3:cisco-iosxr-nc-7.3"
                 or ned_id == "cisco-iosxr-nc-7.3") or
                (ned_id == "cisco-iosxr-nc-7.5:cisco-iosxr-nc-7.5"
                 or ned_id == "cisco-iosxr-nc-7.5") or
                (ned_id == "cisco-iosxr-nc-7.6:cisco-iosxr-nc-7.6"
                 or ned_id == "cisco-iosxr-nc-7.6") or
                (ned_id == "cisco-iosxr-nc-7.7:cisco-iosxr-nc-7.7"
                 or ned_id == "cisco-iosxr-nc-7.7")):
            raise UserErrorException(
                self.log, StatusCodes.SRV6_NOT_SUPPORTED
            ).set_context(
                "SRv6 validation",
                "NED/device image version is not supported for SRv6-TE",
            ).add_state(
                "Service", service.name
            ).add_state(
                "Device", device
            ).add_state(
                "NED-ID", ned_id
            ).finish()

    # pylint: disable=S2325
    def sr_policy_self_test(self, uinfo, root, service, device):
        self.log.info(f"Running SR Policy self test on IOSXR NETCONF device {device}")
        #  IF netsim return success
        if is_netsim_device(self, root, device):
            return ("success", None)
        else:
            #  check status of policy config on device
            return self.run_self_test(uinfo, root, service, device)

    @staticmethod
    def get_self_test_result(uinfo, device, policy_name):
        username = uinfo.username
        with ncs.maapi.single_read_trans(username, "system", db=ncs.OPERATIONAL) as trans:
            device_policy_path = ("/ncs:devices/device{" + device
                                  + "}/live-status/infra-xtc-agent-oper:xtc")
            qh = None
            try:
                trans.exists(device_policy_path)
                qh = _ncs.maapi.query_start(
                    trans.maapi.msock, trans.th, "/ncs:devices/device[name='"
                    + device + "']/live-status" + "/infra-xtc-agent-oper:xtc/policies/policy"
                    + "[policy-name = '" + policy_name + "']", "/", 0, 1, _ncs.QUERY_STRING,
                    ["administrative-up", "operational-up"], [])
            except Exception:
                qh = _ncs.maapi.query_start(
                    trans.maapi.msock, trans.th, "/ncs:devices/device[name='" + device
                    + "']/live-status" + "/Cisco-IOS-XR-infra-xtc-agent-oper:xtc/policies/policy"
                    + "[policy-name = '" + policy_name + "']", "/", 0, 1, _ncs.QUERY_STRING,
                    ["administrative-up", "operational-up"], [])
            admin_result = "0"
            oper_result = "0"

            if qh is not None:
                res = _ncs.maapi.query_result(trans.maapi.msock, qh)
                while res and (res.nresults > 0):
                    for r in res:
                        admin_result = r[0]
                        oper_result = r[1]
                        break
                    break

            _ncs.maapi.query_stop(trans.maapi.msock, qh)
            return (bool(int(admin_result)), bool(int(oper_result)))

    def run_self_test(self, uinfo, root, service, device):
        max_retries = 2
        for retry in range(max_retries):
            try:
                policy_name = ("srte_c_" + str(service.color) + "_ep_" + str(service.tail_end))
                (admin_result, oper_result) = NativeXR.get_self_test_result(uinfo,
                                                                            device, policy_name)
                self.log.info(f"Self Test result: admin-up {admin_result} , oper-up {oper_result}")
                #  Return self test result
                if admin_result and oper_result:
                    return ("success", None)
                elif not admin_result:
                    return ("failed", "Admin state is down")
                elif not oper_result:
                    return ("failed", "Operational state is down")
                elif not admin_result and not oper_result:
                    return ("failed", "Both Admin & Operational states are down")
                else:
                    return ("failed", "No policy status found on device")
            except Exception as e:
                if(any(re.findall(r"Failed to authenticate|Failed to connect", str(e),
                                  re.IGNORECASE)) and retry == (max_retries - 1)):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return (
                        "failed", "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request sr-te cisco-sr-te-cfp-sr-policies:policies "
                        + "policy " + service.name + " head-end " + device + " action self-test'")
                elif any(re.findall(r"Failed to authenticate|Failed to connect",
                                    str(e), re.IGNORECASE)):
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
