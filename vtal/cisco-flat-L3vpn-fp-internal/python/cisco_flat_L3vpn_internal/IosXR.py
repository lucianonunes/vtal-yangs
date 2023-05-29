import ncs
import traceback
import re
import time
from .network import getIpAddress, getNetMask
from cisco_tsdn_core_fp_common.utils import is_netsim_device


class IosXR:
    def conf_l3vpn(self, endpoint):
        self.log.info(f"Configuring Flat L3VPN/SR on IOSXR device {endpoint.access_pe}")
        export_mappingv4 = ""
        export_mappingv6 = ""
        import_mappingv4 = ""
        import_mappingv6 = ""
        import_prependv4_policy = ""
        import_prependv6_policy = ""
        import_appendv4_policy = ""
        import_appendv6_policy = ""
        export_prependv4_policy = ""
        export_prependv6_policy = ""
        export_appendv4_policy = ""
        export_appendv6_policy = ""

        if endpoint.sr_te.exists() and endpoint.sr_te.type == "odn":
            # EXPORT ROUTE POLICY
            color_id = ""
            ipv4 = False
            ipv6 = False
            for color in endpoint.sr_te.odn.export_color:
                color_id = str(color.id)

                # IPV4 handling
                if color.ipv4.exists():
                    ipv4 = True
                    export_mappingv4 = self._generate_color_mapping(color.ipv4.prefix,
                                                                    export_mappingv4, color_id,
                                                                    color.exclusive)

                # IPV6 handling
                if color.ipv6.exists():
                    ipv6 = True
                    export_mappingv6 = self._generate_color_mapping(color.ipv6.ipv6_prefix,
                                                                    export_mappingv6, color_id,
                                                                    color.exclusive)

            if export_mappingv4 == "" and ipv4:
                export_mappingv4 = f"  set extcommunity color COLOR_{color_id}"
            if export_mappingv6 == "" and ipv6:
                export_mappingv6 = f"  set extcommunity color COLOR_{color_id}"

            for extra_policy in endpoint.sr_te.odn.export_extra_policy:
                (export_prependv4_policy, export_prependv6_policy,
                 export_appendv4_policy, export_appendv6_policy) =\
                    self._generate_extra_policy_mapping(extra_policy,
                                                        export_prependv4_policy,
                                                        export_prependv6_policy,
                                                        export_appendv4_policy,
                                                        export_appendv6_policy)

            # IMPORT ROUTE POLICY
            color_id = ""
            ipv4 = False
            ipv6 = False
            for color in endpoint.sr_te.odn.import_color:
                color_id = str(color.id)

                # IPV4 handling
                if color.ipv4.exists():
                    ipv4 = True
                    import_mappingv4 = self._generate_color_mapping(color.ipv4.prefix,
                                                                    import_mappingv4, color_id,
                                                                    color.exclusive)

                # IPV6 handling
                if color.ipv6.exists():
                    ipv6 = True
                    import_mappingv6 = self._generate_color_mapping(color.ipv6.ipv6_prefix,
                                                                    import_mappingv6, color_id,
                                                                    color.exclusive)

            if import_mappingv4 == "" and ipv4:
                import_mappingv4 = f"  set extcommunity color COLOR_{color_id}"
            if import_mappingv6 == "" and ipv6:
                import_mappingv6 = f"  set extcommunity color COLOR_{color_id}"

            for extra_policy in endpoint.sr_te.odn.import_extra_policy:
                (import_prependv4_policy, import_prependv6_policy,
                 import_appendv4_policy, import_appendv6_policy) =\
                    self._generate_extra_policy_mapping(extra_policy,
                                                        import_prependv4_policy,
                                                        import_prependv6_policy,
                                                        import_appendv4_policy,
                                                        import_appendv6_policy)

        export_mappingv4 = export_prependv4_policy + export_mappingv4 + export_appendv4_policy
        export_mappingv6 = export_prependv6_policy + export_mappingv6 + export_appendv6_policy
        import_mappingv4 = import_prependv4_policy + import_mappingv4 + import_appendv4_policy
        import_mappingv6 = import_prependv6_policy + import_mappingv6 + import_appendv6_policy

        l3vpn_vars = ncs.template.Variables()
        l3vpn_vars.add("EXPORT_COLOR_IPV4", export_mappingv4)
        l3vpn_vars.add("EXPORT_COLOR_IPV6", export_mappingv6)
        l3vpn_vars.add("IMPORT_COLOR_IPV4", import_mappingv4)
        l3vpn_vars.add("IMPORT_COLOR_IPV6", import_mappingv6)

        l3vpn_vars.add("IPV4_ADDR", "")
        l3vpn_vars.add("IPV4_MASK", "")

        if endpoint.pe_ip_addr is not None:
            l3vpn_vars.add("IPV4_ADDR", getIpAddress(endpoint.pe_ip_addr))
            l3vpn_vars.add("IPV4_MASK", getNetMask(endpoint.pe_ip_addr))

        as_no = ""
        if endpoint.as_no is not None:
            as_no = endpoint.as_no
        elif endpoint.as_no_from_device.exists():
            as_no = self.get_bgp_as_from_device(self.root, endpoint.access_pe)
        l3vpn_vars.add("AS_NO", as_no)
        l3vpn_vars.add("PASS_ALL_VALUE", "  pass\r\n")
        l3vpn_template = ncs.template.Template(endpoint)
        l3vpn_template.apply("cisco-flat-L3vpn-fp-cli-template", l3vpn_vars)

    def _generate_color_mapping(self, prefixes, mapping, color_id, exclusive):
        # Build prefix list
        prefix_list = ""
        for prefix in prefixes:
            if prefix_list == "":
                prefix_list = prefix_list + prefix
            else:
                prefix_list = prefix_list + ", " + prefix
        # Build mapping
        if prefix_list != "":
            if exclusive:
                return (f"{mapping}  if destination in ({prefix_list}) then\r\n"
                        f"    set extcommunity color COLOR_{color_id}\r\n"
                        "    done\r\n  endif\r\n")
            else:
                return (f"{mapping}  if destination in ({prefix_list}) then\r\n"
                        f"    set extcommunity color COLOR_{color_id}\r\n"
                        "  endif\r\n")
        else:
            return f"{mapping}  set extcommunity color COLOR_{color_id}\r\n"

    def _generate_extra_policy_mapping(self, extra_policy,
                                       prependv4_policy, prependv6_policy,
                                       appendv4_policy, appendv6_policy):
        l_prependv4_policy = prependv4_policy
        l_prependv6_policy = prependv6_policy
        l_appendv4_policy = appendv4_policy
        l_appendv6_policy = appendv6_policy
        name = str(extra_policy.name)
        if str(extra_policy.operation) == "prepend":
            if str(extra_policy.address) in ["ipv4", "both"]:
                l_prependv4_policy = f"{prependv4_policy}  apply {name}\r\n"
            if str(extra_policy.address) in ["ipv6", "both"]:
                l_prependv6_policy = f"{prependv6_policy}  apply {name}\r\n"
        else:
            if str(extra_policy.address) in ["ipv4", "both"]:
                l_appendv4_policy = f"{appendv4_policy}  apply {name}\r\n"
            if str(extra_policy.address) in ["ipv6", "both"]:
                l_appendv6_policy = f"{appendv6_policy}  apply {name}\r\n"
        return (l_prependv4_policy, l_prependv6_policy, l_appendv4_policy, l_appendv6_policy)

    def check_if_interface_exists(self, root, endpoint, service_interface_name,
                                  service_interface_id):
        service_interface = service_interface_name + service_interface_id

        self.log.info(f"Checking {service_interface} on IOSXR device {endpoint.access_pe}")

        device_interface = root.devices.device[endpoint.access_pe].config.cisco_ios_xr__interface
        device_interfaces = getattr(device_interface, service_interface_name.replace("-", "_"),
                                    None)

        if device_interfaces:
            for interface in device_interfaces:
                if service_interface == service_interface_name + str(interface.id):
                    return True

        # CSCvt12530 - In case interface is no shut on the device, it won't show up
        # on running-config. We need a live-status check.
        admin_status = (root.devices.device[endpoint.access_pe]
                        .live_status.if__interfaces_state.interface[service_interface]
                        .admin_status)

        if admin_status is None:
            return False
        else:
            return True

    def l3vpn_self_test(self, root, service, vrf_name, device, src, dst):
        self.log.info(f"Running L3vpn self test on IOSXR device {device}")
        # If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", "netsim")
        else:
            # ping vrf src -> dest
            self_test_command = "ping vrf " + vrf_name + " " + dst + " source " + src

            # Execute self test
            action = root.devices.device[
                device
            ].live_status.cisco_ios_xr_stats__exec.any
            input = action.get_input()
            input.args.create(self_test_command)
            return self.run_self_test(action, input, service)

    def get_interface_shutdown_template(self):
        return "cisco-flat-L3vpn-fp-no-shutdown-iosxr-cli"

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
                    self.log.info("Connection reset on device, will retry self test "
                                  f"for: {service.name}")
                else:
                    traceback.print_exc()
                    return ("failed", str(e))
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def is_vrf_address_family_active(self, root, device, bgp_as_no, vrf_name):
        bgp_no_instance_path = root.devices.device[device].config.cisco_ios_xr__router.\
            bgp.bgp_no_instance

        if bgp_as_no in bgp_no_instance_path:
            vrf_path = bgp_no_instance_path[bgp_as_no].vrf
            if vrf_name in vrf_path:
                vrf = vrf_path[vrf_name]

                if (
                    vrf.address_family.ipv4.unicast
                    or vrf.address_family.ipv4.multicast
                    or vrf.address_family.ipv4.labeled_unicast
                    or vrf.address_family.ipv4.mdt
                    or vrf.address_family.ipv4.rt_filter
                    or vrf.address_family.ipv4.flowspec
                    or vrf.address_family.ipv4.mvpn
                    or vrf.address_family.ipv4.sr_policy
                    or vrf.address_family.ipv6.unicast
                    or vrf.address_family.ipv6.multicast
                    or vrf.address_family.ipv6.flowspec
                    or vrf.address_family.ipv6.mvpn
                    or vrf.address_family.ipv6.sr_policy
                    or vrf.address_family.vpnv4.unicast
                    or vrf.address_family.vpnv4.multicast
                    or vrf.address_family.vpnv4.flowspec
                    or vrf.address_family.vpnv6.unicast
                    or vrf.address_family.vpnv6.multicast
                    or vrf.address_family.l2vpn.mspw
                    or vrf.address_family.l2vpn.vpls_vpws
                    or vrf.address_family.l2vpn.evpn
                    or vrf.address_family.link_state.link_state
                ):
                    return True

        return False

    def get_vrf_rd(self, root, device, vrf_name):
        return root.devices.device[device].config.cisco_ios_xr__vrf.vrf_list[vrf_name].rd

    def get_bgp_vrf_rd(self, root, device, bgp_as_no, vrf_name):
        return (root.devices.device[device].config.cisco_ios_xr__router
                .bgp.bgp_no_instance[bgp_as_no].vrf[vrf_name].rd)

    def get_bgp_as_from_device(self, root, device):
        bgp_no_instance_path = root.devices.device[device].config.cisco_ios_xr__router.\
            bgp.bgp_no_instance
        for bgp in bgp_no_instance_path:
            return bgp.id

        return ""

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
