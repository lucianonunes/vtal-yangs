import ncs
import traceback
import time
from .network import getIpAddress, getNetMask
from cisco_tsdn_core_fp_common.utils import is_netsim_device


class NativeXR:
    def conf_l3vpn(self, endpoint):
        self.log.info(f"Configuring Flat L3VPN/SR on NC IOSXR device {endpoint.access_pe}")
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
            # EXTERNAL ROUTE POLICY
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
                export_mappingv4 = export_mappingv4 + f"  set extcommunity color COLOR_{color_id}"
            if export_mappingv6 == "" and ipv6:
                export_mappingv6 = export_mappingv6 + f"  set extcommunity color COLOR_{color_id}"

            for extra_policy in endpoint.sr_te.odn.export_extra_policy:
                (export_prependv4_policy, export_prependv6_policy,
                 export_appendv4_policy, export_appendv6_policy) =\
                    self._generate_extra_policy_mapping(extra_policy,
                                                        export_prependv4_policy,
                                                        export_prependv6_policy,
                                                        export_appendv4_policy,
                                                        export_appendv6_policy)

            # INTERNAL ROUTE POLICY
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
                import_mappingv4 = import_mappingv4 + f"  set extcommunity color COLOR_{color_id}"
            if import_mappingv6 == "" and ipv6:
                import_mappingv6 = import_mappingv6 + f"  set extcommunity color COLOR_{color_id}"

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

        if export_mappingv4:
            export_mappingv4 = "\n" + export_mappingv4
        if export_mappingv6:
            export_mappingv6 = "\n" + export_mappingv6
        if import_mappingv4:
            import_mappingv4 = "\n" + import_mappingv4
        if import_mappingv6:
            import_mappingv6 = "\n" + import_mappingv6

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

        l3vpn_vars.add("VPN_OUI", "")
        l3vpn_vars.add("VPN_INDEX", "")

        if endpoint.vrf.vpn_id is not None:
            endpoint_vpn = endpoint.vrf.vpn_id.split(":", 1)
            l3vpn_vars.add("VPN_OUI", int(str(endpoint_vpn[0]), 16))
            l3vpn_vars.add("VPN_INDEX", int(str(endpoint_vpn[1]), 16))

        as_no = ""
        if endpoint.as_no is not None:
            as_no = endpoint.as_no
        elif endpoint.as_no_from_device.exists():
            as_no = self.get_bgp_as_from_device(self.root, endpoint.access_pe)
        l3vpn_vars.add("AS_NO", str(as_no))

        l3vpn_template = ncs.template.Template(endpoint)
        l3vpn_template.apply("cisco-flat-L3vpn-fp-native-template", l3vpn_vars)

        rp_vars = ncs.template.Variables()
        rp_vars.add("EXPORT_COLOR_IPV4", export_mappingv4)
        rp_vars.add("EXPORT_COLOR_IPV6", export_mappingv6)
        rp_vars.add("IMPORT_COLOR_IPV4", import_mappingv4)
        rp_vars.add("IMPORT_COLOR_IPV6", import_mappingv6)

        rp_template = ncs.template.Template(endpoint)
        rp_template.apply("cisco-flat-L3vpn-fp-routing-policy-iosxr-nc", rp_vars)

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
                return(f"{mapping}  if destination in ({prefix_list}) then\r\n"
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
        return(l_prependv4_policy, l_prependv6_policy, l_appendv4_policy, l_appendv6_policy)

    def check_if_interface_exists(self, root, endpoint, service_interface_name,
                                  service_interface_id):
        service_interface = service_interface_name + service_interface_id
        self.log.info(f"Checking {service_interface} on IOSXR NC device {endpoint.access_pe}")
        device_interfaces = root.\
            devices.device[endpoint.access_pe].config.\
            Cisco_IOS_XR_ifmgr_cfg__interface_configurations.interface_configuration
        for interface in device_interfaces:
            if service_interface == interface.interface_name:
                return True

        # CSCvt12530 - In case interface is no shut on the device, it won't show up
        # on running-config. We need a live-status check.
        admin_status = (root.devices.device[endpoint.access_pe].live_status.interfaces.
                        interface_briefs.interface_brief[service_interface].state)
        self.log.info(f"admin_status: {admin_status}")
        if admin_status is None:
            return False

        else:
            return True

    def get_interface_shutdown_template(self):
        return "cisco-flat-L3vpn-fp-no-shutdown-iosxr-nc"

    def l3vpn_self_test(self, root, service, vrf_name, device, src, dst):
        self.log.info(f"Running SR Policy self test on IOSXR NETCONF device {device}")
        # IF netsim return success
        if is_netsim_device(self, root, device):
            return ("success", "netsim")
        else:
            # ping vrf src -> dest
            # Execute self test
            action = root.devices.device[device].rpc.Cisco_IOS_XR_ping_act__rpc_ping.ping
            input = action.get_input()
            destination = input.destination.create()
            destination.destination = dst
            destination.vrf_name = vrf_name
            destination.source = src
            ip_version = "ipv4"
            if ":" in src:
                ip_version = "ipv6"
            return self.run_self_test(action, input, service, ip_version)

    def run_self_test(self, action, input, service, ip_version):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(input)
                self.log.info(f"L3vpn Self Test result: {output.ping_response}")

                if ip_version == "ipv4":
                    for ipv4_response in output.ping_response.ipv4:
                        if (ipv4_response.success_rate is not None
                                and ipv4_response.success_rate >= 60):
                            return ("success", str(ipv4_response.success_rate))
                        else:
                            return ("failed", "Unexpected ping result with success-rate: "
                                    + str(ipv4_response.success_rate))
                else:
                    # IPV6 ping
                    if (output.ping_response.ipv6.success_rate is not None
                            and output.ping_response.ipv6.success_rate >= 60):
                        return ("success", str(output.ping_response.ipv6.success_rate))
                    else:
                        return ("failed", "Unexpected ping result with success-rate: "
                                + str(output.ping_response.ipv6.success_rate))

            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (max_retries - 1):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return ("failed", "Device ssh session being used by another transaction. "
                            + "Retry self-test with following command: "
                            + "'request flat-L3vpn " + service.name + " action self-test'")
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, "
                                  f"will retry self test for: {service.name}")
                else:
                    traceback.print_exc()
                    return ("failed", str(e))
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def is_vrf_address_family_active(self, root, device, bgp_as_no, vrf_name):
        vrf_addr = []

        try:
            instance = root.devices.device[device].config.Cisco_IOS_XR_ipv4_bgp_cfg__bgp.instance

            if "default" in instance:
                instance_as = instance["default"].instance_as
                bgp_as = self.get_bgp_as(bgp_as_no)

                if bgp_as in instance_as:
                    four_byte_as = self.get_bgp_four_byte_as(bgp_as_no)

                    if four_byte_as in instance_as[bgp_as].four_byte_as:
                        vrf_addr = instance_as[bgp_as].four_byte_as[four_byte_as]. \
                            vrfs.vrf[vrf_name].vrf_global.vrf_global_afs.vrf_global_af

        except AttributeError:
            bgp_as = (
                root.devices.device[device].config
                .Cisco_IOS_XR_um_router_bgp_cfg__router.Cisco_IOS_XR_um_router_bgp_cfg__bgp
                .Cisco_IOS_XR_um_router_bgp_cfg__as
            )

            if bgp_as_no in bgp_as:
                vrf_addr = bgp_as[bgp_as_no].vrfs.vrf[vrf_name].address_families.address_family

        if len(vrf_addr) > 0:
            return True

        return False

    def get_vrf_rd(self, root, device, vrf_name):
        try:
            rd_inst = root.\
                devices.device[device].config.Cisco_IOS_XR_infra_rsi_cfg__vrfs.vrf[vrf_name].\
                Cisco_IOS_XR_ipv4_bgp_cfg__bgp_global.route_distinguisher
        except AttributeError:
            rd_inst = root.\
                devices.device[device].config.Cisco_IOS_XR_um_vrf_cfg__vrfs.vrf[vrf_name].\
                Cisco_IOS_XR_um_router_bgp_cfg__rd

        return self.get_rd_value(rd_inst)

    def get_bgp_vrf_rd(self, root, device, bgp_as_no, vrf_name):
        try:
            rd_inst = root.devices.device[device].config.Cisco_IOS_XR_ipv4_bgp_cfg__bgp.\
                instance["default"].\
                instance_as[self.get_bgp_as(bgp_as_no)].\
                four_byte_as[self.get_bgp_four_byte_as(bgp_as_no)].vrfs.vrf[vrf_name].\
                vrf_global.route_distinguisher
        except Exception:
            rd_inst = (
                root.devices.device[device].config
                .Cisco_IOS_XR_um_router_bgp_cfg__router
                .Cisco_IOS_XR_um_router_bgp_cfg__bgp
                .Cisco_IOS_XR_um_router_bgp_cfg__as[
                    bgp_as_no
                ].vrfs.vrf[vrf_name].rd
            )

        return self.get_rd_value(rd_inst)

    def get_rd_value(self, rd_inst):
        try:
            if rd_inst.type == "ipv4-address":
                return "".join([str(rd_inst.address), ":", str(rd_inst.address_index)])
            elif rd_inst.type == "as" or rd_inst.type == "four-byte-as":
                as_xx = rd_inst.as_xx
                as_yy = rd_inst.Cisco_IOS_XR_ipv4_bgp_cfg__as
                return "".join([str((65535 * as_xx) + as_yy), ":", str(rd_inst.as_index)])
            elif rd_inst.type == "auto":
                return "auto"
            else:
                return None
        except AttributeError:
            # Auto is only supported under router > bgp > vrf > rd and not under vrfs > vrf > rd.
            # Ignore the exception that occurs for vrfs > vrf > rd.
            try:
                if rd_inst.auto:
                    return "auto"
            except AttributeError:
                pass
            if rd_inst.two_byte_as:
                return f"{rd_inst.two_byte_as.as_number}:{rd_inst.two_byte_as.index}"
            elif rd_inst.four_byte_as:
                return f"{rd_inst.four_byte_as.as_number}:{rd_inst.four_byte_as.index}"
            elif rd_inst.ip_address:
                return f"{rd_inst.ip_address.ipv4_address}:{rd_inst.index}"
            else:
                return None

    def get_bgp_as(self, bgp_as_no):

        if "." in str(bgp_as_no):
            return str(bgp_as_no).split(".")[0]
        else:
            return str(bgp_as_no // 65535)

    def get_bgp_four_byte_as(self, bgp_as_no):

        if "." in str(bgp_as_no):
            return (int(str(bgp_as_no).split(".")[0]) * 65535) + int(
                str(bgp_as_no).split(".")[1])
        else:
            return bgp_as_no

    def get_bgp_as_from_device(self, root, device):
        try:
            bgp_instance_path = root.devices.device[device].config.\
                Cisco_IOS_XR_ipv4_bgp_cfg__bgp.instance["default"].instance_as
            for bgp in bgp_instance_path:
                for four_byte_as in bgp.four_byte_as:
                    # Return 4 byte as
                    return four_byte_as.Cisco_IOS_XR_ipv4_bgp_cfg__as

        except AttributeError:
            bgp_instance_path = (root.devices.device[device].config
                                 .Cisco_IOS_XR_um_router_bgp_cfg__router
                                 .Cisco_IOS_XR_um_router_bgp_cfg__bgp
                                 .Cisco_IOS_XR_um_router_bgp_cfg__as)
            for bgp in bgp_instance_path:
                return bgp.as_number

        return ""

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
