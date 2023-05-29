import ncs
import traceback
import time
from cisco_tsdn_core_fp_common.utils import is_netsim_device
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import \
    UserErrorException
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from ipaddress import ip_address


class UMXR:
    def conf_l2vpn(self, site, local):
        self.log.info(f"Configuring Flat L2VPN/SR on UM IOSXR NETCONF for service {site.pe}")
        mappingv4 = "\n"
        mappingv6 = "\n"
        mappingevpn_rt = "\n"
        mappingevpn_mp = "\n"
        if ((self.service.service_type == "evpn-vpws"
            or self.service.service_type == "evpn-multipoint")
                and site.sr_te.exists()
                and site.sr_te.type == "odn"):
            color_id = ""
            for color in site.sr_te.odn.color:
                color_id = str(color.id)

                # IPV4 handling
                if color.ipv4.exists():
                    # RD List
                    rd_list = ""
                    for rd in color.ipv4.rd:
                        if rd_list == "":
                            rd_list = rd_list + rd
                        else:
                            rd_list = rd_list + ", " + rd
                    if rd_list != "":
                        mappingv4 = (mappingv4 + "  if rd in (" + rd_list + ") then\r\n     "
                                     + "set extcommunity color COLOR_"
                                     + color_id + "\r\n   endif\r\n")

                    if rd_list == "":
                        mappingv4 = (mappingv4 + "  set extcommunity color COLOR_"
                                     + color_id + "\r\n")

                # IPV6 handling
                if color.ipv6.exists():
                    # RD List
                    rd_list = ""
                    for rd in color.ipv6.rd:
                        if rd_list == "":
                            rd_list = rd_list + rd
                        else:
                            rd_list = rd_list + ", " + rd
                    if rd_list != "":
                        mappingv6 = (mappingv6 + "  if rd in (" + rd_list + ") then\r\n     "
                                     + "set extcommunity color COLOR_"
                                     + color_id + "\r\n   endif\r\n")

                    if rd_list == "":
                        mappingv6 = (mappingv6 + "  set extcommunity color COLOR_"
                                     + color_id + "\r\n")

                # EVPN Route-Type handling
                if ((self.service.service_type == "evpn-multipoint")
                        and color.evpn_route_type.exists()):
                    # Route-Type List
                    evpn_rt_list = ""
                    for evpn_rt in color.evpn_route_type.route_type:
                        if evpn_rt_list == "":
                            evpn_rt_list = f"if evpn-route-type is {evpn_rt}"
                        else:
                            evpn_rt_list += f" or evpn-route-type is {evpn_rt}"
                    if evpn_rt_list != "":
                        mappingevpn_rt = (mappingevpn_rt + " "
                                          + evpn_rt_list + " then\r\n     "
                                          + "set extcommunity color COLOR_"
                                          + color_id + "\r\n   endif\r\n")

                    if evpn_rt_list == "":
                        mappingevpn_rt = (mappingevpn_rt + "  set extcommunity color COLOR_"
                                          + color_id + "\r\n")

            if self.service.service_type == "evpn-multipoint":
                # parse beginning of newline to allow concatenation
                # without extra newlines
                if mappingv4 != "\n":
                    mappingv6 = mappingv6[1:]
                if mappingv6 != "\n":
                    mappingevpn_rt = mappingevpn_rt[1:]

                mappingevpn_mp = (mappingv4 + mappingv6 + mappingevpn_rt)

        if mappingv4 == "\n":
            mappingv4 = ""
        if mappingv6 == "\n":
            mappingv6 = ""
        if mappingevpn_mp == "\n":
            mappingevpn_mp = ""

        policy_name = ""
        # Get the SR-Policy-Name
        if self.service.service_type == "evpn-multipoint":
            pass  # Currenlty only supports sr-te/odn
        elif site.sr_te.preferred_path:
            policy_name = site.sr_te.preferred_path.policy
            policy_key = (policy_name, site.pe)
            try:
                # Form the policy name
                policy_node = self.root.cisco_sr_te_cfp_internal__sr_te.\
                    cisco_sr_te_cfp_sr_policies_internal__policies.policy
                if policy_key in policy_node:
                    policy_service = policy_node[policy_key]
                    if policy_service.srv6.exists():
                        err_msg = ("L2VPN association with SRv6 is not supported for "
                                   + "this release.")
                        raise UserErrorException(self.log, StatusCodes.SRV6_NOT_SUPPORTED) \
                            .set_context("Not supported ", err_msg) \
                            .add_state("Device", str(site.pe)) \
                            .add_state("Service", str(self.service.name)) \
                            .add_state("Policy", str(policy_name)) \
                            .finish()
                    policy_name = ("srte_c_" + str(policy_service.color)
                                   + "_ep_" + str(policy_service.tail_end))
            except UserErrorException:
                raise
            except Exception:
                traceback.print_exc()
                policy_name = site.sr_te.preferred_path.policy

        l2vpn_vars = ncs.template.Variables()
        l2vpn_vars.add("LOCAL_NODE", "true" if local is True else "false")
        l2vpn_vars.add("COLOR_IPV4", mappingv4)
        l2vpn_vars.add("COLOR_IPV6", mappingv6)
        l2vpn_vars.add("COLOR_EVPN_MP", mappingevpn_mp)
        l2vpn_vars.add("SR_POLICY_NAME", policy_name)

        l2vpn_template = ncs.template.Template(site)
        template = "cisco-flat-L2vpn-fp-um-p2p-template"

        if self.service.service_type == "evpn-vpws":
            template = "cisco-flat-L2vpn-fp-um-evpn-vpws-template"
        elif self.service.service_type == "evpn-multipoint":
            template = "cisco-flat-L2vpn-fp-um-evpn-multipoint-template"
            l2vpn_template.apply(template, l2vpn_vars)

            evi_id = self.service.flat_L2vpn_evpn_multipoint.evi_id
            evpn_route_targets = \
                self.root.devices.device[site.pe].config.evpn.evis.evi[evi_id].bgp.route_target

            for vpn_target in site.vpn_target:
                (addr_or_as, index) = vpn_target.rt_value.split(":")

                if vpn_target.rt_type == "both":
                    rt_types = ["import", "export"]
                else:
                    rt_types = [vpn_target.rt_type]

                for rt_type in rt_types:
                    # 'import' is a special name in python, so we cannot call it directly.
                    if rt_type == "import":
                        type = ncs.maagic.cd(evpn_route_targets, "import")
                    else:
                        type = evpn_route_targets.export

                    if self._valid_ip_addr(addr_or_as):
                        route_target = type.ipv4_address_rts.ipv4_address_rt
                    elif int(addr_or_as) <= 65535:
                        route_target = type.two_byte_as_rts.two_byte_as_rt
                    else:
                        route_target = type.four_byte_as_rts.four_byte_as_rt

                    route_target.create((addr_or_as, index))

            return

        l2vpn_template.apply(template, l2vpn_vars)

    def conf_l2vpn_rp(self, rr_parent_route_policy):
        self.log.info("Configuring Route Policy on IOSXR for service "
                      f"{rr_parent_route_policy.name} on {rr_parent_route_policy.device}")

        # Build parent route policy blob by applying local route policy names
        parent_rp_blob = ""
        # Iterate over all local route policies
        for local_route_policy in rr_parent_route_policy.local_route_policy:
            # Get prefix
            prefixes = []
            if local_route_policy.prefix:
                prefixes.append(local_route_policy.prefix)
            else:
                if local_route_policy.ipv4.exists():
                    prefixes.append("SET_COLORv4_service_")
                if local_route_policy.ipv6.exists():
                    prefixes.append("SET_COLORv6_service_")

            # Apply local route policy name in parent route policy blob
            for prefix in prefixes:
                parent_rp_blob += f"  apply {prefix}{local_route_policy.name}\r\n"
        # Insert parent_rp_blob in original parent route policy definition
        if rr_parent_route_policy.original_rr_route_policy:
            # original_rr_route_policy will look like:
            # route-policy fib
            # end-policy
            # !
            # Split string to lines (and keep \r \n)
            original_rr_list = str(rr_parent_route_policy.
                                   original_rr_route_policy).splitlines(True)
            if len(original_rr_list) < 2 or "route-policy" not in original_rr_list[0]:
                # Netsim
                parent_rp_blob += str(rr_parent_route_policy.original_rr_route_policy)
            else:
                # Real device
                parent_rp_blob = f"{original_rr_list[0]}"\
                                 f"{parent_rp_blob}{''.join(original_rr_list[1:])}"
        # Configure parent route policy
        rp_vars = ncs.template.Variables()
        rp_vars.add("POLICY_BLOB", parent_rp_blob)
        rp_template = ncs.template.Template(rr_parent_route_policy)
        rp_template.apply("cisco-flat-L2vpn-fp-um-route-policy-iosxr-nc", rp_vars)

    def validate_parent_policy_exists(self, root, device, parent_policy):
        routing_policy = self._get_routing_policy(root, device)

        if parent_policy in routing_policy.route_policies.route_policy:
            return True

        return False

    def get_original_policy(self, root, device, parent_policy):
        routing_policy = self._get_routing_policy(root, device)
        return routing_policy.route_policies.route_policy[parent_policy].rpl_route_policy

    def check_if_interface_exists(self, root, site,
                                  service_interface_name, service_interface_id):
        service_interface = service_interface_name + service_interface_id
        self.log.info(f"Checking {service_interface} on IOSXR NC device {site.pe}")
        device_interfaces = root.devices.device[site.pe].config. \
            Cisco_IOS_XR_um_interface_cfg__interfaces.interface
        for interface in device_interfaces:
            if service_interface == interface.interface_name:
                return True

        # CSCvt12530 - In case interface is no shut on the device, it won't show up
        # on running-config. We need a live-status check.
        admin_status = (root.devices.device[site.pe].live_status.
                        Cisco_IOS_XR_pfi_im_cmd_oper__interfaces.
                        interface_briefs.interface_brief[service_interface].state)
        self.log.info(f"admin_status: {admin_status}")
        if admin_status is None:
            return False
        else:
            return True

        return False

    def get_interface_shutdown_template(self):
        return "cisco-flat-L2vpn-fp-no-shutdown-iosxr-um-nc"

    def l2vpn_self_test(self, root, service, device, xc_group, xc_name):
        self.log.info(f"Running l2vpn self test on IOSXR NETCONF device {device}")
        # IF netsim return success
        if is_netsim_device(self, root, device):
            return ("success", "netsim")
        else:
            # Execute self test
            return self.run_self_test(root, service, device, xc_group, xc_name)

        return ("failed", "Failed to run self-test")

    def run_self_test(self, root, service, device, xc_group, xc_name):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = root.devices.device[device].live_status.\
                    Cisco_IOS_XR_l2vpn_oper__l2vpnv2.active.\
                    xconnects.xconnect[xc_group, xc_name].state

                self.log.info(f"L2vpn Self Test result: {output}")

                if ("l2vpn-xc-state-down" == output or "l2vpn-xc-state-unresolved" == output):
                    return ("failed", str(output))
                elif "l2vpn-xc-state-up" == output:
                    return ("success", str(output))
                else:
                    return ("failed", f"Unexpected result: {output}")

            except Exception as e:
                if ("Connection reset in new state" in str(e) and retry == (max_retries - 1)):

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

    def _valid_ip_addr(self, addr):
        try:
            ip_address(addr)
            return True
        except ValueError:
            return False

    def _get_routing_policy(self, root, device):
        ned_id = TsdnUtils.get_device_ned_id(self, root, device)
        if ned_id == "cisco-iosxr-nc-7.5:cisco-iosxr-nc-7.5":
            return (root.devices.device[device].config
                    .Cisco_IOS_XR_policy_repository_cfg__routing_policy)
        else:
            return (root.devices.device[device].config
                    .Cisco_IOS_XR_um_route_policy_cfg__routing_policy)

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
