from ncs import ITER_CONTINUE, ITER_RECURSE, ITER_STOP
from _ncs.dp import NCS_SERVICE_CREATE, NCS_SERVICE_UPDATE
from ncs.application import NanoService, Service
from ncs.template import Template, Variables as TemplateVariables
from ncs.maagic import get_node, get_trans
from cisco_tsdn_core_fp_common.status_codes.ietf_l3vpn_nm_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.ietf_l3vpn_nm_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.utils import (
    update_plan_status_codes, remove_status_code_detail,
    update_component_status_codes, validate_service,
    is_cq_enabled, update_state_when_timestamp
)
from re import split
from ipaddress import ip_address, IPv4Address
from traceback import print_exc
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from . import utils as Utils
from core_fp_common import instrumentation
import logging


class IETFL3vpnNMValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        validate_service(self, Utils.ietf_l3vpn_validation_callpoint, tctx, kp)


class IETFL3vpnNMCallback(Service):
    @Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.ietf_l3vpn_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        proplist = dict(proplist)
        proplist["VALIDATION_ERROR"] = ""
        try:
            # Validate status code mapping is loaded
            status_code_cfp = root.cfp_common_status_codes__status_codes.core_function_pack
            if op == NCS_SERVICE_CREATE and not status_code_cfp.exists("IETF-L3NM"):
                raise UserErrorException(
                    self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                ).set_context(
                    "Status Code", "Missing L3NM status code mapping"
                ).add_state(
                    "Keypath", str(kp)
                ).finish()

            # Validate port-id (L3VPN: if-type/if-id)
            if op == NCS_SERVICE_CREATE or op == NCS_SERVICE_UPDATE:
                service = get_node(get_trans(root), kp)
                self.log.info(f"Pre-Mod validation for service: {service.vpn_id}")
                for vpn_node in service.vpn_nodes.vpn_node:

                    if ("cisco-ios-cli" in TsdnUtils
                            .get_device_ned_id_from_dispatch_map(self, root, vpn_node.ne_id)
                            and op == NCS_SERVICE_CREATE):
                        if vpn_node.rd is None and vpn_node.node_ie_profile is None:
                            raise UserErrorException(
                                self.log, StatusCodes.MISSING_RD_ON_XE
                            ).set_context(
                                "Missing route-distinguisher. RD is mandatory for XE"
                            ).add_state(
                                "Endpoint", vpn_node.ne_id
                            ).add_state(
                                "Keypath", str(kp)
                            ).finish()

                        if vpn_node.rd is None and vpn_node.node_ie_profile:
                            node_ie_profile_id = vpn_node.node_ie_profile
                            node_ie_profile = service.ie_profiles.ie_profile[node_ie_profile_id]
                            if node_ie_profile.rd is None and (
                                len(vpn_node.vpn_targets.vpn_target) > 0
                                or len(node_ie_profile.vpn_targets.vpn_target) > 0
                            ):
                                raise UserErrorException(
                                    self.log, StatusCodes.MISSING_RD_ON_XE
                                ).set_context(
                                    "Missing route-distinguisher. RD is mandatory for XE"
                                ).add_state(
                                    "Endpoint", vpn_node.ne_id
                                ).add_state(
                                    "Keypath", str(kp)
                                ).finish()
                    if ("cisco-ios-cli" in TsdnUtils
                            .get_device_ned_id_from_dispatch_map(self, root, vpn_node.ne_id)) \
                            and vpn_node.node_ie_profile:
                        node_ie_profile_id = vpn_node.node_ie_profile
                        node_ie_profile = service.ie_profiles.ie_profile[node_ie_profile_id]
                        if node_ie_profile.srv6.exists():
                            raise UserErrorException(
                                self.log, StatusCodes.VALUE_NOT_SUPPORTED
                            ).set_context(
                                "The ie-profile contains SRv6 config,"
                                " which is not supported on XE devices"
                            ).add_state(
                                "vpn-node", vpn_node.ne_id
                            ).add_state(
                                "Keypath", str(kp)
                            ).finish()
                    for (vpn_network_access) in vpn_node.vpn_network_accesses.vpn_network_access:
                        split_str = split(r"(\d(.+)?)", vpn_network_access.port_id)

                        # Check if Bundle-Ether and if if-id is within range
                        if split_str[0] == "Bundle-Ether" and \
                                not (split_str[1].isdigit() or 0 < int(split_str[1]) < 65536):
                            raise UserErrorException(
                                self.log, StatusCodes.VALUE_NOT_SUPPORTED
                            ).set_context(
                                "Interface Validation",
                                "Bundle-Ether if-id must be " "in the range 1-65535",
                            ).add_state(
                                "vpn-node", vpn_node.ne_id
                            ).add_state(
                                "vpn-network-access id", vpn_network_access.id
                            ).finish()

                        # Check if BVI and if if-id is within range
                        if split_str[0] == "BVI" and \
                                not (split_str[1].isdigit() or 0 < int(split_str[1]) < 4294967296):
                            raise UserErrorException(
                                self.log, StatusCodes.VALUE_NOT_SUPPORTED
                            ).set_context(
                                "Interface Validation",
                                "BVI if-id must be in the" " range 1-4294967295",
                            ).add_state(
                                "vpn-node", vpn_node.ne_id
                            ).add_state(
                                "vpn-network-access id", vpn_network_access.id
                            ).finish()

                        if (len(vpn_network_access.routing_protocols.routing_protocol) != 0):
                            # There should only be a single bgp routing-protocol
                            routing_protocol = list(
                                vpn_network_access.routing_protocols.routing_protocol
                            )[0]
                            # Routing protocol must be bgp
                            if routing_protocol.type != "l3vpn-svc:bgp":
                                raise UserErrorException(
                                    self.log, StatusCodes.VALUE_NOT_SUPPORTED
                                ).set_context(
                                    "Invalid Type", "routing-protocol must be bgp type"
                                ).add_state(
                                    "vpn-node", vpn_node.ne_id
                                ).add_state(
                                    "vpn-network-access id", vpn_network_access.id
                                ).finish()
                            # multihop must be within range 1-255
                            if (routing_protocol.bgp.multihop is not None
                                    and not 0 < int(routing_protocol.bgp.multihop) < 256):
                                raise UserErrorException(
                                    self.log, StatusCodes.VALUE_NOT_SUPPORTED
                                ).set_context(
                                    "Invalid Value",
                                    "bgp -> multihop must be in range [1-255]",
                                ).add_state(
                                    "multihop", int(routing_protocol.bgp.multihop)
                                ).add_state(
                                    "vpn-node", vpn_node.ne_id
                                ).add_state(
                                    "vpn-network-access id", vpn_network_access.id
                                ).finish()
                            # peer-autonomous-system must be within range 1-4294967295
                            if (not 0 < int(routing_protocol.bgp.peer_autonomous_system)
                                    < 4294967296):
                                raise UserErrorException(
                                    self.log, StatusCodes.VALUE_NOT_SUPPORTED
                                ).set_context(
                                    "Invalid Value", "bgp -> peer-autonomous-system must "
                                    "be in range [1-4294967295]",
                                ).add_state(
                                    "peer-autonomous-system",
                                    int(routing_protocol.bgp.peer_autonomous_system),
                                ).add_state(
                                    "vpn-node", vpn_node.ne_id
                                ).add_state(
                                    "vpn-network-access id", vpn_network_access.id
                                ).finish()
                # validate service assurance
                service_path = f"/l3vpn-ntw/vpn-services/vpn-service{{{service.vpn_id}}}"
                if(not TsdnUtils.validate_assurance_data(service.service_assurance, service_path)):
                    raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                          .set_context("Invalid value", "preservation cannot be changed if "
                                       "monitoring state value is already 'disable' ").finish()

        except Exception as e:
            print_exc()
            proplist["VALIDATION_ERROR"] = str(e)

        return list(proplist.items())

    @Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.ietf_l3vpn_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        if op == NCS_SERVICE_UPDATE:
            # Mark updated components
            th = get_trans(root)
            service = get_node(th, kp)
            is_only_self_updated = False

            is_redeploy = self.diff_iterate_redeploy(th, service).is_redeploy

            # Define plan kp
            plan_kp = (f"/l3vpn-ntw/vpn-services/vpn-service-plan{{{service.vpn_id}}}")
            # Check if its a redeploy
            if not is_redeploy:
                postmod_iter = self.diff_iterate(th, service)
                updated_components = postmod_iter.updated_components
                # Check service level modification - service-assurance
                if postmod_iter.is_self_update and not updated_components:
                    is_only_self_updated = True

                if len(updated_components) > 0 or is_only_self_updated:
                    device_component_mapping = {}
                    device_cq_details = list()
                    # Trim and generate device -> component mapping
                    for component in updated_components:
                        # Component naming <DEVICE>_<VPN_NETWORK_ACCESS_ID>
                        device = component.rsplit("_", 1)[0]
                        component_list = device_component_mapping.get(device, set())
                        component_list.add(component)
                        device_component_mapping[device] = component_list
                    # Update plan
                    if th.exists(plan_kp):
                        plan = get_node(th, plan_kp).plan
                        if is_only_self_updated:
                            # get generic commit-queue settings
                            cq_enabled = TsdnUtils.is_cq_enabled_generic(self, root, th)
                            if cq_enabled:
                                plan.component[("ncs:self", "self")] \
                                    .state["ncs:ready"].status = "not-reached"
                            else:
                                update_state_when_timestamp(
                                    self, plan.component[("ncs:self", "self")].name,
                                    plan.component[("ncs:self", "self")].state["ncs:ready"],
                                    "due to no CQ")
                        else:
                            # Check updated_components for CQ enabled
                            device_cq_details = is_cq_enabled(self, root,
                                                              device_component_mapping.keys(),
                                                              th, Utils.is_lsa)
                        # Update vpn-node components
                        component_type = "ietf-l3vpn-ntw-nano-services:vpn-node"
                        for (device, cq_enabled) in device_cq_details:
                            # If device updated and CQ enabled, set ready state to not-reached
                            # for both device component and self
                            if cq_enabled:
                                for component in device_component_mapping[device]:
                                    plan.component[(component_type, component)] \
                                        .state["ncs:ready"].status = "not-reached"
                                plan.component[("ncs:self", "self")] \
                                    .state["ncs:ready"].status = "not-reached"
                            # If device updated and no CQ, update ready state timestamp
                            # for both device component and self
                            else:
                                for component in device_component_mapping[device]:
                                    update_state_when_timestamp(
                                        self, plan.component[(component_type, component)].name,
                                        plan.component[(component_type, component)]
                                        .state["ncs:ready"], "due to no CQ")
                                update_state_when_timestamp(
                                    self, plan.component[("ncs:self", "self")].name,
                                    plan.component[("ncs:self", "self")].state["ncs:ready"],
                                    "due to no CQ")
                    else:
                        self.log.error(f"L3NM post_mod NO PLAN! : {plan_kp}")
            else:
                # Update self ready status to generate notifications
                # This is a workaround to update the L3NM plan when l3vpn-route-policy since
                # the route policy change is out of band for the service
                # route-policy change will always trigger a redeploy
                if th.exists(plan_kp):
                    l3vpn_plan_kp = ("/cisco-flat-L3vpn-fp:flat-L3vpn-plan"
                                     f"{{L3NM-{service.vpn_id}-internal}}")
                    if th.exists(l3vpn_plan_kp):
                        l3nm_plan = get_node(th, plan_kp).plan
                        l3vpn_plan = get_node(th, l3vpn_plan_kp).plan
                        update_components = []
                        for l3nm_component in l3nm_plan.component:
                            if str(l3nm_component.name) != "self":
                                component_key = ("cisco-flat-L3vpn-fp-nano-plan-"
                                                 "services:endpoint", l3nm_component.name)
                                l3vpn_component = l3vpn_plan.component[component_key]
                                # Compare static config indicator values and mark components
                                # for update
                                if str(l3nm_component.static_config_redeploy_indicator) != \
                                   str(l3vpn_component.static_config_redeploy_indicator):
                                    # Mark component to be updated
                                    update_components.append(l3nm_component)
                                    l3nm_component.static_config_redeploy_indicator = \
                                        l3vpn_component.static_config_redeploy_indicator

                        # Add self component if we are updating endpoint component
                        if update_components:
                            update_components.append(l3nm_plan.component[("ncs:self", "self")])
                        # Update component ready timestamps
                        for update_component in update_components:
                            ready_state_node = update_component.state["ncs:ready"]
                            update_state_when_timestamp(self, update_component.name,
                                                        ready_state_node, "due to static config"
                                                                          "redeploy indicator")

    @staticmethod
    def diff_iterate_redeploy(th, service) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Check if redeploy
            # /l3vpn-ntw:l3vpn-ntw/vpn-services/vpn-service{0-65008740}/private/re-deploy-counter
            self.is_redeploy = True
            return ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter, is_redeploy=False)
        th.keypath_diff_iterate(diff_iter, 0, "/l3vpn-ntw:l3vpn-ntw/"
                                f"vpn-services/vpn-service{{{service.vpn_id}}}/private/"
                                "re-deploy-counter")

        return diff_iter

    @staticmethod
    def diff_iterate(th, service) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            if len(keypath) < 4:
                if len(keypath) == 3 and str(keypath[-3]) == "service-assurance":
                    self.is_self_update = True
                return ITER_RECURSE
            elif len(keypath) == 4 and str(keypath[-3]) == "endpoint":
                # EX LSA and non LSA site level modification kp:
                #       /cisco-flat-L3vpn-fp:flat-L3vpn{L3NM-0-65008740-internal}/endpoint{PIOSXR-0_25}
                self.updated_components.append(str(keypath[-4][0]))
            elif len(keypath) == 4 and str(keypath[-3]) != "private":
                # EX service level modification kp:
                #       /cisco-flat-L3vpn-fp:flat-L3vpn{L3NM-vpn-id-internal}
                #       /custom-template{CT-CLI-banner}
                for vpn_node in self.service.vpn_nodes.vpn_node:
                    for (vpn_network_access) in \
                            vpn_node.vpn_network_accesses.vpn_network_access:
                        # Marking every component as updated
                        if (f"{vpn_node.ne_id}_{vpn_network_access.id}"
                                not in self.updated_components):
                            self.updated_components\
                                .append(f"{vpn_node.ne_id}_{vpn_network_access.id}")
                return ITER_STOP
            elif len(keypath) >= 4 and str(keypath[-3]) == "private":
                return ITER_CONTINUE
            return ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter,
                                       service=service,
                                       updated_components=[],
                                       is_self_update=False)
        th.keypath_diff_iterate(diff_iter, 0, "/cisco-flat-L3vpn-fp:flat-L3vpn"
                                f"{{L3NM-{service.vpn_id}-internal}}")

        return diff_iter


class IETFL3vpnNMSelfCallback(NanoService):
    """
    NanoService callback handler for ietf vpn-service-plan
    """

    @NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_l3vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            comp_vars = dict(comp_vars)
            new_opaque = opaque
            if state == "ietf-l3vpn-ntw-nano-services:config-apply":
                new_opaque = self._create_config_apply(tctx, root, service, plan,
                                                       component, state, opaque, comp_vars)
            elif state == "ncs:ready":
                new_opaque = self._create_ready(tctx, root, service, plan,
                                                component, state, opaque, comp_vars)

            return new_opaque
        except Exception as e:
            print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    def _create_config_apply(self, tctx, root, service, plan,
                             component, state, opaque, comp_vars):
        # Initialize template
        template = Template(service)

        # Apply Service level L3NM -> L3VPN mapping template
        template.apply("ietf-l3nm-l3vpn-service-template")

    def _create_ready(self, tctx, root, service, plan,
                      component, state, opaque, comp_vars):
        # First sync any plan status codes from internal plan
        l3vpn_service_name = f"L3NM-{service.vpn_id}-internal"
        l3vpn_plan_path = root.cisco_flat_L3vpn_fp__flat_L3vpn_plan

        update_plan_status_codes(self, root, plan, l3vpn_plan_path, l3vpn_service_name)

        # Sync component status codes
        l3vpn_component_key = ("ncs:self", "self")
        update_component_status_codes(self, root, l3vpn_plan_path,
                                      l3vpn_service_name, plan.component[component],
                                      l3vpn_component_key)

        # Check if l3vpn plan has failed status
        if l3vpn_service_name in l3vpn_plan_path:
            l3vpn_plan = l3vpn_plan_path[l3vpn_service_name].plan

            if l3vpn_plan.failed:
                self.log.info(f"l3vpn plan has failed, marking plan as failed."
                              f" l3vpn Plan : {l3vpn_service_name}")
                # We must set the self state status to failed; otherwise, we will get
                # flip flop of plan.failed for each successive reactive-re-deploy.
                # This is because NSO manages plan.failed field.
                #
                # In short:
                #
                # 1. If plan.failed is set, NSO will remove the field overriding
                #    our TSDN service code  if none of the plan states are failed.
                # 2. If plan.failed is not set, NSO will ignore checking the plan
                #    and TSDN service code can run through setting plan.failed
                plan.component[component].state[state].status = "failed"
                plan.failed.create()
                if l3vpn_plan.error_info:
                    plan.error_info.create()
                    plan.error_info.message = l3vpn_plan.error_info.message

        # RT-42737 - This callback will not be needed once this RT is fixed.
        state_node = plan.component[component].state[state]
        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                if (plan_component.state["ncs:ready"].status == "failed"
                        or plan_component.state["ncs:init"].status == "failed"):
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready failed, "
                                  "setting self ready state to failed")
                    state_node.status = "failed"
                    return opaque
        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                if plan_component.state["ncs:ready"].status == "not-reached":
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready not-reached, "
                                  "setting self ready state to not-reached")
                    state_node.status = "not-reached"
                    return opaque

        return opaque


class IETFL3vpnNMVpnNodeCallback(NanoService):
    """
    NanoService callback handler for ietf vpn-service-plan
    """

    @NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_l3vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):

        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            comp_vars = dict(comp_vars)
            new_opaque = opaque

            if state == "ietf-l3vpn-ntw-nano-services:config-apply":
                new_opaque = self._create_config_apply(tctx, root, service, plan,
                                                       component, state, opaque, comp_vars)
            elif state == "ncs:ready":
                new_opaque = self._create_ready(tctx, root, service, plan,
                                                component, state, opaque, comp_vars)

            return new_opaque
        except Exception as e:
            print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    @NanoService.delete
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_l3vpn_servicepoint)
    def cb_nano_delete(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        new_opaque = opaque

        if state == "ncs:init":
            new_opaque = self._delete_init(tctx, root, service, plan,
                                           component, state, opaque, comp_vars)

        if new_opaque is None:
            return opaque

        return new_opaque

    def _create_ready(self, tctx, root, service, plan,
                      component, state, opaque, comp_vars):
        # Check internal plan endpoint component state and update external plan accordingly
        vpn_id = service.vpn_id
        internal_service_name = f"L3NM-{service.vpn_id}-internal"
        ne_id = comp_vars["NE_ID"]
        vpn_network_access_id = comp_vars["VPN_NETWORK_ACCESS_ID"]
        state_node = plan.component[component].state[state]

        # Get internal plan path and component key
        internal_plans = root.cisco_flat_L3vpn_fp__flat_L3vpn_plan
        component_key = ("cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
                         f"{ne_id}_{vpn_network_access_id}")

        # Check if internal service is initialized
        if (internal_service_name not in internal_plans
                or component_key not in internal_plans[internal_service_name].plan.component):
            self.log.info(f"Internal plan for {vpn_id} doesn't exist, ready is not-reached. "
                          f"Internal Plan : {internal_service_name}")
            state_node.status = "not-reached"
            return opaque

        # Get internal plan
        internal_plan = internal_plans[internal_service_name].plan

        # Get internal endpoint component
        internal_component = internal_plan.component[component_key]

        # Sync internal component status
        state_node.status = internal_component.state["ncs:ready"].status

        # Sync status codes from internal plan
        internal_component_key = ("cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
                                  f"{ne_id}_{vpn_network_access_id}")
        update_component_status_codes(self, root,
                                      internal_plans, internal_service_name,
                                      plan.component[component], internal_component_key)

    def _create_config_apply(self, tctx, root, service, plan,
                             component, state, opaque, comp_vars):
        # Gather and parse required service config
        ne_id = comp_vars["NE_ID"]
        vpn_network_access_id = comp_vars["VPN_NETWORK_ACCESS_ID"]
        vpn_node = service.vpn_nodes.vpn_node[ne_id]
        bgp_ipv4_neighbor = ""
        bgp_ipv6_neighbor = ""

        vpn_network_access = vpn_node.\
            vpn_network_accesses.vpn_network_access[vpn_network_access_id]
        split_str = split(r"(\d(.+)?)", vpn_network_access.port_id)
        vpn_node_if_type, vpn_node_if_id = split_str[0], split_str[1]
        if vpn_network_access.routing_protocols.routing_protocol:
            # There should be only one element in routing_protocol list
            routing_protocol = list(vpn_network_access.routing_protocols.routing_protocol)[0]
            # There can be ipv4 and ipv6 address family and corresponding neighbor (one each)
            for ip_addr in routing_protocol.bgp.neighbor:
                if type(ip_address(ip_addr)) == IPv4Address:
                    bgp_ipv4_neighbor = ip_addr
                else:
                    bgp_ipv6_neighbor = ip_addr

        # Initialize template
        template = Template(vpn_node)
        template_vars = TemplateVariables()

        # Populate template vars
        template_vars.add("VPN_ID", service.vpn_id)
        template_vars.add("VPN_NETWORK_ACCESS_ID", vpn_network_access_id)
        template_vars.add("IF_TYPE", vpn_node_if_type)
        template_vars.add("IF_ID", vpn_node_if_id)
        template_vars.add("IPV4_BGP_NEIGHBOR", bgp_ipv4_neighbor)
        template_vars.add("IPV6_BGP_NEIGHBOR", bgp_ipv6_neighbor)

        # Apply L3NM -> L3VPN mapping template to create L3VPN as stacked service
        template.apply("ietf-l3nm-vpn-node-endpoint-template", template_vars)

    def _delete_init(self, tctx, root, service, plan,
                     component, state, opaque, comp_vars):
        ne_id = comp_vars["NE_ID"]

        # Get internal plan path
        internal_plan_path = root.cisco_flat_L3vpn_fp__flat_L3vpn_plan
        internal_service_name = f"L3NM-{service.vpn_id}-internal"
        state_node = plan.component[component].state[state]

        # Get internal plan if it exists
        try:
            internal_plan = internal_plan_path[internal_service_name].plan
        except KeyError:
            self.log.info(f"No L3VPN internal plan for: {internal_service_name}")
            # Remove delete status-code-detail augmentation
            # as platform doesn't remove what is created in delete callback
            remove_status_code_detail(plan, component)
            return opaque

        # Check if endpoint still exists
        internal_component_key = ("cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
                                  component[1])
        if internal_component_key not in internal_plan.component:
            self.log.info(f"No internal component for: {internal_component_key}")
            return opaque

        # Otherwise, sync component state
        state_node.status = internal_plan.\
            component[internal_component_key].state["ncs:init"].status

        # Sync plan status codes from internal plan
        update_plan_status_codes(self, root, plan, internal_plan_path, internal_service_name)

        # Sync component status codes from internal plan
        internal_component_key = ("cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
                                  ne_id)
        update_component_status_codes(self, root,
                                      internal_plan_path, internal_service_name,
                                      plan.component[component], internal_component_key)

        return opaque
