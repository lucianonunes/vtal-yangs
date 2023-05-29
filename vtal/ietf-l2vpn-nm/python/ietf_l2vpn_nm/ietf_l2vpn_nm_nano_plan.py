from _ncs.dp import NCS_SERVICE_CREATE, NCS_SERVICE_UPDATE
from ncs.application import NanoService, Service
import ncs
from ncs import ITER_CONTINUE, ITER_RECURSE, ITER_STOP
from ncs.maagic import get_node, get_trans
from ncs.maapi import Maapi
from traceback import print_exc
import resource_manager.id_allocator as id_allocator
from cisco_tsdn_core_fp_common.status_codes.ietf_l2vpn_nm_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.ietf_l2vpn_nm_base_exception import \
    UserErrorException, ResourceAllocationException
from cisco_tsdn_core_fp_common.utils import update_plan_status_codes, \
    update_component_status_codes, validate_service, remove_status_code_detail, \
    is_cq_enabled, update_state_when_timestamp
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from . import utils as Utils
from core_fp_common import instrumentation
import logging


class IETFL2vpnNMValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        validate_service(self, Utils.ietf_l2vpn_validation_callpoint, tctx, kp)


class IETFL2vpnNMCallback(Service):
    @Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.ietf_l2vpn_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        opaque = dict(proplist)
        opaque["VALIDATION_ERROR"] = ""
        try:
            # Check commit parameters
            th = get_trans(root)
            params = th.get_params()

            # Check for commit parameter dry-run
            opaque["DRY_RUN"] = ""
            if op == NCS_SERVICE_CREATE and params.is_dry_run():
                # If dry-run, set flag in opaque so we can assign dummy values to evi-X
                opaque["DRY_RUN"] = "True"

            # Validate L2NM -> Flat L2VPN constraints
            if (op == NCS_SERVICE_CREATE or op == NCS_SERVICE_UPDATE):
                service = get_node(get_trans(root), kp)
                for vpn_node in service.vpn_nodes.vpn_node:
                    # Validate parent-rr is not PASS_ALL
                    te_mapping = vpn_node.te_service_mapping.te_mapping
                    if te_mapping.te == "odn" and \
                            te_mapping.odn.attach_point.parent_rr_route_policy == "PASS_ALL":
                        raise UserErrorException(self.log, StatusCodes.RP_INVALID_PARENT_POLICY) \
                            .set_context("Service Config Validation",
                                         "PASS_ALL is not allowed as parent route policy name")\
                            .add_state("Device", str(vpn_node.ne_id)).finish()
                    # Validate dot1q-interface -> dot1q -> c-vlan-id
                    for vpn_network_access in vpn_node.vpn_network_accesses.vpn_network_access:
                        connection = vpn_network_access.connection
                        if str(connection.encapsulation_type) != "vpn-common:untagged-int" and \
                                str(connection.
                                    dot1q_interface.l2_access_type) == "vpn-common:dot1q":
                            c_vlan_id = connection.dot1q_interface.dot1q.c_vlan_id
                            if c_vlan_id is not None and not 0 < c_vlan_id < 4095:
                                raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED)\
                                    .set_context("Invalid value", "dot1q -> c-vlan-id must be "
                                                                  "in range [1-4094]") \
                                    .add_state("c-vlan-id", c_vlan_id) \
                                    .add_state("vpn-id", service.vpn_id) \
                                    .add_state("vpn-node", str(vpn_node.ne_id)).finish()
                # validate service assurance
                service_path = f"/l2vpn-ntw/vpn-services/vpn-service{{{service.vpn_id}}}"
                if(not TsdnUtils.validate_assurance_data(service.service_assurance, service_path)):
                    raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                        .set_context("Invalid value", "preservation cannot be changed if "
                                     "monitoring state value is already 'disable' ").finish()

                is_evpn_pbb = self._validate_evpn_pbb(service)
                opaque['EVPN_PBB'] = str(is_evpn_pbb)

                if (op == NCS_SERVICE_CREATE):
                    # Validate status code mapping is loaded
                    status_code_cfp = root.cfp_common_status_codes__status_codes.core_function_pack
                    if not status_code_cfp.exists("IETF-L2NM"):
                        raise UserErrorException(self.log, StatusCodes.STATUS_CODE_NOT_LOADED) \
                            .set_context("Status Code", "Missing L2NM status code mapping") \
                            .add_state("Keypath", str(kp)).finish()

                    if not is_evpn_pbb:
                        self._assign_sites(service, opaque)

                if not is_evpn_pbb:
                    if (op == NCS_SERVICE_UPDATE):
                        local_site_changed = True
                        remote_site_changed = True
                        for node in service.vpn_nodes.vpn_node:
                            if opaque["L2NM_LOCAL_SITE"] == node.vpn_node_id:
                                local_site_changed = False
                            elif opaque["L2NM_REMOTE_SITE"] == node.vpn_node_id:
                                remote_site_changed = False

                        if local_site_changed and remote_site_changed:
                            # re-assign both local-site and remote-site
                            self.log.info("re-assign both L2NM LOCAL_SITE "
                                          "and L2NM_REMOTE_SITE node")
                            self._assign_sites(service, opaque)
                        elif local_site_changed:
                            # re-assign local-site
                            self._re_assign_site(service, opaque, "L2NM_LOCAL_SITE",
                                                 opaque["L2NM_REMOTE_SITE"])
                        elif remote_site_changed:
                            # re-assign remote-site
                            self._re_assign_site(service, opaque, "L2NM_REMOTE_SITE",
                                                 opaque["L2NM_LOCAL_SITE"])

                    self.log.info(f"Selected local-site and remote-site, Opaque: {opaque}")

                    # TODO: Circuit Style, ETA: TSDN 5.0
                    # # validate cs policy constraints
                    # policy_names = set()
                    # policy_types = set()
                    # for vpn_node in service.vpn_nodes.vpn_node:
                    #     policy_node = vpn_node.te_service_mapping.te_mapping.sr_policy
                    #     if policy_node:
                    #         policy_names.add(policy_node.policy)
                    #         policy_types.add(str(policy_node.policy_type))
                    #     else:
                    #         policy_names.add("None")
                    # if len(policy_types) > 1 or \
                    #         ("cs-policy" in policy_types and len(policy_names) > 1):
                    #     raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                    #         .set_context("Invalid value", "The policy-type must be the same for "
                    #                                      "both the vpn-nodes. If the policy-type "
                    #                                       "is cs-policy, the policy must also be "
                    #                                       "the same for both the vpn-nodes.") \
                    #         .add_state("policy-types", policy_types) \
                    #         .add_state("policies", policy_names) \
                    #         .finish()

                    # validate signaling-option for vpn-nodes
                    local_signaling_opt = self.\
                        _get_signaling_option(service, opaque.get("L2NM_LOCAL_SITE"))
                    remote_signaling_opt = self.\
                        _get_signaling_option(service, opaque.get("L2NM_REMOTE_SITE"))
                    if local_signaling_opt is not None and remote_signaling_opt is not None:
                        if local_signaling_opt != remote_signaling_opt:
                            raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                                .set_context("Invalid value", f"{service.vpn_id} signaling-options "
                                                              "should be same for both "
                                                              "vpn-nodes").finish()
                        # For t-ldp signaling option, validate vc-id
                        elif str(local_signaling_opt) == "vpn-common:t-ldp":
                            # Both vpn-nodes should have t-ldp signaling option
                            mpls_label_list = []
                            ned_id_list = []
                            for vpn_node in service.vpn_nodes.vpn_node:
                                for signaling_option in vpn_node.signaling_options:
                                    for ac_pw in signaling_option.t_ldp_pwe.ac_pw_list:
                                        mpls_label_list.append(ac_pw.mpls_label)
                                        ned_id_list.append(
                                            TsdnUtils.
                                            get_device_ned_id_from_dispatch_map(self, root,
                                                                                vpn_node.ne_id))
                                        if not str(ac_pw.vc_id).isnumeric() or \
                                                not 0 < int(ac_pw.vc_id) < 4294967296:
                                            raise UserErrorException(self.log,
                                                                     StatusCodes
                                                                     .VALUE_NOT_SUPPORTED) \
                                                .set_context("Invalid value",
                                                             "t-ldp -> ac-pw -> vc-id "
                                                             "must be an int within "
                                                             "range [1-4294967295]") \
                                                .add_state("vc-id", str(ac_pw.vc_id)) \
                                                .add_state("vpn-node", str(vpn_node.ne_id)).finish()
                            try:
                                if any("cisco-ios-cli" in ne_id for ne_id in ned_id_list):
                                    if mpls_label_list[0] is None or mpls_label_list[1] is None:
                                        raise UserErrorException(
                                            self.log, StatusCodes.MISSING_INPUT) \
                                            .set_context("Missing input ",
                                                         "MPLS labels for both remote and"
                                                         " local site are mandatory when"
                                                         " configuring L2VPN Tunnel on XE") \
                                            .add_state("MPLS Local Label", mpls_label_list[0]) \
                                            .add_state("MPLS Remote Label", mpls_label_list[1]) \
                                            .finish()

                                if not isinstance(mpls_label_list[0], type(mpls_label_list[1])):
                                    raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                                        .set_context("Missing input ",
                                                     "MPLS label must be defined "
                                                     "for both remote and local site") \
                                        .add_state("MPLS Local Label", mpls_label_list[0]) \
                                        .add_state("MPLS Remote Label", mpls_label_list[1]) \
                                        .finish()
                            except IndexError:
                                raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                                    .set_context("Missing input ",
                                                 "MPLS label must be defined "
                                                 "for both remote and local site") \
                                    .finish()
                    else:
                        if local_signaling_opt is None:
                            raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                                .set_context("Missing input ",
                                             "Local Site Signaling "
                                             "options must be defined") \
                                .add_state("Service Name", service.vpn_id) \
                                .add_state("vpn-node", opaque["L2NM_LOCAL_SITE"]) \
                                .add_state("ne-id",
                                           str(list(service.vpn_nodes.vpn_node)[0].ne_id)) \
                                .finish()
                        if remote_signaling_opt is None:
                            raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                                .set_context("Missing input ",
                                             "Remote Site Signaling "
                                             "options must be defined") \
                                .add_state("Service Name", service.vpn_id) \
                                .add_state("vpn-node", opaque["L2NM_REMOTE_SITE"]) \
                                .add_state("ne-id",
                                           str(list(service.vpn_nodes.vpn_node)[1].ne_id)) \
                                .finish()
                    # validate encapsulation-type for vpn-nodes
                    local_encap_type = self._get_encapsulation_type(service,
                                                                    opaque.get("L2NM_LOCAL_SITE"))
                    remote_encap_type = self._get_encapsulation_type(service,
                                                                     opaque.get("L2NM_REMOTE_SITE"))

                    if local_encap_type != remote_encap_type:
                        raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                            .set_context("Invalid value", f"{service.vpn_id} encapsulation-type "
                                                          "should be same for both "
                                                          "vpn-nodes").finish()
                else:
                    # Validate bridge-group is defined
                    if not service.bridge_group:
                        raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                            .set_context("Service Config Validation",
                                         "bridge-group is mandatory for evpn-pbb service type") \
                            .finish()

                # resource pools are mandatory for vpn_svc_type = evpn-bgp
                if service.vpn_svc_type == 'vpn-common:evpn-bgp':
                    # Check if allocation will occur
                    err_msg = None
                    if service.evi_id_choice == "auto-evi-id" and \
                            root.l2vpn_ntw__l2vpn_ntw.id_pools.evi_id_pool_name is None:
                        err_msg = "Resource pool evi-id-pool-name must be defined for " \
                                  "vpn-svc-type evpn-bgp and auto-evi-id"

                    if not is_evpn_pbb and (
                        (service.evi_source_choice == "auto-evi-source" or
                         service.evi_target_choice == "auto-evi-target") and
                            root.l2vpn_ntw__l2vpn_ntw.id_pools.evi_source_target_pool_name is None):
                        err_msg = "Resource pool evi-source-target-pool-name must be defined " \
                                  "for vpn-svc-type evpn-bgp and auto-evi-source or auto-evi-target"
                    if err_msg:
                        raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                            .set_context("Missing input ", err_msg) \
                            .finish()
        except Exception as e:
            print_exc()
            opaque["VALIDATION_ERROR"] = str(e)

        return list(opaque.items())

    @Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.ietf_l2vpn_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        if op == NCS_SERVICE_CREATE or op == NCS_SERVICE_UPDATE:
            proplist = dict(proplist)
            th = get_trans(root)
            service = get_node(th, kp)
            plan_kp = f"/l2vpn-ntw/vpn-services/vpn-service-plan{{{service.vpn_id}}}"
            is_evpn_pbb = (proplist.get('EVPN_PBB') == 'True')
            is_only_self_updated = False

            if not is_evpn_pbb:
                self._update_local_site_oper(th, plan_kp, proplist)

            if op == NCS_SERVICE_UPDATE:
                # Mark updated components
                if is_evpn_pbb:
                    updated_components = {"site": set()}
                else:
                    updated_components = {"local-site": False, "remote-site": False}

                is_redeploy = self.diff_iterate_redeploy(th, service).is_redeploy
                # If not redeploy, iterate through internal service diffset
                if not is_redeploy:
                    postmod_iter = self.diff_iterate(th, service, updated_components)
                    updated_components = postmod_iter.updated_components
                    # Check service level modification - service-assurance
                    if postmod_iter.is_self_update and not any(updated_components.values()):
                        is_only_self_updated = True
                # If re-deploy and plan exists, then update self ready and vpn-node timestamp
                # Note: This is to resolve an issue where if a referenced l2vpn-route-policy
                #       is updated L2NM plan will not be updated and no service-state-change
                #       notification is sent
                elif th.exists(plan_kp):
                    l2vpn_plan_kp = ("/cisco-flat-L2vpn-fp:flat-L2vpn-plan"
                                     f"{{L2NM-{service.vpn_id}-internal}}")
                    if th.exists(l2vpn_plan_kp):
                        # Compare static config indicator for each L2NM
                        # component with L2VPN component
                        l2vpn_plan = get_node(th, l2vpn_plan_kp).plan
                        l2nm_plan = get_node(th, plan_kp).plan
                        update_components = []
                        for l2nm_component in l2nm_plan.component:
                            # Generate l2vpn component key
                            if str(l2nm_component.name) == proplist.get("L2NM_REMOTE_SITE", ""):
                                # Define l2vpn remote site component key
                                l2vpn_site_component_key = ("cisco-flat-L2vpn-fp-nano-plan-"
                                                            "services:remote-site",
                                                            l2nm_component.name)
                            elif str(l2nm_component.name) == proplist.get("L2NM_LOCAL_SITE", ""):
                                # Define l2vpn local site component key
                                l2vpn_site_component_key = ("cisco-flat-L2vpn-fp-nano-plan-"
                                                            "services:local-site",
                                                            l2nm_component.name)
                            elif l2nm_component.type != 'ncs:self' and is_evpn_pbb:
                                l2vpn_site_component_key = ("cisco-flat-L2vpn-fp-nano-plan-"
                                                            "services:site",
                                                            l2nm_component.name)
                            else:
                                continue

                            # Grab l2vpn site component
                            l2vpn_site_component = l2vpn_plan.component[l2vpn_site_component_key]

                            # Compare static config indicator values and mark components for update
                            if str(l2nm_component.static_config_redeploy_indicator) != \
                                    str(l2vpn_site_component.static_config_redeploy_indicator):
                                # Mark component to be updated
                                update_components.append(l2nm_component)
                                # Update static config indicator
                                l2nm_component.static_config_redeploy_indicator = \
                                    l2vpn_site_component.static_config_redeploy_indicator

                        # Add self component if we are updating site components
                        if update_components:
                            update_components.append(l2nm_plan.component[("ncs:self", "self")])

                        # Update component ready timestamps
                        for update_component in update_components:
                            ready_state_node = update_component.state["ncs:ready"]
                            update_state_when_timestamp(self, update_component.name,
                                                        ready_state_node, "due to static config"
                                                        "redeploy indicator")

                # Check updated_components for CQ enabled
                if any(updated_components.values()) or is_only_self_updated:
                    # Populate updated_components with device
                    updated_devices = set()
                    cq_disabled_devices = set()
                    device_cq_details = list()

                    # mapping used between device and components
                    # there maybe multiple components associated with given device
                    device_to_comp = dict()

                    if updated_components.get("site"):
                        if updated_components["site"] is True:
                            for vpn_node in service.vpn_nodes.vpn_node:
                                device_name = vpn_node.vpn_node_id
                                updated_devices.add(device_name)

                                vpn_network_access = \
                                    vpn_node.vpn_network_accesses.vpn_network_access

                                for vpn_net_acc in vpn_network_access:
                                    comp_name = \
                                        Utils.get_evpn_pbb_comp_name(device_name, vpn_net_acc.id)
                                    self._update_device_comp_map(
                                        device_to_comp, device_name, comp_name
                                    )
                        else:
                            comps = get_node(th, plan_kp).plan.component
                            comp_type = "ietf-l2vpn-ntw-nano-services:vpn-node"

                            for comp_name in updated_components["site"]:
                                device_name = comps[(comp_type, comp_name)].pe

                                # device_name will be None if component was deleted
                                # ie. a vpn-network-access was removed
                                if device_name is not None:
                                    updated_devices.add(device_name)

                                self._update_device_comp_map(
                                    device_to_comp, device_name, comp_name
                                )
                    else:
                        if updated_components["local-site"]:
                            updated_devices.add(proplist["L2NM_LOCAL_SITE"])
                            self._update_device_comp_map(
                                device_to_comp,
                                proplist["L2NM_LOCAL_SITE"],
                                proplist["L2NM_LOCAL_SITE"]
                            )
                        if updated_components["remote-site"]:
                            updated_devices.add(proplist["L2NM_REMOTE_SITE"])
                            self._update_device_comp_map(
                                device_to_comp,
                                proplist["L2NM_REMOTE_SITE"],
                                proplist["L2NM_REMOTE_SITE"]
                            )

                    is_generic_cq_enabled = False
                    if is_only_self_updated:
                        # get generic commit-queue settings
                        is_generic_cq_enabled = TsdnUtils.is_cq_enabled_generic(self, root, th)
                    else:
                        # Check if CQ enabled or not for updated head-ends
                        device_cq_details = is_cq_enabled(self, root, updated_devices,
                                                          th, Utils.is_lsa)
                    # Remove all non CQ enabled devices
                    for (device, cq_enabled) in device_cq_details:
                        if not cq_enabled:
                            cq_disabled_devices.add(device)
                            updated_devices.discard(device)
                    # Update plan
                    if th.exists(plan_kp) and (updated_components or is_only_self_updated):
                        plan = get_node(th, plan_kp).plan
                        # Update vpn-node components
                        self_component = plan.component[("ncs:self", "self")]
                        self_ready_state_node = self_component.state["ncs:ready"]
                        # If device updated and no CQ, update self ready timestamp
                        if ((len(cq_disabled_devices) > 0 and len(updated_devices) == 0) or
                                (is_only_self_updated and not is_generic_cq_enabled)):
                            update_state_when_timestamp(self, self_component.name,
                                                        self_ready_state_node, "due to no CQ")
                        # If device updated and CQ enabled, set self ready to not-reached
                        else:
                            self_ready_state_node.status = "not-reached"

                        vpn_node_comp_type = "ietf-l2vpn-ntw-nano-services:vpn-node"
                        for device in cq_disabled_devices:
                            for comp_name in device_to_comp[device]:
                                vpn_node_component = plan.component[(vpn_node_comp_type, comp_name)]
                                vpn_node_ready_state_node = vpn_node_component.state["ncs:ready"]

                                update_state_when_timestamp(
                                    self, vpn_node_component.name,
                                    vpn_node_ready_state_node,
                                    "due to no CQ"
                                )

                        for device in updated_devices:
                            if device_to_comp.get(device):
                                for comp_name in device_to_comp[device]:
                                    plan.component[(vpn_node_comp_type, comp_name)].\
                                        state["ncs:ready"].status = "not-reached"
                            else:
                                plan.component[(vpn_node_comp_type, device)].\
                                    state["ncs:ready"].status = "not-reached"
                    else:
                        self.log.error(f"L2NM post_mod NO PLAN! : {plan_kp}")

    @staticmethod
    def diff_iterate_redeploy(th, service) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Iterate on re-deploy-counter diffset to check for service re-deploy
            self.is_redeploy = True
            return ncs.ITER_STOP

        redeploy_kp = ("/l2vpn-ntw:l2vpn-ntw/vpn-services/vpn-"
                       f"service{{{service.vpn_id}}}/private/"
                       "re-deploy-counter")

        # Check redeploy counter diffset
        diff_iter = DiffIterateWrapper(diter, is_redeploy=False)
        th.keypath_diff_iterate(diff_iter, 0, redeploy_kp)

        return diff_iter

    @staticmethod
    def diff_iterate(th, service, updated_components) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Iterate on service diffset to check for updated devices
            # Skip all L2VPN private paths and recurse paths until service config is reached
            if not len(keypath) > 3:
                if(len(keypath) == 3 and str(keypath[-3]) == "service-assurance"):
                    self.is_self_update = True
                return ITER_RECURSE
            elif str(keypath[-3]) == "private":
                return ITER_CONTINUE

            keypath_neg_4 = str(keypath[-4])

            # L2VPN site level modification
            # EX: /cisco-flat-L2vpn-fp:flat-L2vpn{L2NM-l2nm-p2p-internal}/flat-L2vpn-p2p
            #    /local-site/mtu
            if keypath_neg_4 in self.updated_components.keys():
                # Extract key_path0
                is_key_path0_tuple = isinstance(keypath[0], tuple)
                key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])
                # Verify if there is any common config that was modified
                if str(key_path0) in self.common_config:
                    self.updated_components = dict.fromkeys(iter(self.updated_components), True)
                    return ITER_STOP
                else:
                    # Set component as updated
                    if keypath_neg_4 == 'site':
                        self.updated_components[keypath_neg_4].add(str(keypath[0][0]))
                    else:
                        self.updated_components[keypath_neg_4] = True
                    return ITER_CONTINUE
            # L2VPN service level modification
            # EX: /cisco-flat-L2vpn-fp:flat-L2vpn{L2NM-l2nm-p2p-internal}
            #     /custom-template{CT-CLI-banner}
            else:
                # Set all components as updated
                self.updated_components = dict.fromkeys(iter(self.updated_components), True)
                return ITER_STOP

        common_config = {"evi-source", "evi-target", "xconnect-local-ip",
                         "mpls-local-label", "mpls-remote-label"}
        internal_service_kp = ("/cisco-flat-L2vpn-fp:flat-"
                               f"L2vpn{{L2NM-{service.vpn_id}-internal}}")

        diff_iter = DiffIterateWrapper(diter,
                                       common_config=common_config,
                                       updated_components=updated_components,
                                       is_self_update=False)
        th.keypath_diff_iterate(diff_iter, 0, internal_service_kp)

        return diff_iter

    def _get_signaling_option(self, service, vpn_node_id):
        for signaling_option in \
                service.vpn_nodes.vpn_node[vpn_node_id, vpn_node_id].signaling_options:
            return signaling_option.type

    def _get_encapsulation_type(self, service, vpn_node_id):
        for vpn_ntw_access in \
            service.vpn_nodes.vpn_node[vpn_node_id,
                                       vpn_node_id].vpn_network_accesses.vpn_network_access:
            return vpn_ntw_access.connection.encapsulation_type

    def _assign_sites(self, service, opaque):
        flip = True
        for node in service.vpn_nodes.vpn_node:
            if flip:
                opaque["L2NM_LOCAL_SITE"] = node.vpn_node_id
                flip = False
            else:
                opaque["L2NM_REMOTE_SITE"] = node.vpn_node_id

    def _re_assign_site(self, service, opaque, site_name, other_site_node):
        self.log.info(f"re-assign {site_name} node")
        for node in service.vpn_nodes.vpn_node:
            if other_site_node != node.vpn_node_id:
                opaque[site_name] = node.vpn_node_id
                break

    def _update_local_site_oper(self, th, plan_kp, proplist):
        if th.exists(plan_kp):
            vpn_service_plan = get_node(th, plan_kp)
            local_site = proplist.get("L2NM_LOCAL_SITE", "")

            if local_site not in vpn_service_plan.local_sites:
                vpn_service_plan.local_sites.create(local_site)

            self._cleanup_local_site_oper(vpn_service_plan)

    def _cleanup_local_site_oper(self, vpn_service_plan):
        for local_site in vpn_service_plan.local_sites:
            key = ("ietf-l2vpn-ntw-nano-services:vpn-node", local_site)

            if not vpn_service_plan.plan.component.exists(key):
                del vpn_service_plan.local_sites[local_site]

    def _validate_evpn_pbb(self, service):
        vpn_node_paths = list()
        vpn_nodes = service.vpn_nodes

        trans = get_trans(service)
        trans.xpath_eval(
            "vpn-node/signaling-options/evpn-bgp/type",
            lambda kp, v: vpn_node_paths.append(str(kp)) if v.as_pyval() == 1 else None,
            None,
            vpn_nodes._path
        )
        # Check if all nodes defined either have 'evpn-pbb' configured for all
        # or configured for none of them
        vpn_node_count = len(vpn_node_paths)
        if vpn_node_count == 0 or vpn_node_count == len(vpn_nodes.vpn_node):
            # vpn_node_count > 0 (true) if 'evpn-pbb' is configured, so
            # set to true; otherwise, false
            return vpn_node_count > 0
        else:
            raise UserErrorException(self.log, StatusCodes.MISSING_INPUT) \
                .set_context("Missing input ",
                             "Some vpn-nodes are configured with "
                             "signaling-options/evpn-bgp/type = 'evpn-pbb' "
                             "and some nodes are not. Make sure all vpn-nodes "
                             "are defined with matching signaling-options") \
                .add_state("Service Name", service.vpn_id) \
                .finish()

    def _update_device_comp_map(self, device_to_comp, device_name, comp_name):
        if device_to_comp.get(device_name):
            device_to_comp[device_name].append(comp_name)
        else:
            device_to_comp[device_name] = [comp_name]


class IETFL2vpnNMSelfCallback(NanoService):
    """
    NanoService callback handler for L2NM vpn-service-plan
    """
    @NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_l2vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            comp_vars = dict(comp_vars)

            if state == "ncs:ready":
                return self._create_ready(tctx, root, service, plan, component, state, opaque,
                                          comp_vars)

            return opaque
        except Exception as e:
            print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    def _create_ready(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        # RT-42737 - This callback will not be needed once this RT is fixed.
        state_node = plan.component[component].state[state]

        l2vpn_service_name = "L2NM-{}-internal".format(service.vpn_id)
        l2vpn_plan_path = root.cisco_flat_L2vpn_fp__flat_L2vpn_plan
        if l2vpn_plan_path.exists(l2vpn_service_name):
            l2vpn_plan = root.cisco_flat_L2vpn_fp__flat_L2vpn_plan[l2vpn_service_name].plan
            self_key = ("ncs:self", "self")
            if (l2vpn_plan.component.exists(self_key)
                and l2vpn_plan.component[self_key].state["ncs:ready"].status == "failed") \
                    or l2vpn_plan.failed:
                state_node.status = "failed"
                return opaque

        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                if plan_component.state["ncs:ready"].status == "failed" or\
                        plan_component.state["ncs:init"].status == "failed" or \
                        plan_component.state["ietf-l2vpn-ntw-nano-services:config-apply"].status \
                        == "failed":
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


class IETFL2vpnNMVpnNodeCallback(NanoService):
    """
    NanoService callback handler for L2NM vpn-service-plan
    """
    @NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_l2vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = opaque
            comp_vars = dict(comp_vars)
            state_node = plan.component[component].state[state]

            if state == "ncs:init":
                new_opaque = self._site_create_init(tctx, root, service, plan, component, state,
                                                    opaque, comp_vars)
            elif state == "ietf-l2vpn-ntw-nano-services:config-apply":
                new_opaque = self._site_create_config_apply(tctx, root, service, plan, component,
                                                            state, opaque, comp_vars)
            elif state == "ncs:ready":
                new_opaque = self._site_create_ready(tctx, root, service, plan, component, state,
                                                     opaque, comp_vars)

            if new_opaque is None:
                return opaque

            return new_opaque
        except ResourceAllocationException as e:
            state_node.status = "failed"
            plan.failed.create()
            plan.error_info.create()
            plan.error_info.message = f"Resource Allocation Failed : {e}"
            # Save status code to plan
            e.save_to_plan(plan, component)
        except Exception as e:
            print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
        return list(opaque_dict.items())

    @NanoService.delete
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_l2vpn_servicepoint)
    def cb_nano_delete(
            self, tctx, root, service, plan, component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)

        if state == "ncs:init":
            opaque = self._delete_init(
                tctx, root, service, plan, component, state, opaque, comp_vars)
        return opaque

    def _delete_init(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        state_node = plan.component[component].state[state]

        # Get L2vpn plan path
        l2vpn_service_name = f"L2NM-{service.vpn_id}-internal"
        l2vpn_plan_path = root.cisco_flat_L2vpn_fp__flat_L2vpn_plan

        # Get internal plan if it exists
        try:
            l2vpn_plan = l2vpn_plan_path[l2vpn_service_name].plan
        except KeyError:
            self.log.info(f"No L2VPN internal plan for: {l2vpn_service_name}")
            # Remove delete status-code-detail augmentation
            # as platform doesn't remove what is created in delete callback
            remove_status_code_detail(plan, component)
            return opaque

        # Sync plan status codes from flat-l2vpn-plan
        update_plan_status_codes(self, root, plan, l2vpn_plan_path, l2vpn_service_name)

        l2vpn_mp_site_comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:site",
                                  component[1])
        l2vpn_local_site_comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:local-site",
                                     component[1])
        l2vpn_remote_site_comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
                                      component[1])

        if l2vpn_mp_site_comp_key in l2vpn_plan.component:
            l2vpn_site_comp_key = l2vpn_mp_site_comp_key
            l2vpn_site_comp = l2vpn_plan.component[l2vpn_mp_site_comp_key]
        elif l2vpn_local_site_comp_key in l2vpn_plan.component:
            l2vpn_site_comp_key = l2vpn_local_site_comp_key
            l2vpn_site_comp = l2vpn_plan.component[l2vpn_local_site_comp_key]
        elif l2vpn_remote_site_comp_key in l2vpn_plan.component:
            l2vpn_site_comp_key = l2vpn_remote_site_comp_key
            l2vpn_site_comp = l2vpn_plan.component[l2vpn_remote_site_comp_key]
        else:
            self.log.info(f"No matching site component for: {component[1]}")
            return opaque

        # Otherwise, sync component state
        state_node.status = l2vpn_site_comp.state["ncs:init"].status

        update_component_status_codes(self, root, l2vpn_plan_path, l2vpn_service_name,
                                      plan.component[component], l2vpn_site_comp_key)
        if l2vpn_plan.failed:
            self.log.debug(f"flat-L2vpn-plan {l2vpn_service_name} is failed, "
                           "marking L2NM plan failed")
            plan.failed.create()
            if l2vpn_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = l2vpn_plan.error_info.message

        return opaque

    def _site_create_init(self, tctx, root, service, plan, component, state, opaque,
                          comp_vars):
        plan.component[component].pe = dict(comp_vars)["VPN_NODE"]

    def _site_create_ready(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        state_node = plan.component[component].state[state]

        # Get L2vpn plan path
        l2vpn_service_name = f"L2NM-{service.vpn_id}-internal"
        l2vpn_plan_path = root.cisco_flat_L2vpn_fp__flat_L2vpn_plan

        # Check if internal service is initialized
        if l2vpn_service_name not in l2vpn_plan_path:
            self.log.info(f"flat-L2vpn-plan doesn't exist for {l2vpn_service_name}")
            state_node.status = "not-reached"
            return opaque

        # Sync plan status codes from flat-l2vpn-plan
        update_plan_status_codes(self, root, plan, l2vpn_plan_path, l2vpn_service_name)

        props = dict(opaque)
        vpn_node_id = dict(comp_vars)["VPN_NODE"]

        if vpn_node_id == props.get("L2NM_LOCAL_SITE"):
            # Get L2vpn local-site component key
            l2vpn_site_comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:local-site",
                                   component[1])
            if l2vpn_site_comp_key not in \
                    l2vpn_plan_path[l2vpn_service_name].plan.component:
                self.log.info("flat-L2vpn-plan local-site component doesn't "
                              f"exist for {l2vpn_service_name} {component[1]}")
                state_node.status = "not-reached"
                return opaque
            # Get local-site component
            l2vpn_site_comp = l2vpn_plan_path[l2vpn_service_name].plan.\
                component[l2vpn_site_comp_key]

            self._update_1731_statistic_id(root, service, l2vpn_service_name, vpn_node_id)
            self._update_sman_id(root, service, l2vpn_service_name, vpn_node_id)
        elif vpn_node_id == props.get("L2NM_REMOTE_SITE"):
            # Check L2vpn remote-site component key
            l2vpn_site_comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
                                   component[1])
            if l2vpn_site_comp_key not in \
                    l2vpn_plan_path[l2vpn_service_name].plan.component:
                self.log.info("flat-L2vpn-plan remote-site component "
                              f"doesn't exist for {l2vpn_service_name} {component[1]}")
                state_node.status = "not-reached"
                return opaque
            # Get remote-site component
            l2vpn_site_comp = l2vpn_plan_path[l2vpn_service_name].plan.\
                component[l2vpn_site_comp_key]

            self._update_1731_statistic_id(root, service, l2vpn_service_name, vpn_node_id, False)
            self._update_sman_id(root, service, l2vpn_service_name, vpn_node_id, False)
        else:
            # Check L2vpn remote-site component key
            l2vpn_site_comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:site",
                                   component[1])
            if l2vpn_site_comp_key not in \
                    l2vpn_plan_path[l2vpn_service_name].plan.component:
                self.log.info("flat-L2vpn-plan remote-site component "
                              f"doesn't exist for {l2vpn_service_name} {component[1]}")
                state_node.status = "not-reached"
                return opaque
            # Get remote-site component
            l2vpn_site_comp = l2vpn_plan_path[l2vpn_service_name].plan.\
                component[l2vpn_site_comp_key]

            self._update_1731_statistic_id(root, service, l2vpn_service_name, vpn_node_id, False)
            self._update_sman_id(root, service, l2vpn_service_name, vpn_node_id, False)

        # Sync local-site/remote-site status
        state_node.status = l2vpn_site_comp.state["ncs:ready"].status

        update_component_status_codes(self, root, l2vpn_plan_path, l2vpn_service_name,
                                      plan.component[component], l2vpn_site_comp_key)

        l2vpn_plan = l2vpn_plan_path[l2vpn_service_name].plan
        if l2vpn_plan.failed:
            self.log.debug(f"flat-L2vpn-plan {l2vpn_service_name} is failed, "
                           "marking L2NM plan failed")
            plan.failed.create()

            # Set site ready status to failed if any state in component is failed
            for l2vpn_state in l2vpn_site_comp.state:
                if "failed" == l2vpn_state.status:
                    state_node.status = "failed"

            if l2vpn_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = l2vpn_plan.error_info.message

    def _site_create_config_apply(self, tctx, root, service, plan, component, state, opaque,
                                  comp_vars):
        ready_allocations = []
        in_progress_allocations = []
        failed_allocations = []
        is_evpn_bgp = False
        allocation_dict = {}
        opaque = dict(opaque)
        is_evpn_pbb = (opaque['EVPN_PBB'] == 'True')

        # id allocation from resource-pools
        if service.vpn_svc_type == 'vpn-common:evpn-bgp':
            is_evpn_bgp = True
            if not opaque["DRY_RUN"]:
                pools = root.l2vpn_ntw__l2vpn_ntw.id_pools

                if service.evi_id_choice == "auto-evi-id":
                    allocation_dict["evi_id"] = (service.vpn_id + "-" + "evi_id",
                                                 pools.evi_id_pool_name)
                if not is_evpn_pbb:
                    if service.evi_source_choice == "auto-evi-source":
                        allocation_dict["evi_source"] = (service.vpn_id + "-" + "evi_source",
                                                         pools.evi_source_target_pool_name)
                    if service.evi_target_choice == "auto-evi-target":
                        allocation_dict["evi_target"] = (service.vpn_id + "-" + "evi_target",
                                                         pools.evi_source_target_pool_name)

                self._allocate_ids(tctx, service, allocation_dict)
                self._is_allocations_ready(tctx, root, service, allocation_dict,
                                           in_progress_allocations, failed_allocations,
                                           ready_allocations)

        if len(failed_allocations) > 0:
            self.log.error(f"failed_allocations : {failed_allocations}")
            raise ResourceAllocationException(self.log, StatusCodes.ALLOCATION_FAILED) \
                .set_context("Resource Allocation Failed",
                             f"({''.join(str(failed_allocations))})") \
                .add_state("Service", service.vpn_id).finish()

        template = ncs.template.Template(service)
        variables = ncs.template.Variables()
        variables.add("L2NM_LOCAL_SITE", opaque.get("L2NM_LOCAL_SITE"))

        evi_id = ""
        evi_source = ""
        evi_target = ""

        if is_evpn_bgp is True:
            # Get node from running trans to access previous operational data
            m = Maapi()
            th = m.attach(tctx)
            r_service = get_node(th, service._path)

            # First assign manual evi values if defined
            is_service_create = True
            if service.evi_id_choice == "evi-id":
                evi_id = service.evi_id
            # If evi is auto allocated and dry-run, assign dummy value
            elif opaque["DRY_RUN"]:
                # evi-id 1..65534, dummy val : floor(65534/2) = 32767
                evi_id = "32767"
            # If auto allocation, write stale value in case allocation is not ready
            elif r_service.evi_allocation_data.evi_id:
                evi_id = r_service.evi_allocation_data.evi_id
                is_service_create = False

            if not is_evpn_pbb:
                if service.evi_source_choice == "evi-source":
                    evi_source = service.evi_source
                elif opaque["DRY_RUN"]:
                    # evi-source 1..16777215, dummy val : floor(16777215/2) = 8388607
                    evi_source = "8388607"
                elif r_service.evi_allocation_data.evi_source:
                    evi_source = r_service.evi_allocation_data.evi_source
                    is_service_create = False

                if service.evi_target_choice == "evi-target":
                    evi_target = service.evi_target
                elif opaque["DRY_RUN"]:
                    # evi-target 1..16777215, dummy val : ceiling(16777215/2) = 8388608
                    evi_target = "8388608"
                elif r_service.evi_allocation_data.evi_target:
                    evi_target = r_service.evi_allocation_data.evi_target
                    is_service_create = False

            # Check if all allocations are ready
            if len(allocation_dict) != len(ready_allocations):
                # If allocations not ready and service create, do not apply service template
                if is_service_create:
                    self.log.debug(
                        "L2NM allocation not ready for service create, waiting for RM")
                    return
                # If allocations not ready and service modification, mark state as not reached
                else:
                    plan.component[component].state[state].status = "not-reached"

            # Overwrite old evi value with allocation values
            if not is_evpn_pbb:
                for allocation in ready_allocations:
                    if allocation[0] == "EVI_ID":
                        evi_id = allocation[2]
                    elif allocation[0] == "EVI_SOURCE":
                        evi_source = allocation[2]
                    elif allocation[0] == "EVI_TARGET":
                        evi_target = allocation[2]
            else:
                for allocation in ready_allocations:
                    if allocation[0] == "EVI_ID":
                        evi_id = allocation[2]

            # Add service oper data for UI
            service.evi_allocation_data.evi_id = evi_id
            if not is_evpn_pbb:
                service.evi_allocation_data.evi_source = evi_source
                service.evi_allocation_data.evi_target = evi_target

        # Assign template variables for device config
        variables.add("EVI_ID", evi_id)
        if not is_evpn_pbb:
            variables.add("EVI_SOURCE", evi_source)
            variables.add("EVI_TARGET", evi_target)

            template.apply("ietf-l2nm-l2vpn-template", variables)
            self.log.info("Applied template ietf-l2nm-l2vpn-template")
        else:
            flat_L2_name = f'L2NM-{service.vpn_id}-internal'

            variables.add('SERVICE_NAME', flat_L2_name)
            variables.add('TOPOLOGY', self._translate_svc_topo(service.svc_topo))

            comp_vars = dict(comp_vars)
            vpn_node_key = (comp_vars["VPN_NODE"], comp_vars["NE_ID"])
            context = service.vpn_nodes.vpn_node[vpn_node_key] \
                .vpn_network_accesses.vpn_network_access[comp_vars["VPN_NETWORK_ACCESS_ID"]]
            template = ncs.template.Template(context)

            template.apply("ietf-l2nm-l2vpn-evpn-mp-template", variables)
            self.log.info("Applied template ietf-l2nm-l2vpn-evpn-mp-template")

    def _allocate_ids(self, tctx, service, allocation_dict):
        for suffix in allocation_dict:
            try:
                alloc_data = allocation_dict[suffix]
                self.log.info(f"Requesting id-allocation with alloc_name:{alloc_data[0]}, "
                              f"user:{tctx.username}, "
                              f"pool_name:{ alloc_data[1]}")
                id_allocator.id_request(service,
                                        "/l2vpn-ntw:l2vpn-ntw/vpn-services/"
                                        + "vpn-service[vpn-id='" + service.vpn_id + "']",
                                        tctx.username,
                                        alloc_data[1],  # pool_name
                                        alloc_data[0],  # alloc_name
                                        False)
            except Exception as e:
                raise ResourceAllocationException(self.log, StatusCodes.ALLOCATION_REQUEST_FAILED,
                                                  str(e)) \
                    .set_context("Resource Allocation Error",
                                 f"({suffix}) allocation template failed to apply") \
                    .add_state("Service", service.vpn_id) \
                    .add_state("Pool Name", alloc_data[1]).add_state("Alloc Name", alloc_data[0]) \
                    .finish()

    def _is_allocations_ready(self, tctx, root, service, allocation_dict, in_progress_allocations,
                              failed_allocations, ready_allocations):
        for suffix in allocation_dict:
            alloc_data = allocation_dict[suffix]
            try:
                id = id_allocator.id_read(tctx.username, root, alloc_data[1], alloc_data[0])
                if not id:
                    self.log.info(f"Allocation {alloc_data[0]} not ready")
                    in_progress_allocations.append((alloc_data[0], id))
                else:
                    self.log.info(f"Allocation {alloc_data[0]} is ready , id = {id}")
                    ready_allocations.append((suffix.upper(), alloc_data[0], id))
            except LookupError as e:
                failed_allocations.append((alloc_data[0], str(e)))
                self.log.error(f"Allocation {alloc_data[0]} failed, Error: {str(e)}")

    # pulls oper-data y-1731 statistic-id from flat l2vpn
    def _update_1731_statistic_id(self, root, service, l2vpn_service_name, vpn_node_id, local=True):
        pass
        # Uncomment the following if we re-add support for XE Y.1731
        # vpn_node_key = (vpn_node_id, vpn_node_id)
        # self.log.info(f"Checking for updates for y-1731-profile statistic-id for {vpn_node_id}")

        # l2vpn_service = root.cisco_flat_L2vpn_fp__flat_L2vpn[l2vpn_service_name]
        # # XE Y-1731 is currently only supported for p2p
        # if l2vpn_service.service_type == 'evpn-vpws':
        #     self.log.info("evpn service type is not supported "
        #                   + f"for y-1731-profile statistic-id update for {vpn_node_id}")
        #     return

        # l2nm_y1731_list = iter(service.vpn_nodes.vpn_node[vpn_node_key].vpn_network_accesses
        #                        .vpn_network_access).next().ethernet_service_oam.y_1731

        # for l2nm_y1731 in l2nm_y1731_list:
        #     if local:
        #         l2vpn_y1731 = l2vpn_service.flat_L2vpn_p2p.\
        #             local_site.ethernet_service_oam.y_1731[l2nm_y1731.maid]
        #     else:
        #         l2vpn_y1731 = l2vpn_service.flat_L2vpn_p2p.\
        #             remote_site.ethernet_service_oam.y_1731[l2nm_y1731.maid]

        #     for l2nm_y1731_profile in l2nm_y1731.y_1731_profile:
        #         l2vpn_y1731_profile = l2vpn_y1731.y_1731_profile[l2nm_y1731_profile.name]

        #         for l2_stat in l2vpn_y1731_profile.statistic:
        #             self.log.debug(f"Found statistic entry {l2_stat.type}")

        #             nm_stat = l2nm_y1731_profile.statistic.create(l2_stat.type)
        #             nm_stat.statistic_id = l2_stat.statistic_id

        #             self.log.debug(f"Updated statistic entry {l2_stat.type}")

        # self.log.info(f"Completed update check for y-1731-profile statistic-id for {vpn_node_id}")

    # Pull SMAN ID oper-data from Flat L2VPN
    def _update_sman_id(self, root, service, l2vpn_service_name, vpn_node_id, local=True):
        vpn_node_key = (vpn_node_id, vpn_node_id)
        self.log.info(f"Checking for updates for y-1731 sman-id for {vpn_node_id}")

        l2vpn_service = root.cisco_flat_L2vpn_fp__flat_L2vpn[l2vpn_service_name]

        l2nm_y1731_list = iter(service.vpn_nodes.vpn_node[vpn_node_key].vpn_network_accesses
                               .vpn_network_access).next().ethernet_service_oam.y_1731

        for l2nm_y1731 in l2nm_y1731_list:
            if l2vpn_service.service_type == 'evpn-vpws':
                if local:
                    l2vpn_y1731 = l2vpn_service.flat_L2vpn_evpn_vpws.\
                        local_site.ethernet_service_oam.y_1731[l2nm_y1731.maid]
                else:
                    l2vpn_y1731 = l2vpn_service.flat_L2vpn_evpn_vpws.\
                        remote_site.ethernet_service_oam.y_1731[l2nm_y1731.maid]
            else:
                if local:
                    l2vpn_y1731 = l2vpn_service.flat_L2vpn_p2p.\
                        local_site.ethernet_service_oam.y_1731[l2nm_y1731.maid]
                else:
                    l2vpn_y1731 = l2vpn_service.flat_L2vpn_p2p.\
                        remote_site.ethernet_service_oam.y_1731[l2nm_y1731.maid]

            # Update icc-based SMAN ID
            l2nm_y1731.sman_id_allocation_data.icc_based_id = l2vpn_y1731.sman_id_allocation_data.\
                icc_based_id
            # Update number SMAN ID
            l2nm_y1731.sman_id_allocation_data.number_id = l2vpn_y1731.sman_id_allocation_data. \
                number_id

        self.log.info(f"Completed update check for y-1731 sman-id for {vpn_node_id}")

    def _translate_svc_topo(self, svc_topo):
        if svc_topo == 'vpn-common:hub-spoke':
            return 'E-TREE'
        elif svc_topo == 'vpn-common:any-to-any':
            return 'E-LAN'
        elif svc_topo == 'vpn-common:custom':
            return 'CUSTOM'

    def _remove_rt_value_encoding(self, rt_value):
        r_split = rt_value.split(":")

        if len(r_split) == 3:
            return ":".join(r_split[1:])
        else:
            return ":".join(r_split)
