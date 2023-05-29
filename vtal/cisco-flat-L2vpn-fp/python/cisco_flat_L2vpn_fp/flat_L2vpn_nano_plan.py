# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
from _ncs.dp import NCS_SERVICE_CREATE, NCS_SERVICE_UPDATE
from ncs.maapi import Maapi
from ncs import maagic

from resource_manager import id_allocator
from . import utils as Utils
import traceback
from ncs.maagic import get_node, get_trans
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import \
    UserErrorException, ResourceAllocationException
from lsa_utils_pkg.dmap import dm_utils as LsaUtils

from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from core_fp_common import instrumentation
import logging
import time


class FlatL2vpnValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.l2vpn_validation_callpoint, tctx, kp)


class FlatL2vpnCallBack(ncs.application.Service):
    is_redeploy = False

    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.l2vpn_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        opaque = dict(proplist)
        opaque["VALIDATION_ERROR"] = ""
        opaque["DRY_RUN"] = ""

        try:
            # Check commit parameters
            th = get_trans(root)
            params = th.get_params()

            # Check for commit parameter dry-run
            if (op == NCS_SERVICE_CREATE or op == NCS_SERVICE_UPDATE) and params.is_dry_run():
                # If dry-run, set flag in opaque so we can assign dummy values to
                # RM allocated values
                opaque["DRY_RUN"] = "True"

            # If op is create, validate status code mapping is loaded
            status_code_cfp = root.cfp_common_status_codes__status_codes.core_function_pack

            if op == _ncs.dp.NCS_SERVICE_CREATE and not status_code_cfp.exists("L2VPN"):
                raise UserErrorException(
                    self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                ).set_context(
                    "Status Code", "Missing L2VPN status code mapping"
                ).add_state(
                    "Keypath", str(kp)
                ).finish()

            if (op == NCS_SERVICE_CREATE) or (op == NCS_SERVICE_UPDATE):
                service = get_node(get_trans(root), kp)
                # tunnel-te association is only supported for p2p
                if service.service_type == "p2p":
                    local_site_ned_id = TsdnUtils.get_device_ned_id_from_dispatch_map(
                        self, root, service.flat_L2vpn_p2p.local_site.pe)
                    if "cisco-ios-cli-" in local_site_ned_id:
                        if (service.flat_L2vpn_p2p.local_site.mpls_local_label is None
                                or service.flat_L2vpn_p2p.local_site.mpls_remote_label is None):
                            raise UserErrorException(
                                self.log, StatusCodes.MISSING_INPUT
                            ).set_context(
                                "Missing input ", "MPLS labels for both remote and "
                                "local site are mandatory when configuring L2VPN on XE",
                            ).add_state(
                                "MPLS Local Label",
                                str(service.flat_L2vpn_p2p.local_site.mpls_local_label),
                            ).add_state(
                                "MPLS Remote Label",
                                str(service.flat_L2vpn_p2p.local_site.mpls_remote_label),
                            ).finish()

                    # If ietf-te-service is defined, validate it points to existing IETF-TE service
                    if isinstance(service.flat_L2vpn_p2p.local_site.mpls_remote_label,
                                  type(service.flat_L2vpn_p2p.local_site.mpls_local_label)):
                        opaque = self._validate_ietf_te_service_tunnel_te_id(root, kp, opaque)
                    else:
                        raise UserErrorException(
                            self.log, StatusCodes.MISSING_INPUT
                        ).set_context(
                            "Missing input ", "MPLS label must be defined "
                                              "for both remote and local site",
                        ).add_state(
                            "MPLS Local Label",
                            str(service.flat_L2vpn_p2p.local_site.mpls_local_label),
                        ).add_state(
                            "MPLS Remote Label",
                            str(service.flat_L2vpn_p2p.local_site.mpls_remote_label),
                        ).finish()

                # For evpn, validate that parent-rr is not set to PASS_ALL (conflicts with L3VPN)
                else:
                    if service.service_type == "evpn-vpws":
                        sites = [
                            service.flat_L2vpn_evpn_vpws.local_site,
                            service.flat_L2vpn_evpn_vpws.remote_site,
                        ]
                    else:  # evpn-multipoint
                        sites = service.flat_L2vpn_evpn_multipoint.site

                    for site in sites:
                        if (site and site.sr_te and site.sr_te.odn and
                                site.sr_te.odn.attach_point.parent_rr_route_policy == "PASS_ALL"):
                            raise UserErrorException(
                                self.log, StatusCodes.RP_INVALID_PARENT_POLICY
                            ).set_context(
                                "Service Config Validation",
                                "PASS_ALL is not allowed as parent route policy name",
                            ).add_state(
                                "Device", str(site.pe)
                            ).finish()

                # validate service assurance
                service_path = f"/cisco-flat-L2vpn-fp:flat-L2vpn{{{service.name}}}"
                if(not TsdnUtils.validate_assurance_data(service.service_assurance, service_path)):
                    raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                          .set_context("Invalid value", "preservation cannot be changed if "
                                       "monitoring state value is already 'disable' ").finish()
            self.log.info(f"L2VPN pre_mod external Opaque: {opaque}")

        except Exception as e:
            traceback.print_exc()
            opaque["VALIDATION_ERROR"] = str(e)

        return list(opaque.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.l2vpn_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        # If op is update, mark all components that will be updated with CQ/no-CQ
        if op == _ncs.dp.NCS_SERVICE_UPDATE:
            # Initialize vars
            th = ncs.maagic.get_trans(root)
            service = ncs.maagic.get_node(th, kp)
            is_only_self_updated = False

            local_site_pe = self._get_local_site_device(service)
            remote_site_pe = self._get_remote_site_device(service)
            site_list = self._get_sites(service)

            postmod_iter = self.diff_iterate(th, root, service, local_site_pe,
                                             remote_site_pe, site_list)
            updated_sites = postmod_iter.updated_sites
            if postmod_iter.is_self_update and not updated_sites:
                is_only_self_updated = True

            # Check if sites were updated
            if updated_sites or is_only_self_updated:
                # Define plan kp
                plan_kp = f"/flat-L2vpn-plan{{{service.name}}}/plan"

                # Update plan
                if th.exists(plan_kp):
                    plan = get_node(th, plan_kp)
                    device_cq_details = list()

                    if service.service_type == "evpn-multipoint" and not is_only_self_updated:
                        device_to_comp = dict()
                        pe_list = list()

                        for site_name in updated_sites:
                            pe = service.flat_L2vpn_evpn_multipoint.site[site_name].pe
                            pe_list.append(pe)

                            self._update_device_comp_map(device_to_comp, pe, site_name)
                    else:
                        pe_list = updated_sites

                    updated_components = []
                    if is_only_self_updated:
                        # get generic commit-queue settings
                        is_cq_enabled = TsdnUtils.is_cq_enabled_generic(self, root, th)
                        updated_components.append((("ncs:self", "self"), is_cq_enabled))
                    else:
                        # Check if CQ enabled or not for updated head-ends
                        device_cq_details = TsdnUtils.is_cq_enabled(self, root, pe_list,
                                                                    th, Utils.is_lsa)

                    # Grab corresponding component key for each device
                    for (device, cq_enabled) in device_cq_details:
                        if device == local_site_pe:
                            component_key = ("cisco-flat-L2vpn-fp-nano-plan-services:local-site",
                                             device)
                            updated_components.append((component_key, cq_enabled))
                        elif device == remote_site_pe:
                            component_key = ("cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
                                             device)
                            updated_components.append((component_key, cq_enabled))
                        elif device in updated_sites:
                            for comp_name in device_to_comp[device]:
                                component_key = ("cisco-flat-L2vpn-fp-nano-plan-services:site",
                                                 comp_name)
                                updated_components.append((component_key, cq_enabled))
                        else:
                            continue
                        # Add self component
                        updated_components.append((("ncs:self", "self"), cq_enabled))

                    is_redeploy = self.diff_iterate_redeploy(th, service.name).is_redeploy

                    # Grab timestamp for static config redeploy indicator
                    curr_timestamp = str(round(time.time() * 1000))
                    # For each component, update plan with corresponding operation
                    for component_key, cq_enabled in updated_components:
                        component = plan.component[component_key]
                        state_node = component.state["ncs:ready"]
                        # Process
                        if cq_enabled:
                            state_node.status = "not-reached"
                        else:
                            TsdnUtils.update_state_when_timestamp(self, component, state_node,
                                                                  "due to no CQ")
                        # Update static config redeploy indicator
                        if is_redeploy and str(component.name) != "self":
                            component.static_config_redeploy_indicator = curr_timestamp

    @staticmethod
    def diff_iterate(th, root, service, local_site_pe, remote_site_pe,
                     site_list) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Iterate on service diffset to check for updated devices
            # Iterate on LSA service keypaths
            if Utils.is_lsa:
                # /ncs:devices/device{rfs-node-1}/config/cisco-flat-L2vpn-fp-internal-remote-site:
                # flat-L2vpn-internal-remote-site-service{L2vpn01 PIOSXR-1}/flat-L2vpn-p2p/pw-id
                if len(keypath) < 8:
                    if (len(keypath) == 7 and str(keypath[-7]) == "service-assurance"):
                        self.is_self_update = True
                    return ncs.ITER_RECURSE
                elif (str(keypath[-7]) != "private" and str(keypath[-6][1])
                        not in self.updated_sites):
                    self.updated_sites.append(str(keypath[-6][1]))
            # Iterate on non-LSA service keypaths
            else:
                # cisco-flat-L2vpn-fp-internal-local-site:flat-L2vpn-internal-local-site-service
                # {L2vpn-dynamic-02 PIOSXR-0}/flat-L2vpn-evpn-vpws/evi-id
                if len(keypath) < 4:
                    if (len(keypath) == 3 and str(keypath[-3]) == "service-assurance"):
                        self.is_self_update = True
                    return ncs.ITER_RECURSE
                elif (str(keypath[-3]) != "private" and str(keypath[-2][1])
                        not in self.updated_sites):
                    self.updated_sites.append(str(keypath[-2][1]))
            return ncs.ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter, updated_sites=[], is_self_update=False)

        if site_list:
            for site in site_list:
                internal_kp = FlatL2vpnCallBack._get_internal_site_service_kp(service, site)
                th.keypath_diff_iterate(diff_iter, 0, internal_kp)
        else:
            internal_local_kp = FlatL2vpnCallBack._get_internal_local_service_kp(root,
                                                                                 service,
                                                                                 local_site_pe)
            internal_remote_kp = FlatL2vpnCallBack._get_internal_remote_service_kp(root,
                                                                                   service,
                                                                                   remote_site_pe)

            # Iterate through diffset to find all updated internal services
            th.keypath_diff_iterate(diff_iter, 0, internal_local_kp)
            # Remote site is optional
            if internal_remote_kp:
                th.keypath_diff_iterate(diff_iter, 0, internal_remote_kp)

        return diff_iter

    @staticmethod
    def diff_iterate_redeploy(th, service_name) -> DiffIterateWrapper:
        # Check redeploy counter diffset
        def diter(self, keypath, op, oldv, newv):
            # Iterate on re-deploy-counter diffset to check for service re-deploy
            self.is_redeploy = True
            return ncs.ITER_STOP

        diff_iter = DiffIterateWrapper(diter, is_redeploy=False)
        redeploy_kp = f"/cisco-flat-L2vpn-fp:flat-L2vpn{{{service_name}}}/" \
                      "private/re-deploy-counter"

        th.keypath_diff_iterate(diff_iter, 0, redeploy_kp)

        return diff_iter

    @staticmethod
    def _get_local_site_device(service):
        # Return device of specified site
        if str(service.service_type) == "p2p":
            return str(service.flat_L2vpn_p2p.local_site.pe)
        else:
            return str(service.flat_L2vpn_evpn_vpws.local_site.pe)

    @staticmethod
    def _get_remote_site_device(service):
        # Return device of specified site
        if str(service.service_type) == "p2p":
            if service.flat_L2vpn_p2p.remote_site:
                return str(service.flat_L2vpn_p2p.remote_site.pe)
        else:
            if service.flat_L2vpn_evpn_vpws.remote_site:
                return str(service.flat_L2vpn_evpn_vpws.remote_site.pe)
        return ""

    @staticmethod
    def _get_site_device(service, site_name):
        return str(service.flat_L2vpn_evpn_multipoint.site[site_name].pe)

    @staticmethod
    def _get_sites(service):
        return [site.site_name for site in service.flat_L2vpn_evpn_multipoint.site]

    @staticmethod
    def _get_internal_local_service_kp(root, service, local_site_pe):
        # Return internal service keypath prefix for diff iterate keypath matching
        if Utils.is_lsa:
            rfs_device = LsaUtils.get_remote_nso(FlatL2vpnCallBack
                                                 ._get_local_site_device(service))
            return f"/ncs:devices/device{{{rfs_device}}}/config/cisco-flat-L2vpn-fp-" \
                "internal-local-site:flat-L2vpn-internal-local-site-" \
                f"service{{{service.name} {local_site_pe}}}"
        else:
            return "/cisco-flat-L2vpn-fp-internal-local-site:flat-L2vpn-internal-local-site-" \
                f"service{{{service.name} {local_site_pe}}}"

    @staticmethod
    def _get_internal_remote_service_kp(root, service, remote_site_pe):
        # Return internal service keypath prefix for diff iterate keypath matching
        if Utils.is_lsa:
            if remote_site_pe:
                rfs_device = LsaUtils.get_remote_nso(FlatL2vpnCallBack
                                                     ._get_remote_site_device(service))
                return f"/ncs:devices/device{{{rfs_device}}}/config/cisco-flat-L2vpn-fp-" \
                       "internal-remote-site:flat-L2vpn-internal-remote-site-" \
                       f"service{{{service.name} {remote_site_pe}}}"
        else:
            if remote_site_pe:
                return "/cisco-flat-L2vpn-fp-internal-remote-site:flat-L2vpn-internal-remote-" \
                       f"site-service{{{service.name} {remote_site_pe}}}"
        return ""

    @staticmethod
    def _get_internal_site_service_kp(service, site_name):
        # Return internal service keypath prefix for diff iterate keypath matching
        if Utils.is_lsa:
            rfs_device = \
                LsaUtils.get_remote_nso(FlatL2vpnCallBack._get_site_device(service, site_name))
            return f"/ncs:devices/device{{{rfs_device}}}/config/cisco-flat-L2vpn-fp-" \
                f"internal-site:flat-L2vpn-internal-site-service{{{service.name} {site_name}}}"
        else:
            return "/cisco-flat-L2vpn-fp-internal-site:flat-L2vpn-internal-" \
                f"site-service{{{service.name} {site_name}}}"

    def _validate_ietf_te_service_tunnel_te_id(self, root, kp, opaque_dict):
        service = get_node(get_trans(root), kp)
        # Keys for opaque dict
        remote_key = "REMOTE_SITE_IETF_TE"
        local_key = "LOCAL_SITE_IETF_TE"
        # Add sites to list
        sites = [
            (service.flat_L2vpn_p2p.local_site, local_key),
            (service.flat_L2vpn_p2p.remote_site, remote_key),
        ]
        # Validate per site
        for site, site_key in sites:
            # Check if rsvp policy and ietf-te service cases are defined
            if (site.policy_type != "rsvp-te" or
                    site.rsvp_te.preferred_path.tunnel_te_id_source != "ietf-te-service"):
                # If not, cleanup previously cached values if they exist
                if site_key in opaque_dict:
                    if opaque_dict.get(local_key, "") != opaque_dict.get(remote_key, ""):
                        del opaque_dict[opaque_dict[site_key]]
                    del opaque_dict[site_key]
                continue
            ietf_te_service = str(site.rsvp_te.preferred_path.ietf_te_service)
            # Check if IETF-TE package exists in NSO
            try:
                root.te__te
            except Exception:
                # If IETF-TE doesnt exist but tunnel-te-id from ietf-te-service is cached, continue
                if site_key in opaque_dict and opaque_dict[site_key] == ietf_te_service:
                    continue
                # If first time initializing new ietf-te-service, raise Exception
                raise UserErrorException(
                    self.log, StatusCodes.MISSING_DEPENDENCY
                ).set_context(
                    "Service Config Validation",
                    "IETF-TE package does not exist for ietf-te-service",
                ).add_state(
                    "ietf-te-service", ietf_te_service
                ).finish()
            # Check if rsvp-service references a valid RSVP-TE service
            if ietf_te_service not in root.te__te.tunnels.tunnel:
                # If service doesnt exist but tunnel te id is cached, continue
                if ietf_te_service in opaque_dict:
                    continue
                else:
                    raise UserErrorException(
                        self.log, StatusCodes.INVALID_ASSOCIATION
                    ).set_context(
                        "Service Config Validation",
                        "ietf-te-service does not reference a valid IETF-TE "
                        "service instance",
                    ).add_state(
                        "ietf-te-service", ietf_te_service
                    ).finish()
            # Populate opaque with tunnel-te id and overwrite any existing definition
            if site_key in opaque_dict and opaque_dict[site_key] != ietf_te_service:
                del opaque_dict[opaque_dict[site_key]]
            opaque_dict[site_key] = ietf_te_service
            opaque_dict[ietf_te_service] = str(
                root.te__te.tunnels.tunnel[ietf_te_service].identifier
            )
        # Return opaque
        return opaque_dict

    def _update_device_comp_map(self, device_to_comp, device_name, comp_name):
        if device_to_comp.get(device_name):
            device_to_comp[device_name].append(comp_name)
        else:
            device_to_comp[device_name] = [comp_name]


class FlatL2vpnExternalSelfCallback(ncs.application.NanoService):
    """
    NanoService callback handler for flat-L2vpn-plan
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.l2vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            if state == "ncs:ready":
                new_opaque = self._create_ready(tctx, root, service, plan,
                                                component, state, opaque, comp_vars)
            if new_opaque is None:
                return opaque

            return new_opaque

        except Exception as e:
            traceback.print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    def _create_ready(self, _tctx, _root, _service, plan,
                      component, state, opaque, _comp_vars):
        # RT-42737 - ready never turns back to not-reached/failed
        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                for plan_state in plan_component.state:
                    if plan_state.status == "failed":
                        self.log.info(f"Component '{plan_component.name}' "
                                      f"state '{plan_state.name}' failed, "
                                      "setting self ready state to failed")
                        plan.component[component].state[state].status = "failed"
                        return opaque

        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                if plan_component.state["ncs:ready"].status == "not-reached":
                    self.log.info(f"Component '{plan_component.name}' "
                                  "state ncs:ready not-reached, "
                                  "setting self ready state to not-reached")
                    plan.component[component].state[state].status = "not-reached"
                    return opaque
        return opaque


class FlatL2vpnExternalSiteCallback(ncs.application.NanoService):
    """
    NanoService callback handler for flat-L2vpn plan site
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.l2vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        state_node = plan.component[component].state[state]
        try:
            new_opaque = None
            comp_vars = dict(comp_vars)

            if state == "cisco-flat-L2vpn-fp-nano-plan-services:config-apply":
                new_opaque = self._create_config_apply(tctx, root, service, plan,
                                                       component, state, opaque, comp_vars)
            elif state == "ncs:init":
                new_opaque = self._site_create_init(tctx, root, service, plan,
                                                    component, state, opaque, comp_vars)
            elif state == "ncs:ready":
                new_opaque = self._site_create_ready(tctx, root, service, plan,
                                                     component, state, opaque, comp_vars)
            if new_opaque is None:
                return opaque

            return new_opaque
        except ResourceAllocationException as e:
            state_node.status = "failed"
            plan.failed.create()
            plan.error_info.create()
            plan.error_info.message = f"Resource Allocation Failed : {e}"
            # Save status code to plan
            _, pe_name = self._get_site_and_pe_name(component, comp_vars)
            e.save_to_plan(plan, component, pe_name)
        except Exception as e:
            traceback.print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    @ncs.application.NanoService.delete
    @instrumentation.instrument_nano(logging.INFO, Utils.l2vpn_servicepoint)
    def cb_nano_delete(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        if state == "ncs:init":
            return self._site_delete_init(tctx, root, service, plan,
                                          component, state, opaque, comp_vars)
        return opaque

    def _site_delete_init(self, _tctx, root, service, plan,
                          component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        state_node = plan.component[component].state[state]
        service_name = comp_vars["NAME"]
        is_local_site, pe_name = self._get_site_and_pe_name(component, comp_vars)

        plan.component[component].pe = pe_name

        self.log.info(f"site_delete_init of L2VPN CFS for service: {service_name} {component[1]}")

        internal_plan_path = Utils.get_internal_plan_path(root, is_local_site)

        # Get internal plan if it exists
        try:
            internal_plan = internal_plan_path[service_name, component[1]].plan
        except KeyError:
            self.log.info(f"No L2VPN internal plan for: {service_name} {component[1]}")
            # Remove delete status-code-detail augmentation
            # as platform doesn't remove what is created in delete callback
            TsdnUtils.remove_status_code_detail(plan, component)
            return opaque

        # Check if zombie exists
        if Utils.is_lsa:
            internal_zombie_exists = internal_plan.zombie_exists
        else:
            if is_local_site is None:
                internal_zombie_service = ("/flat-L2vpn-internal-site-service"
                                           f"[name='{service_name}'][site-name='{component[1]}']")
            elif is_local_site:
                internal_zombie_service = ("/flat-L2vpn-internal-local-site-service"
                                           f"[name='{service_name}'][pe='{pe_name}']")
            else:
                internal_zombie_service = ("/flat-L2vpn-internal-remote-site-service"
                                           f"[name='{service_name}'][pe='{pe_name}']")

            internal_zombie_exists = root.ncs__zombies.service.exists(internal_zombie_service)

        # Delete Failure
        if internal_plan.failed and internal_zombie_exists:
            self.log.info(f"L2vpn Internal plan is failed during delete for: {component[1]}")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message, plan,
                                       component, self.log, pe_name)

        else:
            self.log.info("L2vpn Internal plan failure is from create or deletion"
                          f" in progress for: {pe_name}")
            # Failure might be from create failures:
            # mark the state reached & wait for zombie redeploy if things change in internal plan
            state_node.status = "reached"

        return opaque

    def _site_create_init(self, _tctx, root, service, plan,
                          component, state, opaque, comp_vars):

        comp_vars = dict(comp_vars)
        plan.component[component].pe = comp_vars['PE']
        # Applying kicker for l2vpn-route-policy changes redeploy
        if service.service_type == "evpn-vpws":
            if component[0] == "cisco-flat-L2vpn-fp-nano-plan-services:remote-site":
                site = service.flat_L2vpn_evpn_vpws.remote_site
                template_name = "cisco-flat-L2vpn-fp-remote-site-evpn-vpws-rp-kicker"
                site_type = "remote-site"
            else:
                site = service.flat_L2vpn_evpn_vpws.local_site
                template_name = "cisco-flat-L2vpn-fp-local-site-evpn-vpws-rp-kicker"
                site_type = "local-site"

            if (site.sr_te.exists() and site.sr_te.odn.exists()
                    and (site.sr_te.odn.route_policy is not None)):
                self.log.info(f"Applying kicker on l2vpn-route-policy {site.sr_te.odn.route_policy}"
                              f"changes for {site_type} {service.name}")
                template = ncs.template.Template(service)
                vars = ncs.template.Variables()
                vars.add("NAME", service.name)
                vars.add("RP_NAME", site.sr_te.odn.route_policy)
                template.apply(template_name, vars)
        elif service.service_type == "evpn-multipoint":
            site = service.flat_L2vpn_evpn_multipoint.site[comp_vars['SITE']]

            if (site.sr_te.exists() and site.sr_te.odn.exists()
                    and (site.sr_te.odn.route_policy is not None)):
                template_name = "cisco-flat-L2vpn-fp-local-evpn-multipoint-rp-kicker"
                self.log.info(
                    f"Applying kicker on l2vpn-route-policy {site.sr_te.odn.route_policy}"
                    f"changes for {site.site_name} {service.name}"
                )
                template = ncs.template.Template(service)
                vars = ncs.template.Variables()
                vars.add("NAME", service.name)
                vars.add("SITE", site.site_name)
                vars.add("RP_NAME", site.sr_te.odn.route_policy)
                template.apply(template_name, vars)

        return opaque

    def _create_config_apply(self, _tctx, root, service, plan,
                             component, state, opaque, comp_vars):
        if service.service_type == 'evpn-multipoint':
            return self._create_config_mp_apply(_tctx, root, service, plan,
                                                component, state, opaque, comp_vars)
        else:
            return self._create_config_ptp_apply(_tctx, root, service, plan,
                                                 component, state, opaque, comp_vars)

    def _create_config_ptp_apply(self, _tctx, root, service, plan,
                                 component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        is_local_site, pe_name = self._get_site_and_pe_name(component, comp_vars)

        # Convert opaque to dict
        opaque_dict = dict(opaque)

        # Get RSVP-TE tunnel-te id
        remote_key = "REMOTE_SITE_IETF_TE"
        local_key = "LOCAL_SITE_IETF_TE"
        tunnel_te_id = ""
        if is_local_site and local_key in opaque_dict:
            tunnel_te_id = opaque_dict[opaque_dict[local_key]]
        elif not is_local_site and remote_key in opaque_dict:
            tunnel_te_id = opaque_dict[opaque_dict[remote_key]]

        ready_allocations = []
        in_progress_allocations = []
        failed_allocations = []
        allocation_dict = {}
        ip_sla_values = {}
        ip_sla_ignore = ("jitter-sd", "jitter-ds", "jitter-two-way")
        sman_id = ""

        # Get node from running trans to access previous operational data
        m = Maapi()
        r_th = m.attach(_tctx)

        # Get the remote/local site
        if service.service_type == 'p2p':
            if is_local_site:
                site = service.flat_L2vpn_p2p.local_site
            else:
                site = service.flat_L2vpn_p2p.remote_site
        else:
            if is_local_site:
                site = service.flat_L2vpn_evpn_vpws.local_site
            else:
                site = service.flat_L2vpn_evpn_vpws.remote_site

        # Applies kicker to re-deploy service on y-1731 profile updates
        for y1731 in site.ethernet_service_oam.y_1731:
            for y1731_profile in y1731.y_1731_profile:
                template = ncs.template.Template(service)
                vars = ncs.template.Variables()
                vars.add("PROFILE_NAME", y1731_profile.name)
                template.apply("cisco-flat-L2vpn-fp-y1731-profile-kicker", vars)

        pe_is_xe = False

        # Get the PE NED ID
        pe_ned_id = TsdnUtils.get_device_ned_id_from_dispatch_map(
            self, root, site.pe)
        if pe_ned_id.startswith("cisco-ios-"):
            pe_is_xe = True

        # Get y-1731 list
        y_1731_list = site.ethernet_service_oam.y_1731

        if pe_is_xe:
            # Allocate IP SLA
            for y in y_1731_list:
                maid = str(y.maid)

                for local_profile in y.y_1731_profile:
                    alloc_prefix = '-'.join((str(local_profile.name), service.name))
                    profile = root.cisco_flat_L2vpn_fp__y_1731_profile[local_profile.name]

                    self.log.debug(f"profile-path {profile._path}")

                    p_type = str(profile.type)

                    if "delay" == p_type:
                        statistic_list = profile.delay_params.statistic
                    else:
                        statistic_list = profile.loss_params.statistic

                    for s in statistic_list:
                        s_type = str(s.type)
                        if s_type in ip_sla_ignore:
                            continue
                        self.log.debug(f"start s-list {profile.name}")
                        self.log.debug(f"s-list {maid}")
                        self.log.debug(f"s-list {p_type}")
                        self.log.debug(f"end s-list {s_type}")

                        allocation_dict[f"ip_sla_id-{maid},{profile.name},{p_type},{s_type}"] = \
                            (alloc_prefix + "-" + p_type + "-" + s_type, "ip-sla-pool")

                        # check for statistic-id oper data using running transaction
                        try:
                            r_statistic = maagic.get_node(r_th, local_profile.statistic[s_type])
                        except KeyError:
                            r_statistic = None

                        if r_statistic is not None:
                            ip_sla_values[maid + "," +
                                          profile.name + "," +
                                          p_type + "," +
                                          s_type] = r_statistic.statistic_id
                        else:
                            ip_sla_values[maid + "," +
                                          profile.name + "," +
                                          p_type + "," +
                                          s_type] = None
        else:
            # Allocate SMAN ID
            # Note: y-1731 max list size 1
            for y in y_1731_list:
                alloc_name = f"{service.name}-sman-id"
                allocation_dict[alloc_name] = (alloc_name, "sman-id-pool")

                alloc_data = y.sman_id_allocation_data

                try:
                    # If dry-run assign dummy value
                    if opaque_dict["DRY_RUN"]:
                        # Just use example value compatible with both types
                        sman_id = 12321
                    # Write stale value in case allocation is not read
                    elif str(y.id_type) == "icc-based":
                        sman_id = maagic.get_node(r_th, alloc_data.icc_based_id)
                    else:
                        sman_id = maagic.get_node(r_th, alloc_data.number_id)
                except KeyError:
                    self.log.debug("No previous SMAN ID oper-data")

        self._allocate_ids(_tctx, service, allocation_dict)
        self._is_allocations_ready(_tctx, root, allocation_dict,
                                   in_progress_allocations, failed_allocations,
                                   ready_allocations)

        # Populate IP SLA and SMAN ID config with ready allocations
        for allocation in ready_allocations:
            key = allocation[0].split("-", 1)[1]
            self.log.debug(allocation)
            self.log.debug(f"ready-alloc {key}")
            # Update SMAN ID
            if not pe_is_xe:
                # Set SMAN ID
                sman_id = allocation[2]
                # Update SMAN ID oper-data
                y = iter(site.ethernet_service_oam.y_1731).next()
                if str(y.id_type) == "icc-based":
                    y.sman_id_allocation_data.icc_based_id = sman_id
                else:
                    y.sman_id_allocation_data.number_id = sman_id
            # Update IP SLA
            else:
                ip_sla_values[key] = allocation[2]

        create_vars = ncs.template.Variables()
        create_vars.add("SERVICE_NAME", service.name)
        create_vars.add("PE", pe_name)
        create_vars.add("TUNNEL_TE_ID", tunnel_te_id)
        if Utils.is_lsa:
            create_vars.add("RFS_NODE", LsaUtils.get_remote_nso(pe_name))
        self._create_internal_service(service, pe_name, is_local_site, create_vars)
        if len(failed_allocations) > 0:
            self.log.error(f"failed_allocations : {failed_allocations}")
            raise ResourceAllocationException(self.log, StatusCodes.ALLOCATION_FAILED) \
                .set_context("Resource Allocation Failed",
                             f"({''.join(str(failed_allocations))})") \
                .add_state("Service", service.name).finish()

        # Check if all allocations are ready or dry-run
        if len(allocation_dict) == len(ready_allocations) or opaque_dict["DRY_RUN"]:
            create_vars.add("SMAN_ID", sman_id)
            self._create_ethernet_service_oam(root, service, pe_name, is_local_site, ip_sla_values,
                                              pe_is_xe, site, opaque_dict, create_vars)
        else:
            # If allocations not ready, mark state as not reached
            plan.component[component].state[state].status = "not-reached"
        return opaque

    def _create_config_mp_apply(self, _tctx, root, service, plan,
                                component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)

        site = comp_vars["SITE"]

        # Initialize template
        template = ncs.template.Template(service)
        template_vars = ncs.template.Variables()
        template_vars.add("SERVICE_NAME", service.name)
        template_vars.add("SITE", site)

        # Determine if LSA deploy
        if Utils.is_lsa:
            # Get access_pe
            pe = service.flat_L2vpn_evpn_multipoint.site[site].pe
            # Get RFS node from dispatch-map
            template_vars.add("RFS_NODE", LsaUtils.get_remote_nso(pe))

        # Invoke cisco-flat-L2vpn-fp-internal-site or cisco-flat-L2vpn-fp-internal-ned template
        template.apply("cisco-flat-L2vpn-fp-copy-internal-site", template_vars)

        return opaque

    def _allocate_ids(self, tctx, service, allocation_dict):
        for suffix in allocation_dict:
            try:
                alloc_data = allocation_dict[suffix]
                self.log.info(f"Requesting id-allocation with alloc_name:{alloc_data[0]}, "
                              f"user:{tctx.username}, pool_name:{alloc_data[1]}")
                id_allocator.id_request(service,
                                        "/cisco-flat-L2vpn-fp:flat-L2vpn[name='"
                                        + service.name + "']",
                                        tctx.username,
                                        alloc_data[1],  # pool_name
                                        alloc_data[0],  # alloc_name
                                        False)
            except Exception as e:
                raise ResourceAllocationException(self.log, StatusCodes.ALLOCATION_REQUEST_FAILED,
                                                  str(e)) \
                    .set_context("Resource Allocation Error",
                                 f"({suffix}) allocation template failed to apply") \
                    .add_state("Service", service.name) \
                    .add_state("Pool Name", alloc_data[1]).add_state("Alloc Name", alloc_data[0]) \
                    .finish()

    def _is_allocations_ready(self, tctx, root, allocation_dict, in_progress_allocations,
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
                    ready_allocations.append((suffix, alloc_data[0], id))
            except LookupError as e:
                failed_allocations.append((alloc_data[0], str(e)))
                self.log.error(f"Allocation {alloc_data[0]} failed, Error: {str(e)}")

    def _create_internal_service(self, l2vpn_service, pe, is_local_site, create_vars):
        # Write to l2vpn internal using base template
        self.log.debug(f"create L2vpn internal service: {l2vpn_service.name} "
                       f"pe: {pe}, "
                       f"is_local_site: {is_local_site}")
        template = ncs.template.Template(l2vpn_service)
        if is_local_site:
            template_name = "cisco-flat-L2vpn-fp-copy-internal-local-site"
        else:
            template_name = "cisco-flat-L2vpn-fp-copy-internal-remote-site"
        template.apply(template_name, create_vars)

    def _create_ethernet_service_oam(self, root, l2vpn_service, pe, is_local_site, ip_sla_values,
                                     pe_is_xe, site, opaque, create_vars):
        self.log.debug(f"create L2vpn internal ethernet service oam: {l2vpn_service.name} "
                       f"pe: {pe}, "
                       f"is_local_site:{is_local_site}")
        template = ncs.template.Template(l2vpn_service)
        if is_local_site:
            template_name = "cisco-flat-L2vpn-fp-copy-internal-local-site-ethernet-service-oam"
        else:
            template_name = "cisco-flat-L2vpn-fp-copy-internal-remote-site-ethernet-service-oam"

        template.apply(template_name, create_vars)

        # Write to l2vpn internal directly
        if pe_is_xe:
            # Apply Y-1731 config to the internal service for XE devices
            self._apply_ip_sla_config(root, l2vpn_service, pe, is_local_site,
                                      ip_sla_values, site, opaque)

    def _apply_ip_sla_config(self, root, l2vpn_service, pe, is_local_site,
                             ip_sla_values, site, opaque):
        self.log.info("Applying IP SLA config")
        if len(ip_sla_values) > 0:
            # Populate the IP SLA list with dummy values if dry-run
            if opaque["DRY_RUN"]:
                # dummy val: floor(2147483647 / 2) = 1073741823
                ip_sla_start = 1073741823
                for key in ip_sla_values.keys():
                    if ip_sla_values[key] is None:
                        ip_sla_values[key] = ip_sla_start
                        ip_sla_start += 1

            # # Get the internal service y-1731 list
            if is_local_site:
                if Utils.is_lsa:
                    y_1731_list_internal = root.ncs__devices \
                        .device[LsaUtils.get_remote_nso(pe)].config \
                        .flat_L2vpn_internal_local_site_service[(l2vpn_service.name, pe)] \
                        .flat_L2vpn_p2p.local_site.ethernet_service_oam.y_1731
                else:
                    y_1731_list_internal = root \
                        .flat_L2vpn_internal_local_site_service[(l2vpn_service.name, pe)] \
                        .flat_L2vpn_p2p.local_site.ethernet_service_oam.y_1731
            else:
                if Utils.is_lsa:
                    y_1731_list_internal = root.ncs__devices \
                        .device[LsaUtils.get_remote_nso(pe)].config \
                        .flat_L2vpn_internal_remote_site_service[(l2vpn_service.name, pe)] \
                        .flat_L2vpn_p2p.remote_site.ethernet_service_oam.y_1731
                else:
                    y_1731_list_internal = root \
                        .flat_L2vpn_internal_remote_site_service[(l2vpn_service.name, pe)] \
                        .flat_L2vpn_p2p.remote_site.ethernet_service_oam.y_1731

            y_1731_list = site.ethernet_service_oam.y_1731

            for key in ip_sla_values.keys():
                self.log.debug(f"ip_sla unpack {key}")
                maid, profile_name, p_type, s_type = key.split(",")

                # Update flat l2vpn statistic-id oper data
                y_1731_profile = y_1731_list[maid].y_1731_profile[profile_name]
                statistic = y_1731_profile.statistic.create(s_type)
                statistic.statistic_id = ip_sla_values[key]

                # Update internal l2vpn with statistic-id in configuration
                if "delay" == p_type:
                    y_1731_list_internal[maid].y_1731_profile[profile_name].delay_params \
                        .statistic[s_type].statistic_id = ip_sla_values[key]
                else:
                    y_1731_list_internal[maid].y_1731_profile[profile_name].loss_params \
                        .statistic[s_type].statistic_id = ip_sla_values[key]

    def _get_site_and_pe_name(self, component, comp_vars):
        if component[0] == "cisco-flat-L2vpn-fp-nano-plan-services:site":
            pe_name = comp_vars["PE"]
            is_local_site = None
        elif component[0] == "cisco-flat-L2vpn-fp-nano-plan-services:local-site":
            pe_name = comp_vars["LOCAL_SITE"]
            is_local_site = True
        else:
            pe_name = comp_vars["REMOTE_SITE"]
            is_local_site = False
        return is_local_site, pe_name

    def _site_create_ready(self, _tctx, root, service, plan,
                           component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        state_node = plan.component[component].state[state]

        is_local_site, pe_name = self._get_site_and_pe_name(component, comp_vars)

        internal_plan_path = Utils.get_internal_plan_path(root, is_local_site)
        if (service.name, component[1]) not in internal_plan_path:
            self.log.info(f"Internal plan for L2vpn {service.name} "
                          f"{component[1]} doesn't exist, ready is not-reached")
            state_node.status = "not-reached"
            return opaque

        internal_plan = internal_plan_path[service.name, component[1]].plan

        # First check if there is plan error in internal, set it
        if internal_plan.failed:
            self.log.info(f"Internal plan for L2vpn {service.name} "
                          f"{pe_name} is failed, marking ready is failed")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message, plan,
                                       component, self.log, pe_name)
            return opaque

        # Check if there is commit-queue with this local-site/remote-site
        if internal_plan.commit_queue:
            if len(internal_plan.commit_queue.queue_item) > 0:
                self.log.info(f"Internal plan for L2vpn {service.name} "
                              f"{pe_name} has pending queue-item, ready is not-reached")
                state_node.status = "not-reached"
                return opaque

        # Internal plan should also have self -> ready:reached if there is no failure or pending CQ
        # This is to handle the case where undeploy-no-networking is performed to recover from
        # create failures - in that scenario, internal plan details (component etc.)
        # would be wiped out but only plan entry would remain.
        if ("ncs:self", "self") not in internal_plan.component:
            state_node.status = "not-reached"

        return opaque
