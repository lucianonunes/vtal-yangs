# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
from . import utils as Utils
import traceback
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import UserErrorException
from lsa_utils_pkg.dmap.dm_utils import get_remote_nso
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from core_fp_common import instrumentation
import logging
import time


class FlatL3vpnValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.l3vpn_validation_callpoint, tctx, kp)


class FlatL3vpnCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.l3vpn_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        opaque = dict(proplist)
        opaque["VALIDATION_ERROR"] = ""
        try:
            # If op is create, validate status code mapping is loaded
            status_code_cfp = root.cfp_common_status_codes__status_codes.core_function_pack
            if op == _ncs.dp.NCS_SERVICE_CREATE and not status_code_cfp.exists("L3VPN"):
                raise UserErrorException(
                    self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                ).set_context(
                    "Status Code", "Missing L3VPN status code mapping"
                ).add_state(
                    "Keypath", str(kp)
                ).finish()

            if op == _ncs.dp.NCS_SERVICE_UPDATE or op == _ncs.dp.NCS_SERVICE_CREATE:
                # Check for missing RD while trying to configure L3VPN on XE device
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                for endpoint in service.endpoint:
                    for address_family in endpoint.vrf.address_family:
                        if (
                            "cisco-ios-cli"
                            in TsdnUtils.get_device_ned_id_from_dispatch_map(
                                self, root, endpoint.access_pe
                            )
                            and endpoint.vrf.route_distinguisher is None
                            and len(address_family.vpn_target) > 0
                            and op == _ncs.dp.NCS_SERVICE_CREATE
                        ):
                            raise UserErrorException(
                                self.log, StatusCodes.MISSING_RD_ON_XE
                            ).set_context(
                                "Missing route-distinguisher." " RD is mandatory for XE"
                            ).add_state(
                                "Endpoint", endpoint.endpoint_name
                            ).add_state(
                                "Keypath", str(kp)
                            ).finish()
                # validate service assurance
                service_path = f"/cisco-flat-L3vpn-fp:flat-L3vpn{{{service.name}}}"
                if(not TsdnUtils.validate_assurance_data(service.service_assurance, service_path)):
                    raise UserErrorException(self.log, StatusCodes.VALUE_NOT_SUPPORTED) \
                          .set_context("Invalid value", "preservation cannot be changed if "
                                       "monitoring state value is already 'disable' ").finish()

        except Exception as e:
            traceback.print_exc()
            opaque["VALIDATION_ERROR"] = str(e)

        return list(opaque.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.l3vpn_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        if op == _ncs.dp.NCS_SERVICE_UPDATE:
            th = ncs.maagic.get_trans(root)
            service = ncs.maagic.get_node(th, kp)
            # Store {"endpoint": "access-pe"}
            endpoints = dict()
            is_only_self_updated = False
            for endpoint in service.endpoint:
                endpoints[endpoint.endpoint_name] = endpoint.access_pe

            if Utils.is_lsa:
                postmod_diter = self.diff_iterate_lsa(th, service, endpoints)
            else:
                postmod_diter = self.diff_iterate_non_lsa(th, service, endpoints)

            # Check if any device (access_pe) has been updated
            plan_kp = f"/flat-L3vpn-plan{{{service.name}}}"
            updated_endpoint = postmod_diter.updated_endpoints
            # Check service level modification - service-assurance
            if postmod_diter.is_self_update and not updated_endpoint:
                is_only_self_updated = True

            if (len(updated_endpoint.keys()) > 0 or is_only_self_updated) and th.exists(plan_kp):
                device_cq_details = list()
                plan = ncs.maagic.get_node(th, plan_kp).plan
                if is_only_self_updated:
                    # get generic commit-queue settings
                    cq_enabled = TsdnUtils.is_cq_enabled_generic(self, root, th)
                    if cq_enabled:
                        plan.component[("ncs:self", "self")] \
                            .state["ncs:ready"].status = "not-reached"
                    else:
                        TsdnUtils.update_state_when_timestamp(
                            self, plan.component[("ncs:self", "self")].name,
                            plan.component[("ncs:self", "self")].state["ncs:ready"],
                            "due to no CQ")
                else:
                    # Check if CQ enabled or not for updated head-ends
                    device_cq_details = TsdnUtils.is_cq_enabled(self, root,
                                                                updated_endpoint.keys(),
                                                                th, Utils.is_lsa)
                component_type = "cisco-flat-L3vpn-fp-nano-plan-services:endpoint"

                is_redeploy = self.diff_iterate_redeploy(th, service).is_redeploy

                # Grab timestamp for static config redeploy indicator
                curr_timestamp = str(round(time.time() * 1000))
                for (device, cq_enabled) in device_cq_details:
                    # If device updated and CQ enabled, set ready state to not-reached
                    # for both device component and self
                    if cq_enabled:
                        plan.component[("ncs:self", "self")] \
                            .state["ncs:ready"].status = "not-reached"
                    else:
                        TsdnUtils.update_state_when_timestamp(
                            self, plan.component[("ncs:self", "self")].name,
                            plan.component[("ncs:self", "self")].state["ncs:ready"],
                            "due to no CQ")
                    if updated_endpoint:
                        for endpoint in updated_endpoint[device]:
                            component = plan.component[(component_type, endpoint)]
                            if cq_enabled:
                                component.state["ncs:ready"].status = "not-reached"
                            # If device updated and no CQ, update ready state timestamp
                            # for both device component and self
                            else:
                                TsdnUtils.update_state_when_timestamp(
                                    self, component.name,
                                    component.state["ncs:ready"],
                                    "due to no CQ")
                            # Update static config redeploy indicator
                            if is_redeploy:
                                component.static_config_redeploy_indicator = curr_timestamp

    @staticmethod
    def diff_iterate_redeploy(th, service) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Iterate on re-deploy-counter diffset to check for service re-deploy
            self.is_redeploy = True
            return ncs.ITER_STOP

        redeploy_kp = f"/cisco-flat-L3vpn-fp:flat-L3vpn{{{service.name}}}/" \
                      "private/re-deploy-counter"
        diff_iter = DiffIterateWrapper(diter,
                                       is_redeploy=False)
        th.keypath_diff_iterate(diff_iter, 0, redeploy_kp)
        return diff_iter

    @staticmethod
    def diff_iterate_lsa(th, service, endpoints) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # LSA kp: /ncs:devices/device{rfs-node-1}/config/
            #         cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}
            # KP must be a certain length before we reach /internal
            # TODO: When RT-46207 is fixed, this needs to be reworked to ignore /private KPs
            if len(keypath) < 8:
                if len(keypath) == 7 and str(keypath[-7]) == "service-assurance":
                    self.is_self_update = True
                return ncs.ITER_RECURSE
            elif str(keypath[-7]) != "private":
                FlatL3vpnCallBack.update_endpoints(self, str(keypath[-6][1]))
            return ncs.ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter,
                                       service_name=service.name,
                                       endpoints=endpoints,
                                       updated_endpoints=dict(),
                                       is_self_update=False)
        th.keypath_diff_iterate(diff_iter, 0, "/ncs:devices/device")

        return diff_iter

    @staticmethod
    def diff_iterate_non_lsa(th, service, endpoints) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # non LSA kp: /cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}/endpoint{cli-0}
            #   aliter:   /cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}
            #             /custom-template{CT-CLI-banner}
            if len(keypath) <= 2:
                return ncs.ITER_RECURSE
            elif len(keypath) >= 3 and str(keypath[-3]) != "private":
                if len(keypath) == 3 and str(keypath[-3]) == "service-assurance":
                    self.is_self_update = True
                    return ncs.ITER_RECURSE
                if str(keypath[-2][0]) == self.service_name:
                    FlatL3vpnCallBack.update_endpoints(self, str(keypath[-2][1]))
            elif str(keypath[-3]) == "private":
                return ncs.ITER_CONTINUE
            return ncs.ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter,
                                       service_name=service.name,
                                       endpoints=endpoints,
                                       updated_endpoints=dict(),
                                       is_self_update=False)
        th.keypath_diff_iterate(diff_iter, 0, "/cisco-flat-L3vpn-fp-internal:flat-L3vpn")

        return diff_iter

    def update_endpoints(self, endpoint_name):
        service_endpoint = self.endpoints.get(endpoint_name)
        if service_endpoint and self.updated_endpoints.get(service_endpoint):
            self.updated_endpoints[service_endpoint].add(endpoint_name)
        elif service_endpoint:
            self.updated_endpoints[service_endpoint] = set((endpoint_name,))


class FlatL3vpnExternalSelfCallback(ncs.application.NanoService):
    """
    NanoService callback handler for flat-L3vpn-plan
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.l3vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):

        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None
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
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready "
                                  "not-reached, setting self ready state to not-reached")
                    state_node.status = "not-reached"
                    return opaque
        return opaque


class FlatL3vpnExternalEndpointCallback(ncs.application.NanoService):
    """
    NanoService callback handler for flat-L3vpn plan endpoint
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.l3vpn_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):

        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None
            comp_vars = dict(comp_vars)
            self.log.info(f"COMP_VARS: {comp_vars}")
            if state == "ncs:init":
                new_opaque = self._create_init(tctx, root, service, plan,
                                               component, state, opaque, comp_vars)
            if state == "cisco-flat-L3vpn-fp-nano-plan-services:config-apply":
                new_opaque = self._create_config_apply(tctx, root, service, plan,
                                                       component, state, opaque, comp_vars)
            elif state == "ncs:ready":
                new_opaque = self._create_ready(tctx, root, service, plan,
                                                component, state, opaque, comp_vars)
            if new_opaque is None:
                return opaque

            return new_opaque
        except Exception as e:
            traceback.print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    @ncs.application.NanoService.delete
    @instrumentation.instrument_nano(logging.INFO, Utils.l3vpn_servicepoint)
    def cb_nano_delete(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        new_opaque = None
        comp_vars = dict(comp_vars)

        if state == "ncs:init":
            new_opaque = self._delete_init(tctx, root, service, plan,
                                           component, state, opaque, comp_vars)
        if new_opaque is None:
            return opaque

        return new_opaque

    def _create_init(self, tctx, root, service, plan,
                     component, state, opaque, comp_vars):
        # Initialize template
        self.log.info("RP KICKER LOADING")
        template = ncs.template.Template(service)
        # Applying kicker for l3vpn-route-policy changes redeploy
        endpoint = service.endpoint[comp_vars["ENDPOINT"]]
        if endpoint.sr_te.exists():
            sr_te = endpoint.sr_te
            for route_policy in [sr_te.export_route_policy, sr_te.import_route_policy]:
                if route_policy is not None:
                    self.log.info("Applying endpoint kicker for l3vpn-route-policy "
                                  f"changes: {service.name} {endpoint.endpoint_name}")
                    vars = ncs.template.Variables()
                    vars.add("NAME", service.name)
                    vars.add("ENDPOINT", comp_vars["ENDPOINT"])
                    vars.add("RP_NAME", route_policy)
                    template.apply("cisco-flat-L3vpn-fp-endpoint-rp-kicker", vars)

    def _delete_init(self, tctx, root, service, plan,
                     component, state, opaque, comp_vars):

        endpoint = comp_vars["ENDPOINT"]
        state_node = plan.component[component].state[state]

        # Determine internal plan path based on LSA/Non-LSA deploy
        if Utils.is_lsa:
            internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L3vpn_plan
        else:
            internal_plan_path = (root.cisco_flat_L3vpn_fp_internal__flat_L3vpn_internal
                                  .cisco_flat_L3vpn_fp_internal__flat_L3vpn_plan)

        # Get internal plan if it exists
        try:
            internal_plan = internal_plan_path[service.name, endpoint].plan
        except KeyError:
            self.log.info(f"No L3VPN internal plan for: {service.name} {endpoint}")
            # Remove delete status-code-detail augmentation
            # as platform doesn't remove what is created in delete callback
            TsdnUtils.remove_status_code_detail(plan, component)
            return opaque

        # Check if internal service zombie exists
        if Utils.is_lsa:
            internal_zombie_exists = internal_plan.zombie_exists
        else:
            internal_service_path = ("/cisco-flat-L3vpn-fp-internal:flat-L3vpn"
                                     f"[name='{service.name}'][endpoint-name='{endpoint}']")
            internal_zombie_exists = root.ncs__zombies.service.exists(internal_service_path)

        self.log.info(f"Internal service zombie exists for {endpoint}: {internal_zombie_exists}")

        # Failed plan + zombie exists = delete failure
        if internal_plan.failed and internal_zombie_exists:
            self.log.info(f"Internal plan failed during delete for: {endpoint}")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message, plan,
                                       component, self.log, comp_vars["ACCESS_PE"])
            return opaque
        else:
            self.log.info("Internal plan failure is from create or "
                          f"deletion in progress for: {endpoint}")
            # Failure might be from create failures:
            # mark the state reached & wait for zombie redeploy if things change in internal plan
            state_node.status = "reached"
            return opaque

        return opaque

    def _create_config_apply(self, _tctx, root, service, plan,
                             component, state, opaque, comp_vars):

        endpoint = comp_vars["ENDPOINT"]

        # Initialize template
        template = ncs.template.Template(service)
        template_vars = ncs.template.Variables()
        template_vars.add("SERVICE_NAME", service.name)
        template_vars.add("ENDPOINT", endpoint)

        # Determine if LSA deploy
        if Utils.is_lsa:
            # Get access_pe
            access_pe = service.endpoint[endpoint].access_pe
            # Get RFS node from dispatch-map
            template_vars.add("RFS_NODE", get_remote_nso(access_pe))

        # Invoke cisco-flat-L3vpn-fp-internal or cisco-flat-L3vpn-fp-internal-ned template
        template.apply("cisco-flat-L3vpn-fp-copy-internal", template_vars)

    def _create_ready(self, _tctx, root, service, plan, component, state, opaque, comp_vars):

        endpoint = comp_vars["ENDPOINT"]
        state_node = plan.component[component].state[state]

        # Determine internal plan path based on LSA/Non-LSA deploy
        if Utils.is_lsa:
            internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L3vpn_plan
        else:
            internal_plan_path = (root.cisco_flat_L3vpn_fp_internal__flat_L3vpn_internal
                                  .cisco_flat_L3vpn_fp_internal__flat_L3vpn_plan)

        # Check if internal service is initialized
        if (service.name, endpoint) not in internal_plan_path:
            self.log.info(f"Internal plan for {service.name} {endpoint} "
                          "doesn't exist, ready is not-reached")
            state_node.status = "not-reached"
            return opaque

        # Get internal plan
        internal_plan = internal_plan_path[service.name, endpoint].plan

        # Check for plan errors
        if internal_plan.failed:
            self.log.info(f"Internal plan for {service.name} {endpoint} is failed, "
                          "marking ready is failed")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message, plan,
                                       component, self.log, comp_vars["ACCESS_PE"])
            return opaque

        # Check for endpoint in commit-queue
        if (internal_plan.commit_queue and len(internal_plan.commit_queue.queue_item) > 0):
            self.log.info(f"Internal plan for {service.name} {endpoint} "
                          "has pending queue-item, ready is not-reached")
            state_node.status = "not-reached"
            return opaque

        # Internal plan should also have self -> ready:reached if there is no failure or pending CQ
        # This is to handle the case where undeploy-no-networking is performed to recover from
        # create failures - in that scenario, internal plan details (component etc.)
        # would be wiped out but only plan entry would remain.
        if ("ncs:self", "self") not in internal_plan.component:
            state_node.status = "not-reached"
            return list(opaque.items())

        return opaque
