# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
import cisco_sr_te_cfp.utils as Utils
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
import traceback
from core_fp_common import instrumentation
import logging


class SRPolicyValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.policy_validation_callpoint, tctx, kp)


class SRPolicyCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.policy_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        opaque = dict(proplist)
        opaque["VALIDATION_ERROR"] = ""
        try:
            if (op == _ncs.dp.NCS_SERVICE_CREATE) or (op == _ncs.dp.NCS_SERVICE_UPDATE):
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                # If op is create, validate status code mapping is loaded
                status_code_cfp = root.cfp_common_status_codes__status_codes.core_function_pack
                if op == _ncs.dp.NCS_SERVICE_CREATE and not status_code_cfp.exists("SR"):
                    raise UserErrorException(
                        self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                    ).set_context(
                        "Status Code", "Missing SR status code mapping"
                    ).add_state(
                        "Keypath", str(kp)
                    ).finish()

                # metric-type hopcount is not supported for XE type devices.
                for head_end in service.head_end:
                    if ("cisco-ios-cli" in TsdnUtils.get_device_ned_id_from_dispatch_map(
                            self, root, head_end.name)):
                        for path in service.path:
                            if (path.dynamic.exists()
                                    and path.dynamic.metric_type == "hopcount"):
                                raise UserErrorException(
                                    self.log, StatusCodes.VALUE_NOT_SUPPORTED
                                ).set_context(
                                    "Metric-type validation",
                                    "hop-count is not a valid input "
                                    "for metric-type for XE devices",
                                ).add_state(
                                    "Service", service.name
                                ).add_state(
                                    "Device", head_end.name
                                ).add_state(
                                    "Path", path.preference
                                ).finish()
        except Exception as e:
            traceback.print_exc()
            opaque["VALIDATION_ERROR"] = str(e)
        self.log.info(f"SR Policy pre_mod external Opaque: {opaque}")
        return list(opaque.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.policy_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        if op == _ncs.dp.NCS_SERVICE_UPDATE:
            service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)

            # Note: for sr-te-policy the self.service.head_end max-elements = 1 and min-elements = 1
            for head_end in service.head_end:
                head_end = str(head_end.name)

            th = ncs.maagic.get_trans(root)
            internal_service_kp = "/cisco-sr-te-cfp-internal:sr-te" \
                                  "/cisco-sr-te-cfp-sr-policies-internal:policies/policy" \
                                  f"{{{service.name} {head_end}}}"
            if Utils.is_lsa:
                updated_head_ends = self.diff_iterate_lsa(th,
                                                          root,
                                                          head_end,
                                                          internal_service_kp).updated_head_ends
            else:
                updated_head_ends = self.diff_iterate_non_lsa(th,
                                                              internal_service_kp).updated_head_ends

            # Check if any device has been updated
            if updated_head_ends:
                # Check if CQ enabled or not for the head-end
                device_cq_details = TsdnUtils.is_cq_enabled(self, root, [head_end], th,
                                                            Utils.is_lsa)
                # Define plan kp
                plan_kp = "/cisco-sr-te-cfp:sr-te/" \
                          "cisco-sr-te-cfp-sr-policies:policies/" \
                          f"cisco-sr-te-cfp-sr-policies:policy-plan{{{service.name}}}"
                if th.exists(plan_kp):
                    plan = ncs.maagic.get_node(th, plan_kp).plan
                    component_type = "cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end"
                    # For sr-te-policy, there is only one device
                    device, cq_enabled = device_cq_details[0]
                    # If device updated and CQ enabled, set ready state to not-reached
                    # for both device component and self
                    if cq_enabled:
                        plan.component[(component_type, device)] \
                            .state["ncs:ready"].status = "not-reached"
                        plan.component[("ncs:self", "self")] \
                            .state["ncs:ready"].status = "not-reached"
                    # If device updated and no CQ, update ready state timestamp
                    # for both device component and self
                    else:
                        TsdnUtils.update_state_when_timestamp(
                            self, plan.component[(component_type, device)].name,
                            plan.component[(component_type, device)].state["ncs:ready"],
                            "due to no CQ")
                        TsdnUtils.update_state_when_timestamp(
                            self, plan.component[("ncs:self", "self")].name,
                            plan.component[("ncs:self", "self")].state["ncs:ready"],
                            "due to no CQ")

    @staticmethod
    def diff_iterate_lsa(th, root, head_end, internal_service_kp) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # LSA kp: /ncs:devices/device{rfs-node-1}/config/cisco-sr-te-cfp-internal:sr-te
            #         /cisco-sr-te-cfp-sr-policies-internal:policies/policy{SR-CLI-DYNAMIC PIOSXR-0}
            self.updated_head_ends = True
            return ncs.ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter, updated_head_ends=False)
        th.keypath_diff_iterate(diff_iter, 0,
                                "/ncs:devices/device"
                                f"{{{LsaUtils.get_remote_nso(head_end)}}}"
                                f"/config/{internal_service_kp}")
        return diff_iter

    @staticmethod
    def diff_iterate_non_lsa(th, internal_service_kp) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # non LSA kp: /cisco-sr-te-cfp-internal:sr-te
            #             /cisco-sr-te-cfp-sr-policies-internal:policies
            #             /policy{SR-CLI-DYNAMIC PIOSXR-0}/color{300}
            if len(keypath) <= 4:
                return ncs.ITER_RECURSE
            elif len(keypath) >= 5 and str(keypath[-5]) != "private":
                self.updated_head_ends = True
            elif str(keypath[-5]) == "private":
                return ncs.ITER_STOP
            return ncs.ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter, updated_head_ends=False)
        th.keypath_diff_iterate(diff_iter, 0, internal_service_kp)

        return diff_iter


class SRPolicyExternalSelfCallback(ncs.application.NanoService):
    """
    NanoService callback handler for sr-policy plan
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.policy_servicepoint)
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
            elif state == "ncs:init":
                new_opaque = self._create_init(tctx, root, service, plan,
                                               component, state, opaque, comp_vars)

            if new_opaque is None:
                return opaque

            return new_opaque

        except Exception as e:
            traceback.print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    def _create_init(self, _tctx, root, service, plan,
                     component, state, opaque, _comp_vars):
        # Applying kicker for sid-list changes redeploy
        vars = ncs.template.Variables()
        template = ncs.template.Template(service)
        self.log.info(f"Applying kicker template for sid-list changes: {service.name}")
        template.apply("cisco-sr-te-cfp-sr-te-policy-plan-init-create", vars)

        return opaque

    def _create_ready(self, _tctx, _root, _service, plan,
                      component, state, opaque, _comp_vars):
        # RT-42737 - This callback will not be needed once this RT is fixed.
        state_node = plan.component[component].state[state]
        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                if (plan_component.state["ncs:ready"].status == "failed"
                        or plan_component.state["ncs:init"].status == "failed"):
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready "
                                  "failed, setting self ready state to failed")
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


class SRPolicyExternalHeadEndCallback(ncs.application.NanoService):
    """
    NanoService callback handler for sr-policy plan head-end
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.policy_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None
            comp_vars = dict(comp_vars)

            if state == "cisco-sr-te-cfp-sr-policies-nano-plan-services:config-apply":
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
    @instrumentation.instrument_nano(logging.INFO, Utils.policy_servicepoint)
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

    def _delete_init(self, _tctx, root, service, plan,
                     component, state, opaque, comp_vars):

        comp_vars = dict(comp_vars)
        name = comp_vars["NAME"]
        head_end = comp_vars["HEAD_END"]
        state_node = plan.component[component].state[state]
        if Utils.is_lsa:
            internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_policy_plan
        else:
            internal_plan_path = (root.cisco_sr_te_cfp_internal__sr_te
                                  .cisco_sr_te_cfp_sr_policies_internal__policies
                                  .policy_plan)

        # Get internal plan if it exists
        try:
            internal_plan = internal_plan_path[name, head_end].plan
        except KeyError:
            self.log.info(f"No SR Policy internal plan for: {name} {head_end}")
            # Remove delete status-code-detail augmentation
            # as platform doesn't remove what is created in delete callback
            TsdnUtils.remove_status_code_detail(plan, component)
            return opaque

        # Check if zombie exists
        if Utils.is_lsa:
            internal_zombie_exists = internal_plan.zombie_exists
        else:
            internal_zombie_service = ("/cisco-sr-te-cfp-internal:sr-te/"
                                       "cisco-sr-te-cfp-sr-policies-internal:"
                                       f"policies/policy[name='{name}'][head-end='{head_end}']")
            internal_zombie_exists = root.ncs__zombies.service.exists(internal_zombie_service)

        self.log.info(f"Internal service zombie exists for {head_end}: {internal_zombie_exists}")

        # Delete Failure
        if internal_plan.failed and internal_zombie_exists:
            self.log.info(f"Internal plan is failed during delete for: {head_end}")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message,
                                       plan, component, self.log, head_end)
        else:
            self.log.info("Internal plan failure is from create or "
                          f"deletion in progress for: {head_end}")
            # Failure might be from create failures:
            # mark the state reached & wait for zombie redeploy if things change in internal plan
            state_node.status = "reached"
        return opaque

    def _create_config_apply(self, _tctx, root, service, plan,
                             component, state, opaque, comp_vars):

        comp_vars = dict(comp_vars)
        head_end = comp_vars["HEAD_END"]

        # Create head end services
        create_vars = ncs.template.Variables()
        template = ncs.template.Template(service)
        create_vars.add("POLICY_NAME", service.name)
        create_vars.add("HEAD_END", head_end)
        # Write to policy internal
        if Utils.is_lsa:
            create_vars.add("RFS_NODE", LsaUtils.get_remote_nso(head_end))
        template.apply("cisco-sr-te-cfp-sr-te-policy-copy-internal", create_vars)

        return opaque

    def _create_ready(self, _tctx, root, service, plan,
                      component, state, opaque, comp_vars):

        comp_vars = dict(comp_vars)
        head_end = comp_vars["HEAD_END"]
        state_node = plan.component[component].state[state]
        if Utils.is_lsa:
            internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_policy_plan
        else:
            internal_plan_path = (root.cisco_sr_te_cfp_internal__sr_te
                                  .cisco_sr_te_cfp_sr_policies_internal__policies
                                  .policy_plan)

        if (service.name, head_end) not in internal_plan_path:
            self.log.info(f"Internal plan for {service.name} {head_end} "
                          "doesn't exist, ready is not-reached")
            state_node.status = "not-reached"
            return opaque

        opaque = dict(opaque)
        internal_plan = internal_plan_path[service.name, head_end].plan
        # First check if there is plan error in internal, set it
        if internal_plan.failed:
            self.log.info(f"Internal plan for {service.name} {head_end} "
                          "is failed, marking ready is failed")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message, plan,
                                       component, self.log, head_end)
            return list(opaque.items())

        # Check if there is commit-queue with this head-end
        if internal_plan.commit_queue:
            if len(internal_plan.commit_queue.queue_item) > 0:
                self.log.info(f"Internal plan for {service.name} {head_end} has pending "
                              "queue-item, ready is not-reached")
                state_node.status = "not-reached"
                return list(opaque.items())

        # Internal plan should also have self -> ready:reached if there is no failure or pending CQ
        # This is to handle the case where undeploy-no-networking is performed to recover from
        # create failures - in that scenario, internal plan details (component etc.)
        # would be wiped out but only plan entry would remain.
        if ("ncs:self", "self") not in internal_plan.component:
            state_node.status = "not-reached"
            return list(opaque.items())
        return list(opaque.items())
