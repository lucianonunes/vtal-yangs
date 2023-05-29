# -*- mode: python; python-indent: 4 -*-
import traceback
import uuid
from datetime import datetime, timezone

import ncs
import _ncs
from . import utils as Utils
from .ron_ml_errors import (
    RonMLException,
    UserError,
)
from cisco_ron_core_fp_common import utils as RonUtils
from cisco_ron_core_fp_common.constants import AsciiArt
from core_fp_common import common_utils
from .constants import StrConstants
from .ron_validations import validate
from .namespaces.ciscoRonCfp_ns import ns as roncfp_ns
from cisco_ron_core_fp_common.status_codes import StatusCodes

# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class RonMlCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            "ENTRY_POINT for {} at pre_mod of RON-ML, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        proplist = dict(proplist)
        proplist[StrConstants.validation_error_key] = ""
        proplist[StrConstants.edit_key] = "False"
        proplist[StrConstants.cq_data_key] = repr(dict())
        """
            If operation is create or update run validation checks:
            Validation callback(s) is/are called at the end of the create/update callbacks,
            and any exception will be presisted into plan/failed. In-order to
            a) run the validations before we get to work
            b) avoid persisting validation errors into plan (which requires a re-deploy to
                clear).
            So:
            1) We do validation in pre-mod.
            2) Save any exceptions encountered into opaque
            3) If validation errors are present in the opaque avoid running any of the component
            code [to avoid running into more issues], and
            4) when validation callback is invoked, re-run the validation code and throw exception.
            Re-running is required to support "validate" N/B command.
        """
        try:
            # Check commit parameters
            th = ncs.maagic.get_trans(root)
            ## If operation is create or update, check interface Loopback on device
            if op != _ncs.dp.NCS_SERVICE_DELETE:
                self.is_redeploy = False
                self.clear_rollback = False
                self.edit = False
                self.affects_cs_deployment = False
                self.affects_init_post_action = False
                self.end_point_devices = []
                self.affects_both_endpoints = False

                self.service = ncs.maagic.get_node(th, kp)
                if op == _ncs.dp.NCS_SERVICE_CREATE:
                    self.log.info(AsciiArt.torpedo)
                    ## If op is create, validate status code mapping is loaded
                    status_code_cfp = (
                        root.cfp_common_status_codes__status_codes.core_function_pack
                    )
                    if not status_code_cfp.exists("RON"):
                        raise UserError(
                            self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                        ).set_context(
                            "Status Code", "Missing RON status code mapping"
                        ).add_state(
                            "Keypath", str(kp)
                        ).finish()
                else:
                    proplist[
                        StrConstants.past_create_state_key
                    ] = StrConstants.past_create_state_key
                    th.diff_iterate(self.diter, 0)
                    if self.is_redeploy:
                        self.log.info(AsciiArt.caterpillar)
                    else:
                        self.log.info(AsciiArt.wave)
                        if self.edit:  # Edit is only applicable for Zr routers
                            proplist[StrConstants.edit_key] = "True"
                            if self.affects_both_endpoints:
                                self.end_point_devices =\
                                    [ep.end_point_device for ep in self.service.end_point]
                            self.log.info(f"Changes to {self.end_point_devices}")
                            cq_data = dict(
                                Utils.check_for_cq(th, root, self.end_point_devices)
                            )
                            proplist[StrConstants.cq_data_key] = repr(cq_data)

                if not self.is_redeploy:
                    # Validate
                    validate(root, self.service, self.log, op, self)

                if root.ron.ron_ml[self.service.name].ron_data.do_rollback.exists():
                    del root.cisco_ron_cfp__ron.ron_ml[
                        self.service.name
                    ].ron_data.end_point
                    if self.clear_rollback:
                        # remove the rollback flag if present whenever clear-rollback updated
                        # in the service
                        self.log.info("Clearing do-rollback")
                        root.ron.ron_ml[self.service.name].ron_data.do_rollback.delete()

        except UserError as exp:
            traceback.print_exc()
            proplist[StrConstants.validation_error_key] = str(exp)

        except Exception as exp1:
            self.log.error(AsciiArt.roadblock)
            raise RonMLException(
                self.log, StatusCodes.PRE_MOD_CALLBACK_ERROR, str(exp1)
            ).set_context(
                "pre-modification", "Failed to process pre-modification call-back"
            ).add_state(
                "service", str(kp)
            ).finish()

        self.log.info(
            "EXIT_POINT for {} at pre_mod of RON-ML, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )

        return list(proplist.items())

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info("Service create(service=", service._path, ")")
        return proplist

    def diter(self, keypath, op, oldv, newv):
        self.log.info(f" path --> {keypath} and op : {RonUtils.get_service_operation(op)}")
        idx = len(keypath) - 2
        tag = str(keypath[idx])
        if tag != roncfp_ns.cisco_ron_cfp_ron_ml_:
            return ncs.ITER_CONTINUE

        idx = idx - 2
        tag = None
        if idx >= 0:
            tag = str(keypath[idx])
            if tag == roncfp_ns.cisco_ron_cfp_private_:
                idx = idx - 1
                # else check if the change is on leaf re_deploy_counter
                tag = str(keypath[idx])
                if tag == roncfp_ns.cisco_ron_cfp_re_deploy_counter_:
                    self.is_redeploy = True
                    return ncs.ITER_STOP
                else:
                    return ncs.ITER_CONTINUE
            elif tag == roncfp_ns.cisco_ron_cfp_clear_rollback_:
                self.clear_rollback = True
                # We know its rollback and any change is allowed so move on.
                return ncs.ITER_STOP
            elif (
                tag == roncfp_ns.cisco_ron_cfp_mode_
                or tag == roncfp_ns.cisco_ron_cfp_bandwidth_
                or tag == roncfp_ns.cisco_ron_cfp_ols_domain_
            ):
                # mode and badnwidth changes will affect init_post_action
                # ols_domain should not be set after init_post_action
                self.affects_init_post_action = True
                return ncs.ITER_CONTINUE
            elif tag == roncfp_ns.cisco_ron_cfp_end_point_:
                idx = idx - 1
                key = str(keypath[idx][0])
                idx = idx - 1  # Skip the key
                if idx < 0:
                    if (
                        op == _ncs.dp.NCS_SERVICE_CREATE
                        or op == _ncs.dp.NCS_SERVICE_DELETE
                    ):
                        # Do not allow swapping of cfp_end_points
                        self.affects_init_post_action = True
                        return ncs.ITER_STOP
                    else:
                        return ncs.ITER_RECURSE

                # else check if the change on terminal-device-packet
                tag = str(keypath[idx])
                if tag == roncfp_ns.cisco_ron_cfp_terminal_device_optical_:
                    idx = idx - 1
                    tag = str(keypath[idx])
                    if tag == roncfp_ns.cisco_ron_cfp_line_port_:
                        self.affects_init_post_action = True
                        return ncs.ITER_STOP
                    else:
                        self.affects_cs_deployment = (
                            True  # TODO might not affect the CS
                        )
                        return ncs.ITER_STOP
                elif tag == roncfp_ns.cisco_ron_cfp_ols_domain_:
                    self.affects_cs_deployment = True
                    return ncs.ITER_STOP
                elif tag == roncfp_ns.cisco_ron_cfp_terminal_device_packet_:
                    idx = idx - 1
                    if idx < 0:
                        if (
                            op == _ncs.dp.NCS_SERVICE_CREATE
                            or op == _ncs.dp.NCS_SERVICE_DELETE
                        ):
                            # Do not add packet domain if service is already past init_post_action
                            self.affects_init_post_action = True
                            return ncs.ITER_STOP
                        else:
                            return ncs.ITER_RECURSE
                    self.edit = True
                    self.end_point_devices.append(key)
                    return ncs.ITER_CONTINUE
            elif tag == roncfp_ns.cisco_ron_cfp_srlg_:
                self.edit = True
                self.affects_both_endpoints = True
                # Any changes to srlg should be allowed
                return ncs.ITER_CONTINUE
            else:
                # change in circuit-id, grid-type, frequency, dac-rate
                self.affects_cs_deployment = True
                return ncs.ITER_CONTINUE

        return ncs.ITER_RECURSE

    @ncs.application.Service.post_modification
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            "ENTRY_POINT for {} at post_mod of RON-ML, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        self.log.info(AsciiArt.hibernate)
        self.log.info(
            "EXIT_POINT for {} at post_mod of RON-ML, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        return proplist


class RonMlSelf(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    def cb_nano_create(
        self, tctx, root, service, plan, component, state, opaque, comp_vars
    ):
        self.log.info(
            "ENTRY_POINT for nano self create {} at {} of RON-ML".format(
                service.name, state
            )
        )
        opaque_dict = dict(opaque)
        # If we have any validation errors, avoid running callback code.
        if opaque_dict.get(StrConstants.validation_error_key) != "":
            return list(opaque_dict.items())

        new_opaque = None
        if state == StrConstants.ncs_init:
            try:
                self._create_init_callback(
                    tctx, root, service, plan, component, state, opaque, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(
                    self.log, StatusCodes.SELF_CALLBACK_ERROR, str(exp)
                ).set_context(
                    "Self Init", "Failed to process create self-init call-back"
                ).add_state(
                    "service", service._path
                ).finish()
        elif state == StrConstants.ncs_ready:
            try:
                new_opaque = self._create_self_ready(
                    root, plan, service, component, state, opaque
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(
                    self.log, StatusCodes.SELF_CALLBACK_ERROR, str(exp)
                ).set_context(
                    "Self Ready", "Failed to process create self-ready call-back"
                ).add_state(
                    "service", service._path
                ).finish()

        self.log.info(
            "EXIT_POINT for nano self create {} at {} of RON-ML".format(
                service.name, state
            )
        )
        if new_opaque is None:
            return opaque

        return new_opaque

    @ncs.application.NanoService.delete
    def cb_nano_delete(
        self, tctx, root, service, plan, component, state, opaque, comp_vars
    ):
        self.log.info(
            "ENTRY_POINT for nano self delete {} at {} of RON-ML".format(
                service.name, state
            )
        )
        try:
            self.update_self(root, plan, service, component, state, opaque)
        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            raise RonMLException(
                self.log, StatusCodes.SELF_CALLBACK_ERROR, str(exp)
            ).set_context(
                "Self Init", "Failed to process delete self-init call-back"
            ).add_state(
                "service", service._path
            ).finish()
        self.log.info(
            "EXIT_POINT for nano self delete {} at {} of RON-ML".format(
                service.name, state
            )
        )
        return opaque

    def update_self(self, root, plan, service, component, state, opaque):
        opaque_dict = dict(opaque)
        err_codes = []
        for plan_component in plan.component:
            if not (
                plan_component.name == "self"
                and plan_component.type == StrConstants.ncs_self
            ):
                for plan_state in plan_component.state:
                    if plan_state.status == "failed":
                        self.log.debug(f"Component {component} state {state} failed")
                        err_codes.append(plan_component.status_code)
        if err_codes:
            plan.component[component].state[state].status = "failed"
            plan.component[component].status_code = err_codes[0]
            if len(err_codes) > 1:
                plan.component[
                    component
                ].status_code = StatusCodes.MULTIPLE_COMPONENT_FAILURE.cfp_code
        return list(opaque_dict.items())

    def _create_self_ready(self, root, plan, service, component, state, opaque):
        opaque_dict = dict(opaque)
        err_codes = []
        zr_ready_count = 0
        for plan_component in plan.component:
            if not (
                plan_component.name == "self"
                and plan_component.type == StrConstants.ncs_self
            ):
                for plan_state in plan_component.state:
                    if plan_state.status == "failed":
                        self.log.debug(f"Component {component} state {state} failed")
                        err_codes.append(plan_component.status_code)
                if (plan_component.type == StrConstants.com_router) and (
                    plan_component.state[StrConstants.ncs_ready].status == "reached"
                ):
                    zr_ready_count = zr_ready_count + 1

        if not plan.component[component].status_code:
            if err_codes:
                plan.component[component].state[state].status = "failed"
                plan.component[component].status_code = err_codes[0]
                if len(err_codes) > 1:
                    plan.component[
                        component
                    ].status_code = StatusCodes.MULTIPLE_COMPONENT_FAILURE.cfp_code
            else:
                plan.component[
                    component
                ].status_code = StatusCodes.READY_SUCCESS.cfp_code
        self.log.info(
            f"Ready key : {opaque_dict.get(StrConstants.zrs_ready_key, None)}"
        )
        if zr_ready_count == 2:
            # Delete the failure kicker if both router components are ready-reached for first time
            opaque_dict[StrConstants.zrs_ready_key] = "True"

        if opaque_dict.get(StrConstants.edit_key) == "True":
            cq_data = Utils.eval_dict(opaque_dict.get(StrConstants.cq_data_key))
            if not any([x == "True" for x in cq_data.values()]):
                plan.component[component].state[state].when = datetime.now(
                    timezone.utc
                ).isoformat()

        return list(opaque_dict.items())

    def _create_init_callback(
        self, _tctx, root, service, plan, component, state, opaque, _comp_vars
    ):
        self.log.info(f"create init callback: {service.name}")
        opaque_dict = dict(opaque)
        if service.ron_data.do_rollback.exists():
            # trigger rollback of components
            self.log.info("do-rollback is set - self rolling back")
            plan.component[
                component
            ].status_code = StatusCodes.ROLLBACK_TRIGGERED.cfp_code
            plan.component[component].state[state].status = "failed"
        else:
            del plan.component[component].status_code
            del plan.status_code_detail

        has_zr_components = False
        for plan_component in plan.component:
            if plan_component.type == StrConstants.com_router:
                has_zr_components = True

        self.log.info(
            f"Ready key : {opaque_dict.get(StrConstants.zrs_ready_key, None)}"
        )
        if has_zr_components and not (
            opaque_dict.get(StrConstants.zrs_ready_key, None)
        ):
            # create the failure kicker to trigger rollback of optical in case any fails if
            # router components are not ready-reached previously
            self._create_zr_kicker(root, service)

    def _create_zr_kicker(self, root, service):
        component_vars = ncs.template.Variables()
        plan_vars = ncs.template.Variables()
        component_vars.add("ID", "ron-ml-failure-" + str(service.name) + "-component-kicker")
        plan_vars.add("ID", "ron-ml-failure-" + str(service.name) + "-plan-kicker")
        ## Applying failure kicker config
        i = 1
        for end_point in service.end_point:
            component_vars.add("ENDPOINT" + str(i), str(end_point.end_point_device))
            i = i + 1

        template = ncs.template.Template(service)
        template.apply("cisco-ron-cfp-zr-component-failure-kicker", component_vars)
        template.apply("cisco-ron-cfp-zr-plan-failure-kicker", plan_vars)
        self.log.info(f"Created rollback kicker ron-ml-failure-{service.name}-kicker")


class RonMlOptical(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    def cb_nano_create(
        self, tctx, root, service, plan, component, state, opaque, comp_vars
    ):

        comp_vars = dict(comp_vars)

        self.log.info(
            "ENTRY_POINT for nano optical controller create {} {} at {} of RON-ML".format(
                service.name, comp_vars["OPTICAL_CONTROLLER"], state
            )
        )

        opaque_dict = dict(opaque)
        # If we have any validation errors, avoid running callback code.
        if opaque_dict.get(StrConstants.validation_error_key) != "":
            return list(opaque_dict.items())

        new_opaque = None
        if state == StrConstants.ncs_init:
            try:
                new_opaque = self._create_init_callback(
                    tctx, root, service, plan, component, state, opaque_dict, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(
                    self.log, StatusCodes.OPTICAL_INIT_ERROR, str(exp)
                ).set_context(
                    "Optical Init", "Failed to process create Optical init call-back"
                ).add_state(
                    "service", service._path
                ).finish()
        elif state == StrConstants.state_conf_apply:
            try:
                if service.ron_data.do_rollback.exists():
                    # trigger rollback of the optical component
                    self.log.info("do-rollback is set - optical rolling back")
                else:
                    new_opaque = self._create_config_apply(
                        tctx,
                        root,
                        service,
                        plan,
                        component,
                        state,
                        opaque_dict,
                        comp_vars,
                    )
            except UserError:
                self.log.error(AsciiArt.roadblock)
                raise
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(
                    self.log, StatusCodes.OPTICAL_CONFIG_ERROR, str(exp)
                ).set_context("optical controller config").add_state(
                    "service", service._path
                ).finish()

        elif state == StrConstants.ncs_ready:
            try:
                new_opaque = self._update_optical_ready(
                    tctx, root, service, plan, component, state, opaque_dict, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(
                    self.log, StatusCodes.OP_READY_ERROR, str(exp)
                ).set_context("optical controller config").add_state(
                    "service", service._path
                ).finish()

        self.log.info(
            "EXIT POINT for nano optical controller create {} {} at {} of RON-ML".format(
                service.name, comp_vars["OPTICAL_CONTROLLER"], state
            )
        )

        if new_opaque is None:  # TODO is this required ?
            return opaque

        return list(new_opaque.items())

    @ncs.application.NanoService.delete
    def cb_nano_delete(
        self, tctx, root, service, plan, component, state, opaque, comp_vars
    ):
        new_opaque = None
        comp_vars = dict(comp_vars)
        opaque_dict = dict(opaque)

        self.log.info(
            "ENTRY_POINT for nano optical controller delete: {} {} at {} of RON-ML".format(
                service.name, comp_vars["OPTICAL_CONTROLLER"], state
            )
        )

        if state == StrConstants.ncs_init:
            try:
                new_opaque = self._delete_init_callback(
                    tctx, root, service, plan, component, state, opaque_dict, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(
                    self.log, StatusCodes.OP_INIT_RB_ERROR, str(exp)
                ).set_context(
                    "optical init", "Failed to process rollback of init call-back"
                ).add_state(
                    "service", service._path
                ).finish()

        self.log.info(
            "EXIT_POINT for nano optical controler delete: {} {} at {} of RON-ML".format(
                service.name, comp_vars["OPTICAL_CONTROLLER"], state
            )
        )

        if new_opaque is None:
            return opaque

        return list(new_opaque.items())

    def _update_optical_ready(
        self, _tctx, root, service, plan, component, state, opaque_dict, _comp_vars
    ):

        self.log.info(f"Optical ready : {service.name} ")

        if Utils.is_lsa(root):
            op_plan = (
                root.cisco_ron_cfp__ron.remote_local_data.connectivity_service_plan
            )
        else:
            op_plan = (
                root.tapi_common__context.tapi_connectivity__connectivity_context.
                connectivity_service_plan
            )

        # Step 1: check if there is plan
        service_uuid = opaque_dict.get(StrConstants.cs_uuid_key)
        if service_uuid not in op_plan:
            self.log.info("optical stack service is not ready yet")
            plan.component[component].state[state].status = "not-reached"
            plan.component[component].status_code = StatusCodes.INPROGRESS.cfp_code
            return opaque_dict

        op_plan_instance = op_plan[service_uuid].plan
        # Optical controller
        oc = opaque_dict.get(StrConstants.optical_controller_key)

        # Step 2: Error checks
        # Step 2.1: check if there are any failed plan states
        for plan_component in op_plan_instance.component:
            for plan_state in plan_component.state:
                if (plan_state.status == "failed"):
                    self.log.error(
                        f"Component {plan_component.name} state {plan_state.name} failed\
                                     @{op_plan_instance._path}, setting optical ready\
                                                                         state to failed"
                    )
                    plan.component[component].state[state].status = "failed"
                    code = StatusCodes.OPTICAL_FAILUER.cfp_code
                    plan.component[component].status_code = code
                    if plan_component.status_code:
                        plan.component[component].status_code = plan_component.status_code

                    # Now we know plan has failed. Does it have any error_info ?
                    # If so now is the best time to pick-it-up
                    if op_plan_instance.failed:
                        plan.component[component].state[state].status = "failed"
                        plan.failed.create()
                        if op_plan_instance.error_info:
                            plan.error_info.create()
                            plan.error_info.message = op_plan_instance.error_info.message
                            Utils.save_to_status_code_detail(
                                plan.error_info.message,
                                plan,
                                component,
                                self.log,
                                oc
                            )
                    else:
                        Utils.save_to_status_code_detail(None, plan, component, self.log, oc)
                    return opaque_dict

        # Step 2.2: check if there is plan error
        if op_plan_instance.failed:
            plan.component[component].state[state].status = "failed"
            plan.failed.create()
            if op_plan_instance.error_info:
                plan.error_info.create()
                msg = op_plan_instance.error_info.message
                plan.error_info.message = msg
                Utils.save_to_status_code_detail(msg, plan, component, self.log, oc)
            return opaque_dict

        # Step 3: has the internal plan ready-reached ?
        if (
            op_plan_instance.component["ncs:self", "self"]
            .state[StrConstants.ncs_ready]
            .status
            != "reached"
        ):
            plan.component[component].state[state].status = "not-reached"
            plan.component[component].status_code = StatusCodes.INPROGRESS.cfp_code
            return opaque_dict

        # Step 4: does the plan has commit-queue item ?
        if op_plan_instance.commit_queue.queue_item:
            plan.component[component].state[state].status = "not-reached"
            plan.component[component].status_code = StatusCodes.CQ_INPROGRESS.cfp_code
            return opaque_dict

        # Step 5: On success
        self._populate_optics_oper_data(root, service, opaque_dict)
        plan.component[component].status_code = StatusCodes.READY_SUCCESS.cfp_code
        opaque_dict[StrConstants.cs_done_key] = StrConstants.cs_done_key
        return opaque_dict

    def _populate_optics_oper_data(self, root, service, opaque_dict):
        self.log.info(f"Populating optics oper data in ron for service {service.name}")
        ron_data = root.cisco_ron_cfp__ron.ron_ml[service.name].ron_data
        for end_point in service.end_point:
            if Utils.is_lsa(root):
                self.log.debug("LSA RFS_NODE")
                cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)
                if (cs_uuid in root.cisco_ron_cfp__ron.remote_local_data
                        .connectivity_service_oper_data):
                    optics_data = root.cisco_ron_cfp__ron.remote_local_data \
                        .connectivity_service_oper_data[cs_uuid]
            else:
                self.log.debug("NON LSA")
                cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)
                if (cs_uuid in root.tapi_common__context.tapi_connectivity__connectivity_context
                        .connectivity_service_oper_data):
                    optics_data = root.tapi_common__context \
                        .tapi_connectivity__connectivity_context \
                        .connectivity_service_oper_data[cs_uuid]

            optical_ep_name = opaque_dict.get(end_point.end_point_device)
            self.log.debug(f"optic endpoint name is {optical_ep_name}")
            ron_data.frequency = optics_data.end_point[optical_ep_name].\
                optics_data.frequency
            if end_point.end_point_device in ron_data:
                end_point_data = ron_data.end_point[end_point.end_point_device]
            else:
                end_point_data = ron_data.end_point.create(end_point.end_point_device)

            transmit_power = optics_data.end_point[optical_ep_name].optics_data.transmit_power
            # 10s dBm of what ONC returns
            transmit_power = int(float(transmit_power) * 10)
            end_point_data.transmit_power = transmit_power

    def _create_init_callback(
        self, _tctx, root, service, plan, component, state, opaque_dict, _comp_vars
    ):
        self.log.info(f"create init callback: {service.name}")

        if opaque_dict.get(StrConstants.cs_uuid_key) is None:
            cs_uuid = uuid.uuid4()
            opaque_dict[StrConstants.cs_uuid_key] = str(cs_uuid)
        else:
            cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)

        # Store lookup data
        service_lookup = root.cisco_ron_cfp__ron.ron_service_lookup.create(str(cs_uuid))
        reverse_service_lookup = root.cisco_ron_cfp__ron.uuid_lookup.create(service.name)
        service_lookup.ron_ml_service = service.name
        reverse_service_lookup.uuid = str(cs_uuid)
        if opaque_dict.get(StrConstants.optical_controller_key) is None:
            for end_point in service.end_point:

                managed_link = root.cisco_ron_cfp__ron.inter_layer_link[
                    end_point.end_point_device,
                    end_point.terminal_device_optical.line_port,
                ]
                optical_controller = managed_link.ols_domain.optical_controller
                service_lookup.optical_controller = optical_controller
                opaque_dict[StrConstants.optical_controller_key] = str(
                    optical_controller
                )
                break
        else:
            service_lookup.optical_controller = opaque_dict.get(
                StrConstants.optical_controller_key
            )
        # Done storing lookup data

        # inter-link-db update
        # Why in optical-init ?
        # This is the first state that enters after init-post-action if the ron-ml
        # has ols-domain set
        for ep in service.end_point:
            key = (ep.end_point_device, ep.terminal_device_optical.line_port)
            self.log.debug(
                f"persist inter-layer-link {key} usage @  inter-layer-link-oper"
            )
            mg_op = root.cisco_ron_cfp__ron.inter_layer_link_oper.create(key)
            mg_op.ron_ml_service = service.name

        return opaque_dict

    def _delete_init_callback(
        self, _tctx, root, service, plan, component, state, opaque_dict, _comp_vars
    ):
        self.log.info(f"delete init callback: {service.name}")

        if Utils.is_lsa(root):
            op_plan = (
                root.cisco_ron_cfp__ron.remote_local_data.connectivity_service_plan
            )
        else:
            op_plan = (
                root.tapi_common__context.tapi_connectivity__connectivity_context.
                connectivity_service_plan
            )

        # Step 1: check if there is plan
        cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)
        if cs_uuid not in op_plan:
            self.log.info("optical stack service is deleted")
            del root.cisco_ron_cfp__ron.ron_service_lookup[cs_uuid]
            del root.cisco_ron_cfp__ron.uuid_lookup[service.name]
            del plan.component[component].status_code
            return opaque_dict

        try:
            service_lookup = root.cisco_ron_cfp__ron.ron_service_lookup.create(str(cs_uuid))
            reverse_service_lookup = root.cisco_ron_cfp__ron.uuid_lookup.create(service.name)
            service_lookup.ron_ml_service = service.name
            reverse_service_lookup.uuid = str(cs_uuid)
            op_plan_instance = op_plan[cs_uuid].plan
            if opaque_dict.get(StrConstants.optical_controller_key) is not None:
                service_lookup.optical_controller = opaque_dict.get(
                    StrConstants.optical_controller_key
                )

            # Step 2: check if there is plan error
            if op_plan_instance.failed:
                plan.component[component].state[state].status = "failed"
                plan.failed.create()
                if op_plan_instance.error_info:
                    plan.error_info.create()
                    msg = op_plan_instance.error_info.message
                    plan.error_info.message = msg
                    # plan.component[component].status_code = Utils.get_status_code(msg)
                return opaque_dict
        except KeyError:
            # Handle optical service deletion if happened in between
            self.log.info("optical stack service should have been deleted")
            del root.cisco_ron_cfp__ron.ron_service_lookup[cs_uuid]
            del root.cisco_ron_cfp__ron.uuid_lookup[service.name]
            del plan.component[component].status_code
            return opaque_dict

        # Step 3: may be will wait for the delete
        plan.component[component].state[state].status = "reached"
        plan.component[component].status_code = StatusCodes.DELETE_INPROGRESS.cfp_code
        return opaque_dict

    def _create_config_apply(
        self, _tctx, root, service, plan, component, state, opaque_dict, comp_vars
    ):
        # TODO: Come-back later
        ## From user input or learned
        self.log.debug("Preparing Optical configuration")
        cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)

        # Initialize template
        template = ncs.template.Template(service)
        template_vars = ncs.template.Variables()
        template_vars.add("SERVICE_NAME", service.name)
        template_vars.add("CS_UUID", cs_uuid)
        template_vars.add("SERVICE_STATE", service.ols_domain.service_state)
        template_vars.add("CENTRAL_FREQUENCY", "")
        if service.frequency:
            template_vars.add("CENTRAL_FREQUENCY", str(service.frequency))
        i = 1
        for end_point in service.end_point:

            ## Get SIP either from inter-layer-link or from optical controller
            ## and build AP info
            sip_uuid = None

            managed_link = root.cisco_ron_cfp__ron \
                .inter_layer_link[end_point.end_point_device,
                                  end_point.terminal_device_optical.line_port]

            end_point_data = root.cisco_ron_cfp__ron.ron_ml[service.name].ron_data \
                .end_point[end_point.end_point_device]

            optical_controller = managed_link.ols_domain.optical_controller

            sip_uuid = end_point_data.sip

            if sip_uuid is not None:
                ap_id_type = "PROPRIETARY"
                (capability_valid, capability_pid, fec, bandwidth, modulation, baud_rate,
                 dac_rate, _,) = \
                    Utils.get_pid_info(root, service, end_point, self.log)
                if not capability_valid:
                    raise UserError(self.log, StatusCodes.PLUGGABLE_CAPABILITY_NOT_SUPPORTED) \
                        .set_context("Validation", "Hardware capability does not support this "
                                     + "configuration of mode/bandwidth/fec",) \
                        .add_state("device", str(end_point.end_point_device)) \
                        .add_state("Service", str(service._path)).finish()
                ap_code = (
                    capability_pid
                    + "#"
                    + str(fec)
                    + "#"
                    + str(bandwidth)
                    + "#"
                    + str(modulation)
                    + "#"
                    + str(baud_rate)
                    + "#"
                    + str(dac_rate)
                )
                template_vars.add("CONTROLLER", optical_controller)
                template_vars.add("ENDPOINT" + str(i), "EndPoint" + str(i))
                template_vars.add(
                    "ENDPOINT_STATE" + str(i), end_point.ols_domain.end_point_state
                )
                template_vars.add("SIP_UUID" + str(i), sip_uuid)
                template_vars.add("AP_ID_TYPE" + str(i), ap_id_type)
                template_vars.add("AP_CODE" + str(i), ap_code)

                opaque_dict[str(end_point.end_point_device)] = "EndPoint" + str(i)
            else:
                raise UserError(
                    self.log, StatusCodes.OPTICS_SIP_NOT_FOUND
                ).set_context(
                    "Validation",
                    "Dynamic optics SIP is not present on optical controller",
                ).add_state(
                    "Device", str(end_point.end_point_device)
                ).add_state(
                    "Service", str(service._path)
                ).finish()

            i += 1

        # Determine if LSA deploy

        if Utils.is_lsa(root):
            template_vars.add(
                "NSO_DEVICE", Utils.safe_get_remote_nso(self.log, optical_controller)
            )
            self.log.info("LSA RFS_NODE")

        # Invoke cisco-ron-cfp-using-cisco-optical-template either interal or ned
        template.apply("cisco-ron-cfp-using-cisco-optical-template", template_vars)
        return opaque_dict


class RonMlRouter(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    def cb_nano_create(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        new_opaque = None
        comp_vars = dict(comp_vars)

        self.log.info("ENTRY_POINT for nano router create {} {} at {} of RON-ML"
                      .format(service.name, comp_vars["ROUTER"], state))

        opaque_dict = dict(opaque)
        # If we have any validation errors, avoid running callback code.
        if opaque_dict.get(StrConstants.validation_error_key) != "":
            return list(opaque_dict.items())

        if state == StrConstants.state_conf_apply:
            try:
                if service.ron_data.do_rollback.exists():
                    # trigger rollback of the zr component
                    self.log.info("do-rollback is set - zr rolling back")
                else:
                    new_opaque = self._create_config_apply(
                        tctx, root, service,
                        plan, component, state,
                        opaque_dict, comp_vars,)
            except RonMLException:
                raise
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(self.log, StatusCodes.ZR_CONFIG_ERROR, str(exp)) \
                    .set_context("terminal-device config") \
                    .add_state("service", service._path) \
                    .add_state("device", comp_vars["ROUTER"]).finish()
        elif state == StrConstants.ncs_ready:
            try:

                new_opaque = self._update_zr_ready(
                    tctx, root, service,
                    plan, component, state,
                    opaque_dict, comp_vars)
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(self.log, StatusCodes.ZR_READY_ERROR, str(exp)) \
                    .set_context("terminal-device config") \
                    .add_state("service", service._path) \
                    .add_state("device", comp_vars["ROUTER"]).finish()

        self.log.info("EXIT_POINT for nano router create {} {} at {} of RON-ML"
                      .format(service.name, comp_vars["ROUTER"], state))

        if new_opaque is None:
            return opaque

        return list(new_opaque.items())

    @ncs.application.NanoService.delete
    def cb_nano_delete(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        new_opaque = None
        comp_vars = dict(comp_vars)
        opaque_dict = dict(opaque)
        self.log.info("ENTRY_POINT for nano router delete: {} {} at {} of RON-ML"
                      .format(service.name, comp_vars["ROUTER"], state))

        if state == StrConstants.ncs_init:
            try:
                new_opaque = self._delete_init_callback(
                    tctx, root, service, plan, component, state, opaque_dict, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise RonMLException(self.log, StatusCodes.ZR_INIT_RB_ERROR, str(exp)) \
                    .set_context("router init", "Failed to process rollback of init call-back") \
                    .add_state("service", service._path) \
                    .add_state("device", comp_vars["ROUTER"]).finish()

        self.log.info("EXIT_POINT for nano router delete: {} {} at {} of RON-ML"
                      .format(service.name, comp_vars["ROUTER"], state))

        if new_opaque is None:
            return opaque

        return list(opaque_dict.items())

    def _update_zr_ready(
        self, _tctx, root, service, plan, component, state, opaque_dict, comp_vars
    ):

        router = comp_vars["ROUTER"]
        self.log.info(f"Terminal device ready : {service.name}/{router} ")

        if Utils.is_lsa(root):
            zr_plan = root.cisco_ron_cfp__ron.remote_local_data.dco_plan
        else:
            zr_plan = root.cisco_zr_cfp__zr.dco_plan

        # Step 1: check if there is plan
        if (service.name + "-internal", router) not in zr_plan:
            self.log.info("terminal device stack service is not ready yet")
            plan.component[component].state[state].status = "not-reached"
            plan.component[component].status_code = StatusCodes.INPROGRESS.cfp_code
            return opaque_dict

        zr_plan_instance = zr_plan[service.name + "-internal", router].plan

        # Step 2: check if there is plan error
        if zr_plan_instance.failed:
            plan.component[component].state[state].status = "failed"
            plan.failed.create()
            if zr_plan_instance.error_info:
                plan.error_info.create()
                msg = zr_plan_instance.error_info.message
                plan.error_info.message = msg
                Utils.save_to_status_code_detail(msg, plan, component, self.log)
            return opaque_dict

        # Step 3: does the plan has commit-queue item ?
        if zr_plan_instance.commit_queue.queue_item:
            plan.component[component].state[state].status = "not-reached"
            plan.component[component].status_code = StatusCodes.CQ_INPROGRESS.cfp_code
            return opaque_dict

        # Step 4: Check if there are edits? If so we need to handle the ready accordingly
        if opaque_dict.get(StrConstants.edit_key) == "True":
            self.log.info(opaque_dict.get(StrConstants.cq_data_key))
            cq_data = Utils.eval_dict(opaque_dict.get(StrConstants.cq_data_key))
            if router in cq_data and cq_data[router] == "True":
                plan.component[component].state[state].status = "not-reached"
            elif router in cq_data and cq_data[router] == "False":
                plan.component[component].state[state].when = datetime.now(timezone.utc).isoformat()
                self.log.info("Reseting time-stamp on ready")
            return opaque_dict

        # Step 5: On success
        plan.component[component].status_code = StatusCodes.READY_SUCCESS.cfp_code
        return opaque_dict

    def _delete_init_callback(
            self, _tctx, root, service, plan, component, state, opaque_dict, comp_vars):
        router = comp_vars["ROUTER"]
        self.log.info(f"Terminal device init : {service.name}/{router} ")

        if Utils.is_lsa(root):
            zr_plan = root.cisco_ron_cfp__ron.remote_local_data.dco_plan
        else:
            zr_plan = root.cisco_zr_cfp__zr.dco_plan

        # Step 1: check if there is plan
        if (service.name + "-internal", router) not in zr_plan:
            self.log.info("ZR stack service is deleted")
            del plan.component[component].status_code
            return opaque_dict

        try:
            zr_plan_instance = zr_plan[service.name + "-internal", router].plan

            # Step 2: check if there is plan error
            if zr_plan_instance.failed:
                plan.component[component].state[state].status = "failed"
                plan.failed.create()
                if zr_plan_instance.error_info:
                    plan.error_info.create()
                    msg = zr_plan_instance.error_info.message
                    plan.error_info.message = msg
                    # plan.component[component].status_code = Utils.get_status_code(msg)
                return opaque_dict
        except KeyError:
            self.log.info("ZR stack service should have been deleted")
            del plan.component[component].status_code
            return opaque_dict

        # Step 3: may be will wait for the delete
        plan.component[component].state[state].status = "reached"
        plan.component[component].status_code = StatusCodes.DELETE_INPROGRESS.cfp_code
        return opaque_dict

    def _create_config_apply(
            self, _tctx, root, service, plan, component, state, opaque_dict, comp_vars):

        router = comp_vars["ROUTER"]
        end_point = service.end_point[router]
        self.log.debug("Entered Config Apply for Router {} for service {}"
                       .format(service.name, router))

        ## From user input or learned

        (_, capability_pid, fec, _, _, _, dac, transmit_power) = Utils \
            .get_pid_info(root, service, end_point, self.log)

        end_point_data = root.cisco_ron_cfp__ron.ron_ml[service.name].ron_data \
            .end_point[end_point.end_point_device]

        flex_id = "NA"
        if end_point_data.flexport_linecard_pid:
            flex_id = end_point_data.flexport_linecard_pid

        platform_pid = "NA"
        if end_point_data.platform_pid:
            platform_pid = end_point_data.platform_pid

        grid_type = service.grid_type

        optics_data = None
        if service.ols_domain:
            if Utils.is_lsa(root):
                self.log.info("LSA RFS_NODE")
                cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)
                if (cs_uuid in root.cisco_ron_cfp__ron.remote_local_data
                        .connectivity_service_oper_data):
                    optics_data = root.cisco_ron_cfp__ron.remote_local_data \
                        .connectivity_service_oper_data[cs_uuid]
            else:
                self.log.info("NON LSA")
                cs_uuid = opaque_dict.get(StrConstants.cs_uuid_key)
                if (cs_uuid in root.tapi_common__context.tapi_connectivity__connectivity_context
                        .connectivity_service_oper_data):
                    optics_data = root.tapi_common__context \
                        .tapi_connectivity__connectivity_context \
                        .connectivity_service_oper_data[cs_uuid]
        frequency = service.frequency
        if service.ols_domain:
            optical_ep_name = opaque_dict.get(router)
            if service.frequency is None:
                frequency = optics_data.end_point[optical_ep_name].optics_data.frequency

            if end_point.terminal_device_optical.transmit_power is None:
                transmit_power = optics_data.end_point[optical_ep_name].optics_data.transmit_power
                # 10s dBm of what ONC returns
                transmit_power = int(float(transmit_power) * 10)

        # Initialize template
        template = ncs.template.Template(end_point)
        template_vars = ncs.template.Variables()
        template_vars.add("SERVICE_NAME", service.name + "-internal")
        template_vars.add("ROUTER", router)
        template_vars.add("DAC", dac)
        template_vars.add("FEC", fec)
        template_vars.add("TRANSMIT_POWER", transmit_power)
        template_vars.add("GRID_TYPE", grid_type)
        template_vars.add("FREQUENCY", frequency)
        template_vars.add("PLUGGABLE_PID", capability_pid)
        template_vars.add("FLEX_ID", flex_id)
        template_vars.add("PLATFORM_PID", platform_pid)

        # Determine if LSA deploy
        if Utils.is_lsa(root):
            template_vars.add("NSO_DEVICE", Utils.safe_get_remote_nso(self.log, router))
            self.log.info("LSA RFS_NODE")

        template.apply("cisco-ron-cfp-using-cisco-zr-template", template_vars)

        # populate operational data
        ron_data = root.cisco_ron_cfp__ron.ron_ml[service.name].ron_data
        ron_data.frequency = frequency
        ron_data.end_point[end_point.end_point_device].transmit_power = transmit_power

        # Check if dco meta-data is available. If yes update the ron-data
        key = (service.name + "-internal", router)
        if Utils.is_lsa(root):
            if key in root.cisco_ron_cfp__ron.remote_local_data.dco:
                for interface in root.cisco_ron_cfp__ron.remote_local_data.dco[key].interface:
                    ep_int = end_point_data.end_point_interface.create(interface.index)
                    ep_int.name = interface.name
        else:
            l_user = common_utils.get_local_user()
            with ncs.maapi.single_read_trans(l_user, StrConstants.system) as th:
                l_root = ncs.maagic.get_root(th)
                if key in l_root.cisco_zr_cfp__zr.dco:
                    for interface in l_root.cisco_zr_cfp__zr.dco[key].metadata.interface:
                        ep_int = end_point_data.end_point_interface.create(interface.index)
                        ep_int.name = interface.name

        return opaque_dict
