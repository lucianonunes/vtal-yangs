# -*- mode: python; python-indent: 4 -*-
from enum import Enum, auto

import _ncs
import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from .ron_ml_errors import UserError
from .constants import StrConstants
from .namespaces.ciscoRonCfp_ns import ns as roncfp_ns

from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes

# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class ServiceState(Enum):
    CLEAR = auto()
    POST_ACTION_DONE = auto()
    ONC_SERVICE_DONE = auto()
    ZR_CONFIGURED = auto()


def validate_line_port_reuse(root, service, log):
    """
    Checks if line-port of service to be created is not already in use
    """
    th = ncs.maagic.get_trans(root)
    for endpoint in service.end_point:
        kp = f"/cisco-ron-cfp:ron/inter-layer-link-oper" \
             f"{{{endpoint.end_point_device} " \
             f"{endpoint.terminal_device_optical.line_port}}}"
        try:
            inter_layer_oper_data = ncs.maagic.get_node(th, kp)
            if service.name != inter_layer_oper_data.ron_ml_service:
                raise UserError(
                    log, StatusCodes.LINE_PORT_ALREADY_IN_USE
                ).set_context(
                    "Validation",
                    "Line Port {} is already in use".format(
                        inter_layer_oper_data.line_port
                    ),
                ).add_state(
                    "Router", str(inter_layer_oper_data.end_point_device)
                ).add_state(
                    "Service using requested line-port", str(inter_layer_oper_data.ron_ml_service)
                ).finish()
        except KeyError as e:
            # Happy Path
            # A non existent keypath argument for ncs.maagic.get_node(th, kp) throws a KeyError
            # raise KeyError('%s not in %s' % (keystr, self._path))
            log.debug(e)


def validate(root, service, log, op, caller):
    max_index = 0
    if service.mode == "muxponder":
        if service.bandwidth == "400":
            max_index = 4
        elif service.bandwidth == "300":
            max_index = 3
        elif service.bandwidth == "200":
            max_index = 2
        elif service.bandwidth == "100":
            max_index = 1
    else:
        max_index = 1

    no_domain_set = True
    for end_point in service.end_point:
        if end_point.terminal_device_packet.exists():
            no_domain_set = False
            for interface in end_point.terminal_device_packet.interface:
                if interface.index >= max_index:
                    raise UserError(
                        log, StatusCodes.INTERFACE_INDEX_OUT_OF_BOUND
                    ).set_context(
                        "Validation",
                        "Interface index is out-of-bound, index can only be upto {}".format(
                            max_index - 1
                        ),
                    ).add_state(
                        "Router", str(end_point.end_point_device)
                    ).add_state(
                        "Service", str(service._path)
                    ).finish()

    if service.ols_domain.exists():
        no_domain_set = False
        controllers = set()
        for end_point in service.end_point:
            if (
                not (
                    end_point.end_point_device,
                    end_point.terminal_device_optical.line_port,
                )
                in root.cisco_ron_cfp__ron.inter_layer_link
            ):
                raise UserError(
                    log, StatusCodes.INTER_LAYER_LINK_NOT_FOUND
                ).set_context(
                    "Validation",
                    "Cannot find inter-layer link entry for {} {}".format(
                        end_point.end_point_device,
                        end_point.terminal_device_optical.line_port,
                    ),
                ).add_state(
                    "Managed Link", str("inter-layer-link")
                ).add_state(
                    "Service", str(service._path)
                ).finish()
            else:
                managed_link = root.cisco_ron_cfp__ron.inter_layer_link[
                    end_point.end_point_device,
                    end_point.terminal_device_optical.line_port,
                ]

                optical_controller = root.cisco_ron_cfp__ron.default_optical_controller
                if managed_link.ols_domain.optical_controller is not None:
                    optical_controller = managed_link.ols_domain.optical_controller

                if optical_controller is None:
                    raise UserError(
                        log, StatusCodes.OPTICAL_CONTROLLER_NOT_CONFIGURED
                    ).set_context(
                        "Validation",
                        "Optical Controller is not configured for {} {}".format(
                            end_point.end_point_device,
                            end_point.terminal_device_optical.line_port,
                        ),
                    ).add_state(
                        "Service", str(service._path)
                    ).finish()
                else:
                    controllers.add(optical_controller)
        # End of for service.end_point:
        if len(controllers) > 1:
            raise UserError(log, StatusCodes.MULTIPLE_CONTROLLERS_FOUND).set_context(
                "Validation",
                "multiple optical controllers detected"
                " for the link stated on the service",
            ).add_state("Service", str(service._path)).finish()

    if op == _ncs.dp.NCS_SERVICE_CREATE:
        validate_line_port_reuse(root, service, log)

    if no_domain_set:
        raise UserError(log, StatusCodes.NO_DOMAIN_SET).set_context(
            "Validation", "No domain selection made"
        ).add_state("Service", str(service._path)).finish()

    # Kind of a guard from unwanted edits.
    if not (
        op == _ncs.dp.NCS_SERVICE_CREATE or caller.clear_rollback or caller.is_redeploy
    ) and (caller.affects_cs_deployment or caller.affects_init_post_action):
        log.debug(
            f" payload changes affects-cs-deployments : {caller.affects_cs_deployment} "
            f"and affects-init-post-action {caller.affects_init_post_action}"
        )
        state = ServiceState.CLEAR
        opaque = service.private.property_list.property
        # Change that will affect either init-post-action or the ONC deployment
        # What is the ron-ml service state?
        plan = root.cisco_ron_cfp__ron.ron_ml_plan
        if service.name in plan:
            ron_plan = plan[service.name].plan
            self_comp = (StrConstants.ncs_self, StrConstants.self_cn)
            ron_self_init_state = ron_plan.component[self_comp].state[
                StrConstants.ncs_init
            ]
            if ron_self_init_state.post_action_status == "create-reached":
                state = ServiceState.POST_ACTION_DONE
                optical_comp = (
                    StrConstants.com_optical_controller,
                    StrConstants.optical_cn,
                )
                if (
                    optical_comp in ron_plan.component
                    and StrConstants.cs_done_key in opaque
                ):
                    state = ServiceState.ONC_SERVICE_DONE
                # Check ZR only case
                if optical_comp not in ron_plan.component:
                    for plan_component in ron_plan.component:
                        if (
                            plan_component.type == StrConstants.com_router
                            and plan_component.state[
                                StrConstants.state_conf_apply].status == "reached"
                        ):
                            state = ServiceState.ZR_CONFIGURED
                            break
        log.debug(f"Service state {state}")
        if state == ServiceState.POST_ACTION_DONE and caller.affects_init_post_action:
            raise UserError(log, StatusCodes.SERVICE_PAST_INIT_STATE).set_context(
                "Validation", "Service is past init-state"
            ).add_state("Service", str(service._path)).finish()
        elif state == ServiceState.ONC_SERVICE_DONE and (
            caller.affects_init_post_action or caller.affects_cs_deployment
        ):
            raise UserError(log, StatusCodes.CHANGES_AFFECT_ONC_DEPLOYMENT).set_context(
                "Validation", "Service is past optical service state"
            ).add_state("Service", str(service._path)).finish()
        elif state == ServiceState.ZR_CONFIGURED and (
            caller.affects_init_post_action or caller.affects_cs_deployment
        ):
            raise UserError(log, StatusCodes.CHANGES_AFFECT_ZR_DEPLOYMENT).set_context(
                "Validation", "Service has already configured at least one ZR device"
            ).add_state("Service", str(service._path)).finish()


class RonMlValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        self.log.info("ENTRY_POINT for RON ML validation: {}".format(kp))
        self.log.info(AsciiArt.castle)
        try:
            ## Check for child interfaces matching bandwidth and mode
            t = maapi.Maapi().attach(tctx)
            # Next get root node
            root = maagic.get_root(t)
            # Get service
            service = maagic.get_node(maagic.get_trans(root), kp)

            self.affects_cs_deployment = False
            self.affects_init_post_action = False
            self.clear_rollback = False
            self.is_redeploy = False

            op = _ncs.dp.NCS_SERVICE_CREATE
            # diff iterate in validation always see service as modified, and we cannot differentiate
            # betwee create and modifiy, so thie opaque is a workaround.
            opaque = service.private.property_list.property
            if StrConstants.past_create_state_key in opaque:
                op = _ncs.dp.NCS_SERVICE_UPDATE
                # Monitor payload changes
                t = maagic.get_trans(root)
                t.diff_iterate(self.iter, 0)

            # Validate
            validate(root, service, self.log, op, self)

        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            self.log.error(exp)
            raise

        self.log.info(AsciiArt.castle)
        self.log.info("EXIT_POINT for RON ML validation: {}".format(kp))
        return _ncs.OK

    # In future we should merge the diff-iter of service and validate.
    def iter(self, keypath, op, oldv, newv):
        idx = len(keypath) - 2
        tag = str(keypath[idx])
        # Interesting enough, we will see the entire diff of the service including
        # the stack-service,and device changes. We are interested in ron-ml only,
        # hence the below check.
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
                idx = idx - 2  # Skip the key
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
                    return ncs.ITER_CONTINUE
            elif tag == roncfp_ns.cisco_ron_cfp_ron_data_:
                # Oper data continue
                return ncs.ITER_CONTINUE
            elif tag == roncfp_ns.cisco_ron_cfp_srlg_:
                # Any changes to srlg should be allowed
                return ncs.ITER_CONTINUE
            else:
                # change in circuit-id, grid-type, frequency, dac-rate
                self.affects_cs_deployment = True
                return ncs.ITER_CONTINUE

        return ncs.ITER_RECURSE
