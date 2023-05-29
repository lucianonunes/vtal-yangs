# -*- mode: python; python-indent: 4 -*-
import re
import traceback
import ncs
import _ncs
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.rsvp_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.rsvp_te_fp_base_exception import (
    UserErrorException,
    ServiceException,
    DeviceConfigException,
    CustomTemplateException
)
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from core_fp_common import instrumentation
import logging
# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class RSVPTEValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.rsvp_te_validation_callpoint, tctx, kp)


class P2PTeTunnelCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.rsvp_te_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        proplist = dict(proplist)
        proplist["VALIDATION_ERROR"] = ""
        try:
            #  If op is create, validate status code mapping is loaded
            status_code_cfp = root.\
                cfp_common_status_codes__status_codes.core_function_pack
            if op == _ncs.dp.NCS_SERVICE_CREATE and not status_code_cfp.exists("RSVP-TE"):
                raise UserErrorException(
                    self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                ).set_context(
                    "Status Code", "Missing RSVP-TE status code mapping"
                ).add_state(
                    "Keypath", str(kp)
                ).finish()

            #  If operation is create or update, check interface Loopback on device
            if (op == _ncs.dp.NCS_SERVICE_CREATE) or (op == _ncs.dp.NCS_SERVICE_UPDATE):
                # Get service
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                proplist["DEVICE_TRANS_INVALIDATED"] = "false"
                if op == _ncs.dp.NCS_SERVICE_UPDATE:
                    th = ncs.maagic.get_trans(root)

                    if self.diff_iterate(th).is_redeploy:
                        #  Check if device invalidated
                        if ("INVALIDATED" == root.ncs__devices.device[service.head_end].state
                                .last_transaction_id):
                            proplist["DEVICE_TRANS_INVALIDATED"] = "true"

                try:
                    router = Utils.\
                        get_device_impl_default_class(self, root, service, service.head_end,
                                                      root.cisco_rsvp_te_fp__cfp_configurations
                                                      .dynamic_device_mapping)
                except Exception as e:
                    raise ServiceException(
                        self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Device Mapping",
                        "Error retrieving Dynamic Device class",
                    ).add_state(
                        "Device", service.head_end
                    ).add_state(
                        "Service", str(service._path)
                    ).finish()

                try:
                    loopback = router.get_interface_loopback(root, service)
                    #  If no interface loopback is set on the device, error out.
                    if loopback is None:
                        self.log.error(f"Loopback for head-end {service.head_end} not set")
                        raise UserErrorException(
                            self.log, StatusCodes.VALUE_NOT_SUPPORTED
                        ).set_context("Pre-modification", "Loopback not set").add_state(
                            "Head-end", str(service.head_end)
                        ).add_state(
                            "Service", str(service._path)
                        ).finish()

                    service_loopback = service.ipv4.unnumbered.loopback
                    # If loopback is provided in the service payload,
                    # and the value does not match on the device, error out.
                    if service_loopback is not None and service_loopback != loopback:
                        raise UserErrorException(
                            self.log, StatusCodes.VALUE_NOT_SUPPORTED
                        ).set_context(
                            "Pre-modification",
                            "Loopback on device does not match provided value.",
                        ).add_state(
                            "Head-end", str(service.head_end)
                        ).add_state(
                            "Service", str(service._path)
                        ).finish()

                    proplist["LOOPBACK"] = str(loopback)

                except AttributeError as e:
                    raise UserErrorException(
                        self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Method Error", "get_interface_loopback method is missing from "
                        "multi-vendor python class",
                    ).add_state(
                        "Head-end", service.head_end
                    ).finish()

                self.log.info(f"RSVP-TE pre_mod internal Opaque: {proplist}")

        except Exception as e:
            traceback.print_exc()
            proplist["VALIDATION_ERROR"] = str(e)

        return list(proplist.items())

    @staticmethod
    def diff_iterate(th) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Extract key_path0
            is_key_path0_tuple = isinstance(keypath[0], tuple)
            key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])
            # Check if redeploy
            if is_key_path0_tuple is False and key_path0 == "re-deploy-counter":
                self.is_redeploy = True
                return ncs.ITER_STOP
            else:
                return ncs.ITER_RECURSE

        diff_iter = DiffIterateWrapper(diter, is_redeploy=False)
        th.diff_iterate(diff_iter, 0)

        return diff_iter


class P2PTeSelfTunnel(ncs.application.NanoService):
    """
    NanoService callback handler for rsvp-te plan
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.rsvp_te_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        new_opaque = None

        if state == "ncs:ready":
            new_opaque = self._create_ready(tctx, root, service, plan,
                                            component, state, opaque, comp_vars)
        if new_opaque is None:
            return opaque

        return new_opaque

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
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready not-reached, "
                                  "setting self ready state to not-reached")
                    state_node.status = "not-reached"
                    return opaque
        return opaque


class P2PTeTunnel(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.rsvp_te_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None

            if state == "cisco-rsvp-te-fp-nano-plan-services:config-apply":
                new_opaque = self._create_config_apply(tctx, root, service, plan,
                                                       component, state, opaque, comp_vars)
            elif state == "ncs:init":
                new_opaque = self._create_init(tctx, root, service, plan,
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

    def _create_config_apply(self, tctx, root, service, plan,
                             component, state, opaque, comp_vars):
        # Get the value of loopback from opaque that was set in pre-mod
        opaque = dict(opaque)
        loopback = opaque.get("LOOPBACK")

        try:
            #  Will always be one node for inner service
            router = Utils.\
                get_device_impl_default_class(self, root, service, service.head_end,
                                              root.cisco_rsvp_te_fp__cfp_configurations
                                              .dynamic_device_mapping)
        except UserErrorException:
            raise
        except Exception as e:
            raise ServiceException(
                self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Device Mapping", "Error retrieving Dynamic Device class"
            ).add_state(
                "Device", service.head_end
            ).add_state(
                "Service", str(service._path)
            ).finish()

        try:
            router.conf_rsvp_te_tunnel_p2p(service, loopback)
        except AttributeError as e:
            raise UserErrorException(
                self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Method Error",
                "conf_rsvp te tunnel p2p  method is missing from "
                "multi-vendor python class",
            ).add_state(
                "Head-end", service.head_end
            ).finish()
        except Exception as e:
            raise DeviceConfigException(
                self.log, StatusCodes.CONFIG_FAILURE, str(e)
            ).set_context(
                "Configuration Error", "Could not apply service config on headend"
            ).add_state(
                "Head-end", service.head_end
            ).add_state(
                "Service", str(service._path)
            ).finish()

        #  Apply custom-template
        if service.custom_template:
            TsdnUtils.apply_custom_template(self, root, service, service.head_end,
                                            StatusCodes.CUSTOM_TEMPLATE_ERROR,
                                            CustomTemplateException)

        return list(opaque.items())

    def _create_init(self, _tctx, root, service, plan,
                     component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)

        #  Applying kicker for explicit-path changes redeploy
        for path_option in service.path_option:
            if path_option.explicit.name:
                vars = ncs.template.Variables()
                vars.add("NAME", service.name)
                vars.add("HEADEND", service.head_end)
                vars.add("PATHNAME", re.sub(r"\s+", "", path_option.explicit.name))
                template = ncs.template.Template(service)
                template.apply("cisco-rsvp-te-fp-explicit-path-name-kicker", vars)
            elif path_option.explicit.identifier:
                vars = ncs.template.Variables()
                vars.add("NAME", service.name)
                vars.add("HEADEND", service.head_end)
                vars.add("IDENTIFIER", path_option.explicit.identifier)
                template = ncs.template.Template(service)
                template.apply("cisco-rsvp-te-fp-explicit-path-identifier-kicker", vars)

        return opaque

    def _create_ready(self, _tctx, root, service, plan,
                      component, state, opaque, comp_vars):

        state_node = plan.component[component].state[state]
        opaque = dict(opaque)
        #  First check if there is plan error in internal, set it
        if "true" == opaque.get("DEVICE_TRANS_INVALIDATED"):
            self.log.info(f"Device Transaction for service {service.name} "
                          f"device {service.head_end} is invalidated, marking ready failed")
            state_node.status = "failed"
            return list(opaque.items())

        # Update timestamp for CQ service updates (with no device change) to re-deploy NB service to
        # mark plan reached
        TsdnUtils.update_state_when_timestamp(self, component, state_node, "from internal FP")

        return list(opaque.items())
