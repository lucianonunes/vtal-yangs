# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
import cisco_sr_te_cfp_internal.utils as Utils
import ncs.maagic as maagic
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import (
    DeviceConfigException, CustomTemplateException,
    UserErrorException, ServiceException
)
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
import traceback
from core_fp_common import instrumentation
import logging


class SROdnValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.odn_internal_validation_callpoint, tctx, kp)


class SROdnCallback(ncs.application.Service):
    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.odn_internal_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        opaque = dict(proplist)
        opaque["VALIDATION_ERROR"] = ""
        try:
            # If op is create, validate status code mapping is loaded
            status_code_cfp = (root.cfp_common_status_codes__status_codes.core_function_pack)
            if op == _ncs.dp.NCS_SERVICE_CREATE and not status_code_cfp.exists("SR"):
                raise UserErrorException(
                    self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                ).set_context(
                    "Status Code", "Missing SR status code mapping"
                ).add_state(
                    "Keypath", str(kp)
                ).finish()
            if op == _ncs.dp.NCS_SERVICE_CREATE or op == _ncs.dp.NCS_SERVICE_UPDATE:
                service = maagic.get_node(maagic.get_trans(root), kp)
                opaque["DEVICE_TRANS_INVALIDATED"] = "false"
                if op == _ncs.dp.NCS_SERVICE_UPDATE:
                    is_redeploy = self.diff_iterate(maagic.get_trans(root)).is_redeploy

                    if is_redeploy:
                        # Check if device invalidated
                        if ("INVALIDATED" == root.ncs__devices.device[
                                service.head_end].state.last_transaction_id):
                            opaque["DEVICE_TRANS_INVALIDATED"] = "true"
                else:
                    is_redeploy = False

                self.log.info("Pre-Mod for internal odn service is "
                              f"redeploy: {is_redeploy}")
                self.log.info(f"ODN pre_mod internal Opaque: {opaque}")

                sr_validation_enabled = (root.cisco_sr_te_cfp_internal__cfp_configurations
                                         .sr_validation_enabled)
                if not is_redeploy and sr_validation_enabled:
                    device = service.head_end
                    # SRv6-TE is supported on XR 7.3.2 and 7.5.1 and above
                    if service.srv6.exists():
                        device_impl_list = root.cisco_sr_te_cfp_internal__cfp_configurations \
                            .dynamic_device_mapping
                        router = Utils.get_device_impl_default_class(self, root, service,
                                                                     service.head_end,
                                                                     device_impl_list)
                        router.validate_srv6_te(root, service, device)
        except Exception as e:
            traceback.print_exc()
            opaque["VALIDATION_ERROR"] = str(e)

        return list(opaque.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.odn_internal_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        pass

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


class SROdnHeadEndCallBack(ncs.application.NanoService):
    """
    NanoService callback handler for sr-odn service head-end
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.odn_internal_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):

        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None

            if state == "cisco-sr-te-cfp-sr-odn-nano-services:config-apply":
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

    def _create_config_apply(self, tctx, root, service, plan,
                             component, state, opaque, comp_vars):

        try:
            router = Utils.\
                get_device_impl_default_class(self, root, service, service.head_end, root
                                              .cisco_sr_te_cfp_internal__cfp_configurations
                                              .dynamic_device_mapping)
        except UserErrorException:
            raise
        except Exception as e:
            raise ServiceException(
                self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Device Mapping", "Error retrieving Dynamic Device class"
            ).add_state(
                "Head-end", service.head_end
            ).add_state(
                "Service", service.name
            ).finish()

        try:
            router.conf_sr_template(service.head_end)
        except AttributeError as e:
            raise UserErrorException(
                self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Method Error",
                "conf_sr_template method is missing from " "multi-vendor python class",
            ).add_state(
                "Head-end", service.head_end
            ).add_state(
                "Service", service.name
            ).finish()
        except Exception as e:
            raise DeviceConfigException(
                self.log, StatusCodes.CONFIG_FAILURE, str(e)
            ).set_context(
                "Configuration Error", "Could not apply service config on device"
            ).add_state(
                "Head-end", service.head_end
            ).add_state(
                "Service", service.name
            ).finish()

        # Apply ODN custom-templates
        if service.custom_template:
            TsdnUtils.apply_custom_template(self, root, service, service.head_end,
                                            StatusCodes.CUSTOM_TEMPLATE_ERROR,
                                            CustomTemplateException)

        return opaque

    def _create_ready(self, _tctx, root, service, plan,
                      component, state, opaque, comp_vars):

        state_node = plan.component[component].state[state]
        opaque = dict(opaque)
        # First check if there is plan error in internal, set it
        if "true" == opaque.get("DEVICE_TRANS_INVALIDATED"):
            self.log.info(f"Device Transaction for service {service.name} device "
                          f"{service.head_end} is invalidated, marking ready failed")
            state_node.status = "failed"
            return list(opaque.items())

        # Update timestamp for CQ service updates (with no device change)
        # to re-deploy NB service to mark plan reached
        TsdnUtils.update_state_when_timestamp(self, component, state_node, "from internal FP")

        return list(opaque.items())
