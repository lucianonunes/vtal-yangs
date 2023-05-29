# -*- mode: python; python-indent: 4 -*-

import ncs
import _ncs

from . import utils as Utils
from .constants import StrConstants as str_con
from .zr_errors import (
    ZRCfpException,
    UserError,
    CustomTemplateException,
)
from cisco_ron_core_fp_common import utils as RonUtils
from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes
# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class ZrDcoCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            "ENTRY_POINT for {} at pre_mod of ZR-DCO, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        proplist = dict(proplist)
        try:

            if (op == _ncs.dp.NCS_SERVICE_CREATE) or (op == _ncs.dp.NCS_SERVICE_UPDATE):
                # Get service
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                ## If op is create, validate status code mapping is loaded
                if op == _ncs.dp.NCS_SERVICE_CREATE:
                    self.log.info(AsciiArt.torpedo)
                    status_code_cfp = (
                        root.cfp_common_status_codes__status_codes.core_function_pack
                    )
                    if not status_code_cfp.exists("RON"):
                        raise UserError(
                            self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                        ).set_context(
                            "Status Code", "Missing RON status code mapping"
                        ).add_state(
                            "Keypath", status_code_cfp._path
                        ).finish()

                else:

                    proplist["DEVICE_TRANS_INVALIDATED"] = "false"
                    self.is_redeploy = False
                    th = ncs.maagic.get_trans(root)
                    th.diff_iterate(self.diter, 0)

                    if self.is_redeploy:
                        self.log.info(AsciiArt.caterpillar)
                        ## Check if device invalidated
                        if (
                            "INVALIDATED"
                            == root.ncs__devices.device[
                                service.router
                            ].state.last_transaction_id
                        ):
                            proplist["DEVICE_TRANS_INVALIDATED"] = "true"
                    else:
                        self.log.info(AsciiArt.wave)

                # Validation
                try:
                    Utils.get_device_impl_default_class(
                        root, service, service.router, self.log
                    )
                except ZRCfpException as e:
                    raise UserError(
                        self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Device Mapping",
                        "Error retrieving Dynamic Device class",
                    ).add_state(
                        "Device", service.router
                    ).add_state(
                        "Service", str(service._path)
                    ).finish()

                ## TODO: Do we need to validate the interface?
                # if not self.is_redeploy and self.is_service_val_updated:
                # try:
                #     if not router.check_if_interface_exists(root, service):
                #         raise UserErrorException(self.log,
                #                                  StatusCodes.INTERFACE_NOT_FOUND) \
                #             .set_context("Interface Validation",
                #                          "Provided interface not present on device") \
                #             .add_state("Interface", 'controller' + service.port) \
                #             .add_state("Device", service.router).finish()
                # except AttributeError as e:
                #     raise UserErrorException(self.log,
                #                              StatusCodes.DYNAMIC_METHOD_NOT_FOUND,
                #                              str(e)). \
                #         set_context("Dynamic Method Error",
                #                     "check_if_interface_exists method is missing from "
                #                     "multi-vendor python class") \
                #         .add_state("Device", service.router).finish()

                self.log.info("ZR-DCO pre_mod internal Opaque: {}".format(proplist))
            else:
                self.log.info(AsciiArt.blackwidow)

        except UserError:
            self.log.error(AsciiArt.roadblock)
            raise
        except Exception as exp1:
            self.log.error(AsciiArt.roadblock)
            raise ZRCfpException(
                self.log, StatusCodes.ZR_FP_PRE_MOD_CALLBACK_ERROR, str(exp1)
            ).set_context(
                "pre-modification", "Failed to process pre-modification call-back"
            ).add_state(
                "service", str(kp)
            ).finish()

        return list(proplist.items())

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info("Service create(service=", service._path, ")")
        return proplist

    def diter(self, keypath, op, oldv, newv):
        is_key_path0_tuple = isinstance(keypath[0], tuple)
        key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])

        if not is_key_path0_tuple:
            # First check if redeploy
            if key_path0 == "re-deploy-counter":
                self.is_redeploy = True
                return ncs.ITER_STOP
            if (
                len(keypath) == 4
                and isinstance(keypath[-3], tuple)
                and key_path0 == "port"
            ):
                self.is_service_val_updated = True
                return ncs.ITER_STOP
        return ncs.ITER_RECURSE

    @ncs.application.Service.post_modification
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            "ENTRY_POINT for {} at post_mod of ZR-DCO, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        self.log.info(AsciiArt.hibernate)
        self.log.info(
            "EXIT_POINT for {} at post_mod of ZR-DCO, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        return proplist


class ZrDco(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    def cb_nano_create(
        self, tctx, root, service, plan, component, state, opaque, comp_vars
    ):
        self.log.info(
            "ENTRY_POINT for nano dco create {} {} at {} of ZR-DCO".format(
                service.name, service.router, state
            )
        )
        new_opaque = None
        if state == str_con.state_config_apply:
            try:

                new_opaque = self._create_config_apply(
                    tctx, root, service, plan, component, state, opaque, comp_vars
                )
            except (CustomTemplateException, UserError):
                self.log.error(AsciiArt.roadblock)
                raise
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise ZRCfpException(
                    self.log, StatusCodes.ZR_FP_CONFIG_ERROR, str(exp)
                ).set_context(
                    "Configuration Error", "Could not apply service config on router"
                ).add_state(
                    "Router", service.router
                ).add_state(
                    "Service", str(service._path)
                ).finish()
        elif state == str_con.ncs_ready:
            try:
                new_opaque = self._create_ready(
                    tctx, root, service, plan, component, state, opaque, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise ZRCfpException(
                    self.log, StatusCodes.ZR_FP_READY_ERROR, str(exp)
                ).set_context(
                    "Configuration Error", "Could not move dco to ready state"
                ).add_state(
                    "Router", service.router
                ).add_state(
                    "Service", str(service._path)
                ).finish()

        self.log.info(
            "EXIT_POINT for nano dco create {} {} at {} of ZR-DCO".format(
                service.name, service.router, state
            )
        )
        if new_opaque is None:
            return opaque

        return new_opaque

    def _create_config_apply(self, tctx, root, service, plan, component, state, opaque, comp_vars):

        opaque = dict(opaque)
        router = Utils.get_device_impl_default_class(root, service, service.router, self.log)

        try:
            router.conf_zr_dco()
        except AttributeError as e:
            raise UserError(self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)).set_context(
                "Dynamic Method Error",
                "conf_zr_dco method is missing from multi-vendor python class"). \
                add_state("Router", service.router).finish()
        ## Apply custom-template
        if service.custom_template:
            self.log.info("Applying ZR custom-templates")
            RonUtils.apply_custom_template(
                self,
                root,
                service,
                service.router,
                StatusCodes.CUSTOM_TEMPLATE_ERROR,
                CustomTemplateException,
            )
        else:
            self.log.info("No custom-templates for ZR")

        return list(opaque.items())

    def _create_ready(
        self, _tctx, root, service, plan, component, state, opaque, comp_vars
    ):

        state_node = plan.component[component].state[state]
        opaque = dict(opaque)
        ## First check if there is plan error in internal, set it
        if "true" == opaque.get("DEVICE_TRANS_INVALIDATED"):
            self.log.info(
                f"Device Transaction for service {service.name} device {service.router}"
                " is invalidated, marking ready failed"
            )
            state_node.status = "failed"
            return list(opaque.items())

        return list(opaque.items())
