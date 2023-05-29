# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs

from . import utils as Utils
from .constants import StrConstants as str_con
from .optical_errors import (
    OpticalCfpException,
    UserError,
    DeviceConfigException,
    CustomTemplateException,
)
from cisco_ron_core_fp_common import utils as RonUtils
from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes

# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class OpticalConnectivityCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        self.log.info("ENTRY_POINT for {} at pre_mod of Connectivity Service, operation: {}"
                      .format(RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)))
        proplist = dict(proplist)
        if proplist.get("OPER_DATA_FETCHED") is None:
            proplist["OPER_DATA_FETCHED"] = "false"
        try:
            ## If operation is create or update, check interface Loopback on device
            if op != _ncs.dp.NCS_SERVICE_DELETE:
                # Get service
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                proplist["UUID"] = service.uuid
                proplist["CONTROLLER"] = service.controller
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
                            "Keypath", str(kp)
                        ).finish()

                else:
                    proplist["DEVICE_TRANS_INVALIDATED"] = "false"
                    self.is_redeploy = False
                    th = ncs.maagic.get_trans(root)
                    th.diff_iterate(self.diter, 0)

                    if self.is_redeploy:
                        self.log.info(AsciiArt.caterpillar)
                        ## Check if device invalidated
                        if ("INVALIDATED" == root.ncs__devices.device[service.controller].
                                state.last_transaction_id):
                            proplist["DEVICE_TRANS_INVALIDATED"] = "true"
                    else:
                        self.log.info(AsciiArt.wave)

                try:
                    Utils.get_device_impl_default_class(root, service, service.controller, self.log)
                except Exception as e:
                    raise UserError(
                        self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Device Mapping",
                        "Error retrieving Dynamic Device class",
                    ).add_state(
                        "Device", service.controller
                    ).add_state(
                        "Service", str(service._path)
                    ).finish()

                self.log.info(
                    "Connectivity Service pre_mod internal Opaque: {}".format(proplist)
                )
            else:
                self.log.info(AsciiArt.blackwidow)
        except UserError:
            self.log.error(AsciiArt.roadblock)
            raise
        except Exception as exp1:
            self.log.error(AsciiArt.roadblock)
            raise OpticalCfpException(
                self.log, StatusCodes.OP_FP_PRE_MOD_CALLBACK_ERROR, str(exp1)
            ).set_context(
                "pre-modification", "Failed to process pre-modification call-back"
            ).add_state(
                "service", str(kp)
            ).finish()

        self.log.info("EXIT_POINT for {} at pre_mod of Connectivity Service, operation: {}"
                      .format(RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)))
        return list(proplist.items())

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info("Service create(service=", service._path, ")")
        return proplist

    def diter(self, keypath, op, oldv, newv):
        is_key_path0_tuple = isinstance(keypath[0], tuple)
        key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])

        # Check if redeploy
        if not is_key_path0_tuple and key_path0 == "re-deploy-counter":
            self.is_redeploy = True
            return ncs.ITER_STOP
        else:
            return ncs.ITER_RECURSE

    @ncs.application.Service.post_modification
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        self.log.info("ENTRY_POINT for {} at post_mod of Connectivity Service, operation: {}"
                      .format(RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)))
        self.log.info(AsciiArt.hibernate)
        self.log.info("EXIT_POINT for {} at post_mod of Connectivity Service, operation: {}"
                      .format(RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)))
        return proplist


class OpticalConnectivity(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    def cb_nano_create(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        self.log.info("ENTRY_POINT for nano endpoint create {} {} at {} of Connectivity Service"
                      .format(service.name, service.controller, state))
        opaque = dict(opaque)
        new_opaque = None
        if state == str_con.state_config_apply:
            try:
                new_opaque = self._create_config_apply(
                    tctx, root, service,
                    plan, component, state,
                    opaque, comp_vars)
            except (CustomTemplateException, UserError):
                self.log.error(AsciiArt.roadblock)
                raise
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise OpticalCfpException(
                    self.log, StatusCodes.OPTICAL_FP_CONFIG_ERROR, str(exp)
                ).set_context(
                    "Configuration Error", "Could not initiate connectivity-service"
                ).add_state(
                    "Controller", service.controller
                ).add_state(
                    "Service", str(service._path)
                ).finish()
        elif state == str_con.state_installed:
            try:
                new_opaque = self._create_installed(
                    tctx, root, service,
                    plan, component, state,
                    opaque, comp_vars)
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise OpticalCfpException(
                    self.log, StatusCodes.OPTICAL_FP_INSTALL_ERROR, str(exp)
                ).set_context(
                    "Operation Error",
                    "Error during connectivity-service installed state callback",
                ).add_state(
                    "Controller", service.controller
                ).add_state(
                    "Service", str(service._path)
                ).finish()
        elif state == str_con.ncs_ready:
            try:
                new_opaque = self._create_ready(
                    tctx, root, service,
                    plan, component, state,
                    opaque, comp_vars)
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise OpticalCfpException(
                    self.log, StatusCodes.OPTICAL_FP_READY_ERROR, str(exp)
                ).set_context(
                    "Ready Error",
                    "Could not move connectivity-service to ready reached",
                ).add_state(
                    "Controller", service.controller
                ).add_state(
                    "Service", str(service._path)
                ).finish()

        self.log.info("EXIT_POINT for nano endpoint create {} {} at {} of Connectivity Service"
                      .format(service.name, service.controller, state))
        if new_opaque is None:
            return list(opaque.items())

        return list(new_opaque.items())

    @ncs.application.NanoService.delete
    def cb_nano_delete(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        self.log.info("ENTRY_POINT for nano endpoint delete {} {} at {} of Connectivity Service"
                      .format(service.name, service.controller, state))
        opaque = dict(opaque)
        new_opaque = None
        if state == str_con.ncs_init:
            try:
                new_opaque = self._delete_init(
                    tctx, root, service, plan, component, state, opaque, comp_vars
                )
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                raise OpticalCfpException(
                    self.log, StatusCodes.OPTICAL_FP_INIT_RB_ERROR, str(exp)
                ).set_context(
                    "Self Init", "Failed to process delete init call-back"
                ).add_state(
                    "service", service._path
                ).finish()

        self.log.info("EXIT_POINT for nano endpoint delete {} {} at {} of Connectivity Service"
                      .format(service.name, service.controller, state))

        if new_opaque is None:
            return list(opaque.items())

        return list(new_opaque.items())

    def _create_config_apply(self, tctx, root, service, plan, component, state, opaque, comp_vars):

        controller = Utils \
            .get_device_impl_default_class(root, service, service.controller, self.log)

        try:
            controller.conf_connectivity_service(service)
        except AttributeError as e:
            raise UserError(
                self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Method Error",
                "conf_connectivity_service method is missing from "
                "multi-vendor python class",
            ).add_state(
                "Controller", service.controller
            ).finish()
        except Exception as e:
            raise DeviceConfigException(
                self.log, StatusCodes.CONFIG_FAILURE, str(e)
            ).set_context(
                "Configuration Error", "Could not apply service config on controller"
            ).add_state(
                "Controller", service.controller
            ).add_state(
                "Service", str(service._path)
            ).finish()

        ## Apply custom-template
        if service.custom_template:
            RonUtils.apply_custom_template(
                self,
                root,
                service,
                service.controller,
                StatusCodes.CUSTOM_TEMPLATE_ERROR,
                CustomTemplateException,
            )

        return opaque

    def _create_installed(self, _tctx, root, service, plan, component, state, opaque, comp_vars):
        state_node = plan.component[component].state[state]
        if (service.uuid in root.tapi_common__context.tapi_connectivity__connectivity_context
                .connectivity_service_oper_data):
            service_oper = root.tapi_common__context.tapi_connectivity__connectivity_context. \
                connectivity_service_oper_data[service.uuid]
            last_state = None
            for state in service_oper.lifecycle_state:
                last_state = state
            self.log.info(f"last_state is {last_state}")

            if last_state.error:
                state_node.status = "failed"
                plan.component[component].status_code = last_state.code
                plan.failed.create()
                plan.error_info.create()
                plan.error_info.message = "Connectivity service"\
                                          + f" lifecycle-state error: {last_state.error}"
            elif last_state.state != str_con.installed:
                state_node.status = "not-reached"
        else:
            state_node.status = "not-reached"
        return opaque

    def _create_ready(self, _tctx, root, service, plan, component, state, opaque, comp_vars):

        state_node = plan.component[component].state[state]
        ## First check if there is plan error in internal, set it
        if "true" == opaque.get("DEVICE_TRANS_INVALIDATED"):
            # TODO Should we assign error code ?
            self.log.info(f"Device Transaction for service {service.name}"
                          f" device {service.controller} is invalidated, marking ready failed")
            state_node.status = "failed"
            return opaque

    def _delete_init(self, _tctx, root, service, plan, component, state, opaque, comp_vars):
        uuid = opaque["UUID"]
        state_node = plan.component[component].state[state]
        # We should wait until the delete notification is recieved and the oper is cleared
        if (uuid in root.tapi_common__context.tapi_connectivity__connectivity_context.
                connectivity_service_oper_data):
            oper = root.tapi_common__context.tapi_connectivity__connectivity_context. \
                connectivity_service_oper_data[uuid]
            self.log.info(f"Waiting for service oper {oper._path} to be deleted")
            state_node.status = "reached"

        return opaque
