# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
import ncs.maagic as maagic
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import (
    UserErrorException, ServiceException,
    DeviceConfigException, CustomTemplateException
)
from cisco_tsdn_core_fp_common import utils as TsdnUtils
import traceback
from core_fp_common import instrumentation
import logging


class FlatL2vpnValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.remote_site_validation_callpoint, tctx, kp)


class FlatL2vpnCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.remote_site_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        opaque = dict(proplist)
        opaque["VALIDATION_ERROR"] = ""
        try:
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

            localhost = "127.0.0.1"  # NOSONAR
            # if operation is create or update, check interfaces on device & validate
            if (op == _ncs.dp.NCS_SERVICE_CREATE) or (op == _ncs.dp.NCS_SERVICE_UPDATE):
                service = maagic.get_node(maagic.get_trans(root), kp)
                self.log.info(f"Pre-Mod for service: {service.name} , operation: {op}")

                l2vpn_validation_enabled = root.\
                    cisco_flat_L2vpn_fp_internal_common__cfp_configurations.\
                    l2vpn_validation_enabled

                opaque["DEVICE_TRANS_INVALIDATED"] = "false"
                if op == _ncs.dp.NCS_SERVICE_UPDATE:
                    is_redeploy = Utils.diff_iterate(maagic.get_trans(root))

                    if is_redeploy:
                        # Check if device invalidated
                        if "INVALIDATED" == root.ncs__devices.device[service.pe].state.\
                                last_transaction_id:
                            opaque["DEVICE_TRANS_INVALIDATED"] = "true"
                else:
                    is_redeploy = False

                self.log.info("Pre-Mod for remote-site service is redeploy: " +
                              f"{is_redeploy}")
                self.log.info(f"L2VPN remote-site pre_mod internal Opaque: {opaque}")

                if service.flat_L2vpn_evpn_vpws:
                    # Update or generate new entry for l2vpn-rr-parent-route-policy Service
                    # Get attach-point
                    parent_rr_route_policy = service.\
                        flat_L2vpn_evpn_vpws.remote_site.sr_te.odn.\
                        attach_point.parent_rr_route_policy

                    if parent_rr_route_policy:
                        # Get device impl class
                        device = service.flat_L2vpn_evpn_vpws.remote_site.pe
                        device_impl_list = root.\
                            cisco_flat_L2vpn_fp_internal_common__cfp_configurations.\
                            dynamic_device_mapping

                        device_impl_class = Utils.\
                            get_device_impl_default_class(self, root, service,
                                                          service.pe, device_impl_list)
                        # Create/Update l2vpn-rr-parent-route-policy
                        Utils.create_parent_route_policy_service(self, str(parent_rr_route_policy),
                                                                 root, device, device_impl_class)
                        # Store l2vpn-rr-parent-route-policy key in proplist for use in post-mod
                        opaque["PARENT_RP_KEY"] = f"{str(parent_rr_route_policy)} {device}"

                if l2vpn_validation_enabled:
                    # Remote
                    if service.service_type == "p2p":
                        site = service.flat_L2vpn_p2p.remote_site
                    else:
                        site = service.flat_L2vpn_evpn_vpws.remote_site

                    if (site.exists() and
                        root.devices.device[service.pe].address != localhost and
                            site.if_type != "Loopback"):
                        router = Utils.get_device_impl_default_class(
                            self, root, service, service.pe,
                            root.cisco_flat_L2vpn_fp_internal_common__cfp_configurations
                            .dynamic_device_mapping
                        )

                        service_interface = str(site.if_type) + str(site.if_id)

                        try:
                            if not router.check_if_interface_exists(root, site,
                                                                    str(site.if_type),
                                                                    str(site.if_id)):
                                raise UserErrorException(
                                    self.log, StatusCodes.INTERFACE_NOT_FOUND
                                ).set_context(
                                    "Interface Validation",
                                    "Provided interface not present on remote site",
                                ).add_state(
                                    "Service", service.name
                                ).add_state(
                                    "Interface", service_interface
                                ).add_state(
                                    "Device", service.pe
                                ).finish()

                        except AttributeError as e:
                            raise UserErrorException(
                                self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                            ).set_context(
                                "Dynamic Method Error",
                                "check_if_interface_exists method is missing "
                                "from multi-vendor python class",
                            ).add_state(
                                "Service", service.name
                            ).add_state(
                                "Remote Site", service.pe
                            ).finish()

        except Exception as e:
            traceback.print_exc()
            opaque["VALIDATION_ERROR"] = str(e)

        return list(opaque.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.remote_site_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):

        opaque = dict(proplist)

        # If op is DELETE or UPDATE, cleanup l2vpn-rr-parent-route-policy if there are no more
        # references
        if op == _ncs.dp.NCS_SERVICE_DELETE or op == _ncs.dp.NCS_SERVICE_UPDATE:
            # Grab Parent RP key from opaque
            parent_rp_key = opaque.get("PARENT_RP_KEY", "")
            if parent_rp_key:
                # Define path
                parent_rp_kp = f"/l2vpn-rr-parent-route-policy{{{parent_rp_key}}}"
                th = maagic.get_trans(root)
                try:
                    # Cleanup Parent RP Entry if there are no more local-route-policies defined
                    if th.exists(parent_rp_kp):
                        # Check if parent rp service has any local route policies defined
                        parent_rp_node = maagic.get_node(th, parent_rp_kp)
                        if len(parent_rp_node.local_route_policy) == 0:
                            self.log.info("Parent RP has no local-route-policy definitions, "
                                          "cleaning up")
                            # Delete parent route policy
                            th.delete(parent_rp_kp)
                except Exception as e:
                    # TODO : Raise alarm when cleanup fails, Python API does not support user alarms
                    traceback.print_exc()
                    self.log.error(e)


class FlatL2vpnSelfCallBack(ncs.application.NanoService):

    """
    NanoService callback handler for flat-L2vpn service remote-site
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.remote_site_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None
            if state == "cisco-flat-L2vpn-fp-remote-site-nano-services:config-apply":
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

    def _create_config_apply(self, tctx, root, service, plan, component, state, opaque, comp_vars):
        if service.service_type == "p2p":
            site = service.flat_L2vpn_p2p.remote_site
        else:
            site = service.flat_L2vpn_evpn_vpws.remote_site
            # Configure parent rr route policy service with local route policy
            Utils.config_parent_route_policy_service(self, site)

        try:
            router = Utils.get_device_impl_default_class(
                self, root, service, site.pe,
                root.cisco_flat_L2vpn_fp_internal_common__cfp_configurations
                .dynamic_device_mapping
            )
        except UserErrorException:
            raise
        except Exception as e:
            raise ServiceException(
                self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Device Mapping", "Error retrieving Dynamic Device class"
            ).add_state(
                "Device", site.pe
            ).add_state(
                "Service", service.name
            ).finish()

        try:
            try:
                router.conf_l2vpn(site, False)
            except AttributeError as e:
                raise UserErrorException(
                    self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                ).set_context(
                    "Dynamic Method Error",
                    "conf_l2vpn method is missing from multi-vendor python class",
                ).add_state(
                    "Remote Site", site.pe
                ).add_state(
                    "Service", service.name
                ).finish()

            try:
                template_name = router.get_interface_shutdown_template()
                if template_name:
                    shutdown_service = root.\
                        core_fp_delete_tag_service__core_fp_delete_shutdown_service.\
                        create(site.pe, site.if_type, site.if_id)
                    shutdown_service.template_name = template_name
            except AttributeError as e:
                raise UserErrorException(
                    self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                ).set_context(
                    "Dynamic Method Error", "get_interface_shutdown_template method \
                    is missing from multi-vendor python class",
                ).add_state(
                    "Remote Site", site.pe
                ).add_state(
                    "Service", service.name
                ).finish()

        except UserErrorException:
            raise
        except Exception as e:
            raise DeviceConfigException(
                self.log, StatusCodes.CONFIG_FAILURE, str(e)
            ).set_context(
                "Configuration Error", "Could not apply service config on remote site"
            ).add_state(
                "Remote Site", site.pe
            ).add_state(
                "Service", service.name
            ).finish()

        # Apply custom-template from global level
        if service.custom_template:
            TsdnUtils.apply_custom_template(self, root, service, service.pe,
                                            StatusCodes.CUSTOM_TEMPLATE_ERROR,
                                            CustomTemplateException)

        # Apply custom-template at site level
        if site.custom_template:
            TsdnUtils.apply_custom_template(self, root, site, site.pe,
                                            StatusCodes.CUSTOM_TEMPLATE_ERROR,
                                            CustomTemplateException)

        return opaque

    def _create_ready(self, _tctx, root, service, plan, component, state, opaque, comp_vars):

        state_node = plan.component[component].state[state]
        opaque = dict(opaque)
        # First check if there is plan error in internal, set it
        if "true" == opaque.get("DEVICE_TRANS_INVALIDATED"):
            self.log.info(f"Device Transaction for service {service.name} device {service.pe} "
                          "is invalidated, marking ready failed")
            state_node.status = "failed"
            return list(opaque.items())

        # Update timestamp for CQ service updates (with no device change) to re-deploy NB service to
        # mark plan reached
        TsdnUtils.update_state_when_timestamp(self, component, state_node, "from internal plan")

        return list(opaque.items())
