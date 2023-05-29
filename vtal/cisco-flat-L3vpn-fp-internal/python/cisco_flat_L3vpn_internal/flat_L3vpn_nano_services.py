# -*- mode: python; python-indent: 4 -*-
import ncs
from ncs import maagic
import _ncs
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import (
    UserErrorException,
    ServiceException,
    DeviceConfigException,
    CustomTemplateException
)
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
import traceback
from core_fp_common import instrumentation
import logging


class FlatL3vpnValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.l3vpn_internal_validation_callpoint, tctx, kp)


class FlatL3vpnCallBack(ncs.application.Service):
    device_attr_updated = None

    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.l3vpn_internal_servicepoint)
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

            localhost = "127.0.0.1"  # NOSONAR

            # if operation is create or update, check interfaces on device & validate
            if (op == _ncs.dp.NCS_SERVICE_CREATE) or (op == _ncs.dp.NCS_SERVICE_UPDATE):
                # Get service
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                # Get flag: Validate all endpoint interfaces are available on the endpoint
                l3vpn_validation_enabled = root.\
                    cisco_flat_L3vpn_fp_internal__cfp_configurations.l3vpn_validation_enabled
                # Get flag: Validate RD change will be accepted by endpoint
                l3vpn_device_val_enabled = (
                    root.cisco_flat_L3vpn_fp_internal__cfp_configurations
                    .l3vpn_device_error_validation_enabled)

                # Set default state vals
                opaque["DEVICE_TRANS_INVALIDATED"] = "false"

                # Iterate through trans diff to check if redeploy/update
                if op == _ncs.dp.NCS_SERVICE_UPDATE:
                    premod_diter = self.diff_iterate(root, service)
                    device_modified = premod_diter.device_modified
                    device_attr_updated = premod_diter.device_attr_updated

                    if not device_modified:
                        # Check if device invalidated
                        endpoint = service.endpoint[service.endpoint_name]
                        if ("INVALIDATED" == root.ncs__devices.device[endpoint.access_pe].
                                state.last_transaction_id):
                            opaque["DEVICE_TRANS_INVALIDATED"] = "true"
                else:
                    device_modified = True
                    device_attr_updated = set()

                self.log.info(f"Pre-Mod for service is device_modified: {device_modified}")
                self.log.info(f"L3VPN pre_mod internal Opaque: {opaque}")

                # Check for BGP existence on the device config
                if device_modified:
                    # Get endpoint node
                    endpoint = service.endpoint[service.endpoint_name]
                    if endpoint.as_no_from_device.exists():
                        bgp_as_no = Utils.get_bgp_as_from_device(self, root, service, endpoint)
                        if bgp_as_no == "":
                            raise UserErrorException(self.log, StatusCodes.BGP_NOT_CONFIGURED) \
                                .set_context("BGP AS Validation",
                                             "BGP routing process is not configured on the device")\
                                .add_state("Endpoint", endpoint.endpoint_name) \
                                .add_state("Keypath", str(kp)).finish()

                # Check flags and validate RD change will be accepted by endpoint
                if l3vpn_device_val_enabled and "route-distinguisher" in device_attr_updated:
                    # Get endpoint node
                    endpoint = service.endpoint[service.endpoint_name]
                    self.log.info("Check if value update allowed on "
                                  f"endpoint: {endpoint.access_pe}")
                    # Check if we can update RD on endpoint
                    can_update_val = Utils.can_update_on_device(self, root, service,
                                                                endpoint, "route-distinguisher")
                    if not can_update_val:
                        raise UserErrorException(
                            self.log, StatusCodes.GLOBAL_RD_VRF_ACTIVE
                        ).set_context(
                            "RD Edit Validation",
                            "RD cannot be changed or "
                            "removed while a vrf "
                            "address-family is Active",
                        ).add_state(
                            "Endpoint", endpoint.endpoint_name
                        ).add_state(
                            "Keypath", str(kp)
                        ).finish()

                # Check flags and validate all endpoint interfaces are available on the endpoint
                if device_modified and l3vpn_validation_enabled:
                    for endpoint in service.endpoint:
                        if (root.devices.device[endpoint.access_pe].address == localhost
                                or endpoint.if_type == "Loopback"):
                            continue
                        elif endpoint.if_type == "BVI":
                            router = Utils.get_device_impl_default_class(
                                self, root, service, endpoint.access_pe,
                                root.cisco_flat_L3vpn_fp_internal__cfp_configurations
                                .dynamic_device_mapping
                            )
                            for ac in endpoint.l2_attachment_circuit:
                                service_interface = str(ac.if_type) + str(ac.if_id)
                                try:
                                    if not router.check_if_interface_exists(root,
                                                                            endpoint,
                                                                            str(ac.if_type),
                                                                            str(ac.if_id)):
                                        raise UserErrorException(
                                            self.log, StatusCodes.INTERFACE_NOT_FOUND
                                        ).set_context(
                                            "Interface Validation",
                                            "Provided BVI interface not present "
                                            "on device",
                                        ).add_state(
                                            "Interface", service_interface
                                        ).add_state(
                                            "Device", endpoint.access_pe
                                        ).finish()

                                except AttributeError as e:
                                    raise UserErrorException(
                                        self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                                    ).set_context(
                                        "Dynamic Method Error",
                                        "check_if_interface_exists method is missing "
                                        "from multi-vendor python class",
                                    ).add_state(
                                        "Endpoint", endpoint.access_pe
                                    ).finish()
                            continue
                        else:
                            router = Utils.get_device_impl_default_class(
                                self, root, service, endpoint.access_pe,
                                root.cisco_flat_L3vpn_fp_internal__cfp_configurations
                                .dynamic_device_mapping
                            )
                            service_interface = str(endpoint.if_type) + str(endpoint.if_id)

                            try:
                                if not router.check_if_interface_exists(root, endpoint,
                                                                        str(endpoint.if_type),
                                                                        str(endpoint.if_id)):
                                    raise UserErrorException(
                                        self.log, StatusCodes.INTERFACE_NOT_FOUND
                                    ).set_context(
                                        "Interface Validation",
                                        "Provided interface not present on device",
                                    ).add_state(
                                        "Interface", service_interface
                                    ).add_state(
                                        "Device", endpoint.access_pe
                                    ).finish()
                            except AttributeError as e:
                                raise UserErrorException(
                                    self.log,
                                    StatusCodes.DYNAMIC_METHOD_NOT_FOUND,
                                    str(e),
                                ).set_context(
                                    "Dynamic Method Error",
                                    "check_if_interface_exists method is missing from "
                                    "multi-vendor python class",
                                ).add_state(
                                    "Endpoint", endpoint.access_pe
                                ).finish()

        except Exception as e:
            traceback.print_exc()
            opaque["VALIDATION_ERROR"] = str(e)

        return list(opaque.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.l3vpn_internal_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        pass

    @staticmethod
    def diff_iterate(root, service) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Device is modified if the following paths are modified
            # /cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}/endpoint
            # /cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}/custom-template

            # Ignore service assurance changes as it doesn't modify device
            # /cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}/service-assurance
            if len(keypath) < 3:
                return ncs.ITER_RECURSE

            kp_3 = str(keypath[-3])

            if kp_3 == 'custom-template':
                self.device_modified = True
            elif kp_3 == 'endpoint':
                self.device_modified = True

                # Catch route-distinguisher update
                # /cisco-flat-L3vpn-fp-internal:flat-L3vpn{L3 cli-0}
                # /endpoint{cli-0}/vrf/route-distinguisher
                if len(keypath) == 6:
                    if str(keypath[0]) == "route-distinguisher":
                        self.device_attr_updated.add(str(keypath[0]))
                else:
                    return ncs.ITER_RECURSE

            return ncs.ITER_CONTINUE

        diff_iter = DiffIterateWrapper(diter,
                                       device_attr_updated=set(),
                                       device_modified=False)
        t = maagic.get_trans(root)
        t.keypath_diff_iterate(diff_iter, 0, service._path)

        return diff_iter


class FlatL3vpnEndpointCallBack(ncs.application.NanoService):
    """
    NanoService callback handler for flat-L3vpn service node
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.l3vpn_internal_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None

            if state == "cisco-flat-L3vpn-fp-nano-services:config-apply":
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

        for endpoint in service.endpoint:
            try:
                # Will always be one node for inner service
                router = Utils.get_device_impl_default_class(
                    self, root, service, endpoint.access_pe,
                    root.cisco_flat_L3vpn_fp_internal__cfp_configurations.dynamic_device_mapping,
                )
            except UserErrorException:
                raise
            except Exception as e:
                raise ServiceException(
                    self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
                ).set_context(
                    "Dynamic Device Mapping", "Error retrieving Dynamic Device class"
                ).add_state(
                    "Device", endpoint.access_pe
                ).add_state(
                    "Service", service.name
                ).finish()

            try:
                try:
                    router.conf_l3vpn(endpoint)
                except AttributeError as e:
                    raise UserErrorException(
                        self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Method Error",
                        "conf_l3vpn method is missing from "
                        "multi-vendor python class",
                    ).add_state(
                        "Endpoint", endpoint.access_pe
                    ).finish()
                try:
                    template_name = router.get_interface_shutdown_template()
                    if template_name:
                        shutdown_service = root.\
                            core_fp_delete_tag_service__core_fp_delete_shutdown_service.\
                            create(endpoint.access_pe, endpoint.if_type, endpoint.if_id)
                        shutdown_service.template_name = template_name
                        for l2_att in endpoint.l2_attachment_circuit:
                            shutdown_service = root.\
                                core_fp_delete_tag_service__core_fp_delete_shutdown_service.\
                                create(endpoint.access_pe, l2_att.if_type, l2_att.if_id)
                            shutdown_service.template_name = template_name
                except AttributeError as e:
                    raise UserErrorException(
                        self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Method Error",
                        "get_interface_shutdown_template method is missing "
                        "from multi-vendor python class",
                    ).add_state(
                        "Endpoint", endpoint.access_pe
                    ).finish()

            except UserErrorException:
                raise
            except Exception as e:
                raise DeviceConfigException(
                    self.log, StatusCodes.CONFIG_FAILURE, str(e)
                ).set_context(
                    "Configuration Error", "Could not apply service config on endpoint"
                ).add_state(
                    "Endpoint", endpoint.access_pe
                ).add_state(
                    "Service", service.name
                ).finish()

            # Apply global level custom-template
            if service.custom_template:
                TsdnUtils.apply_custom_template(self, root, service, endpoint.access_pe,
                                                StatusCodes.CUSTOM_TEMPLATE_ERROR,
                                                CustomTemplateException)

            # Apply local level custom-template
            if endpoint.custom_template:
                TsdnUtils.apply_custom_template(self, root, endpoint, endpoint.access_pe,
                                                StatusCodes.CUSTOM_TEMPLATE_ERROR,
                                                CustomTemplateException)

        return opaque

    def _create_ready(self, _tctx, root, service, plan,
                      component, state, opaque, comp_vars):

        state_node = plan.component[component].state[state]
        opaque = dict(opaque)

        # First check if there is plan error in internal, set it
        if "true" == opaque.get("DEVICE_TRANS_INVALIDATED"):
            endpoint = service.endpoint[service.endpoint_name]
            self.log.info(f"Device Transaction for service {service.name} "
                          f"device {endpoint.access_pe} is invalidated, marking ready failed")
            state_node.status = "failed"
            return list(opaque.items())

        # Update timestamp for CQ service updates (with no device change) to re-deploy NB service to
        # mark plan reached
        TsdnUtils.update_state_when_timestamp(self, component, state_node, "from internal plan")

        return list(opaque.items())
