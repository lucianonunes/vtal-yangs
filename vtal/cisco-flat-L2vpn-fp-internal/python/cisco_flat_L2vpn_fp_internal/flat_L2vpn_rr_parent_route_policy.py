from ncs.application import Service
from .utils import get_device_impl_default_class
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import (
    DeviceConfigException,
    ServiceException
)
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from . import utils as Utils
from core_fp_common import instrumentation
import logging


class RRParentRoutePolicyCallback(Service):
    @Service.create
    @instrumentation.instrument_service(logging.INFO,
                                        Utils.l2vpn_rr_parent_route_policy_servicepoint)
    def cb_create(self, tctx, root, service, proplist):

        # Get device impl
        try:
            router = get_device_impl_default_class(
                self, root, service, service.device,
                root.cisco_flat_L2vpn_fp_internal_common__cfp_configurations.dynamic_device_mapping
            )
        except Exception as e:
            raise ServiceException(
                self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)
            ).set_context(
                "Dynamic Device Mapping", "Error retrieving Dynamic Device class"
            ).add_state(
                "Device", service.device
            ).add_state(
                "Service", service.name
            ).finish()

        # Apply route policies
        try:
            router.conf_l2vpn_rp(service)
        except Exception as e:
            raise DeviceConfigException(
                self.log, StatusCodes.CONFIG_FAILURE, str(e)
            ).set_context(
                "Configuration Error",
                "Could not apply L2 RR Parent Route Policy " "service config",
            ).add_state(
                "Parent RP Service", service.name
            ).add_state(
                "Device", service.device
            ).finish()
