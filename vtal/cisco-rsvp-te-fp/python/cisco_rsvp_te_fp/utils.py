# -*- mode: python; python-indent: 4 -*-

from .IosXR import IosXR
from cisco_tsdn_core_fp_common.status_codes.rsvp_te_fp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.status_codes.rsvp_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common import utils as TsdnUtils

rsvp_te_servicepoint = "rsvp-te-servicepoint"
rsvp_te_validation_callpoint = "rsvp-te-validation"


def get_device_impl_default_class(self, root, service, device, dynamic_map_list):
    device_ned_id = TsdnUtils.get_device_ned_id(self, root, device)

    self.log.info(device_ned_id)
    iosxr_default_ned_id = root.cisco_rsvp_te_fp__cfp_configurations.iosxr_default_ned_id
    if iosxr_default_ned_id == device_ned_id:
        router = IosXR(self.log, root, service)
    elif device_ned_id is not None:
        # Dynamic Loading
        router = TsdnUtils.get_device_impl_class(self, root, service, device,
                                                 device_ned_id, dynamic_map_list,
                                                 StatusCodes.DYNAMIC_CLASS_NOT_FOUND,
                                                 UserErrorException)
        if router is None:
            raise UserErrorException(
                self.log, StatusCodes.NED_NOT_SUPPORTED
            ).set_context(
                f"Router NED not supported: {device_ned_id}",
                "Missing dynamic device mapping",
            ).add_state(
                "Device", device
            ).add_state(
                "Service", service.name
            ).add_state(
                "Device NED ID", device_ned_id
            ).finish()
    else:
        raise UserErrorException(self.log, StatusCodes.NED_NOT_SUPPORTED).set_context(
            f"Router NED not supported: {device_ned_id}",
            "Missing dynamic device mapping",
        ).add_state("Device", device).add_state("Service", service.name).add_state(
            "Device NED ID", device_ned_id
        ).finish()

    return router
