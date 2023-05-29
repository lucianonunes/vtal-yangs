# -*- mode: python; python-indent: 4 -*-
from .IosXR import IosXR
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common import utils as TsdnUtils

odn_internal_servicepoint = "sr-odn-servicepoint"
policy_internal_servicepoint = "sr-policies-servicepoint"
odn_internal_validation_callpoint = "sr-odn-internal-validation"
policy_internal_validation_callpoint = "sr-policies-internal-validation"


def get_device_impl_default_class(self, root, service, device, dynamic_map_list):
    device_ned_id = TsdnUtils.get_device_ned_id(self, root, device)

    self.log.info(device_ned_id)
    iosxr_default_ned_id = root.cisco_sr_te_cfp_internal__cfp_configurations.iosxr_default_ned_id
    if iosxr_default_ned_id == device_ned_id:
        router = IosXR(self.log, root, service)
    elif device_ned_id is not None:
        # Dynamic Loading
        router = TsdnUtils.get_device_impl_class(self, root, service, device, device_ned_id,
                                                 dynamic_map_list,
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


# This function is to check if the external service package co-exists. If it
# does, it means zombie would look something like: /cisco-sr-te-cfp-internal:sr-te/
# Otherwise it would look something like: /sr-te/...
# This is needed only for SR-TE-CFP package because of the way service is
# modelled.
def external_service_model_exists(root):
    try:
        # If this path exists, it is not LSA
        if root.cisco_sr_te_cfp__sr_te:
            return True
        return False
    except Exception:
        return False
