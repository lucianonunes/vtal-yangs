# -*- mode: python; python-indent: 4 -*-
from .IosXRCli import IosXRCli
from .zr_errors import UserError
from cisco_ron_core_fp_common import utils as RonUtils
from cisco_ron_core_fp_common.status_codes import StatusCodes


def get_device_impl_default_class(root, service, device, log):
    device_ned_id = RonUtils.get_device_ned_id(root, device)

    log.info(device_ned_id)
    if service is None:
        service_name = ""
    else:
        service_name = service.name
    iosxr_default_ned_id = root.cisco_zr_cfp__cfp_configurations.iosxr_default_ned_id
    if iosxr_default_ned_id == device_ned_id:
        router = IosXRCli(log, root, service)
    elif device_ned_id is not None:
        # Dynamic Loading
        router = RonUtils.get_device_impl_class(
            root,
            service,
            device,
            device_ned_id,
            root.cisco_zr_cfp__cfp_configurations.dynamic_device_mapping,
            StatusCodes.DYNAMIC_CLASS_NOT_FOUND,
            UserError,
            log,
        )
        if router is None:
            raise UserError(log, StatusCodes.NED_NOT_SUPPORTED).set_context(
                "Router NED not supported: {}".format(device_ned_id),
                "Missing dynamic device mapping",
            ).add_state("Device", device).add_state("Service", service_name).add_state(
                "Device NED ID", device_ned_id
            ).finish()
    else:
        raise UserError(log, StatusCodes.NED_NOT_SUPPORTED).set_context(
            "Router NED not supported: {}".format(device_ned_id),
            "Missing dynamic device mapping",
        ).add_state("Device", device).add_state("Service", service_name).add_state(
            "Device NED ID", device_ned_id
        ).finish()

    return router
