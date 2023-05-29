# -*- mode: python; python-indent: 4 -*-
import importlib
from .onc import onc
from .optical_errors import UserError

from cisco_ron_core_fp_common.status_codes import StatusCodes


def get_device_ned_id(root, device):
    device_type_node = root.devices.device[device].device_type
    device_context = root.devices.device[device].source.context

    if device_type_node.ne_type == "cli":
        device_ned_id = device_type_node.cli.ned_id

    elif device_type_node.ne_type == "generic":
        device_ned_id = device_type_node.generic.ned_id

    # Default
    elif device_type_node.ne_type == "netconf":
        device_ned_id = device_type_node.netconf.ned_id

    elif device_type_node.ne_type == "snmp":
        device_ned_id = device_type_node.snmp.ned_id

    else:
        device_ned_id = None

    return (device_ned_id, device_context)


def get_device_impl_class(root, service, device, device_ned_id, device_context, log):
    """Get runtime python class."""
    router = None
    my_class = None
    if service is None:
        service_name = ""
    else:
        service_name = service.name
    try:
        for (
            dynamic_map
        ) in root.tapi_connectivity__cfp_configurations.dynamic_device_mapping:
            # NED ID matching
            if (dynamic_map.ned_id == device_ned_id) and (
                dynamic_map.vendor == device_context
            ):
                my_class = dynamic_map.python_impl_class_name
                log.info("Dynamic Loading Dynamic Class {}".format(my_class))
                myclass = getattr(
                    importlib.import_module(my_class), my_class.split(".")[-1]
                )
                router = myclass(log, root, service)
                break
    except ImportError as e:
        raise UserError(log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND, str(e)).set_context(
            "Dynamic Device Class",
            "Dynamic class {} not loaded into NSO".format(my_class),
        ).add_state("Device", device).add_state("Service", service_name).add_state(
            "Device NED ID", device_ned_id
        ).finish()

    return router


def get_device_impl_default_class(root, service, device, log):
    (device_ned_id, device_context) = get_device_ned_id(root, device)

    log.info(device_ned_id)
    controller_default_ned_id = (
        root.tapi_connectivity__cfp_configurations.controller_default_ned_id
    )

    if not device_context:
        device_context = (
            root.tapi_connectivity__cfp_configurations.controller_default_vendor
        )

    if service is None:
        service_name = ""
    else:
        service_name = service.name

    if controller_default_ned_id == device_ned_id:
        router = onc(log, root, service)
    elif device_ned_id is not None:
        # Dynamic Loading
        router = get_device_impl_class(
            root, service, device, device_ned_id, device_context, log
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
