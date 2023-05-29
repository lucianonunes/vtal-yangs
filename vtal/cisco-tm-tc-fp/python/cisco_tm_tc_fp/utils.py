import importlib
import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from .NativeXR import NativeXR
from .NativeUMXR import NativeUMXR

supported_ned_ids = ["cisco-iosxr-nc-6.6:cisco-iosxr-nc-6.6","cisco-iosxr-nc-6.5:cisco-iosxr-nc-6.5"]
supported_um_ned_ids = ["cisco-iosxr-nc-7.0:cisco-iosxr-nc-7.0","cisco-iosxr-nc-7.3:cisco-iosxr-nc-7.3","cisco-iosxr-nc-7.315:cisco-iosxr-nc-7.315","cisco-iosxr-nc-7.32:cisco-iosxr-nc-7.32","cisco-iosxr-nc-7.4:cisco-iosxr-nc-7.4", "cisco-iosxr-nc-7.5:cisco-iosxr-nc-7.5"]

def get_device_ned_id(self, root, device):
    device_type_node = root.devices.device[device].device_type

    if device_type_node.ne_type == "cli":
        return device_type_node.cli.ned_id

    elif device_type_node.ne_type == "generic":
        return device_type_node.generic.ned_id

    # Default
    elif device_type_node.ne_type == "netconf":
        return device_type_node.netconf.ned_id

    elif device_type_node.ne_type == "snmp":
        return device_type_node.snmp.ned_id

    else:
        return None


def get_device_impl_class(self, root, service, device, device_ned_id, dynamic_map_list):
    """Get runtime python class."""
    router = None
    my_class = None

    try:
        for dynamic_map in dynamic_map_list:
            # NED ID matching
            if dynamic_map.ned_id == device_ned_id:
                my_class = dynamic_map.python_impl_class_name
                self.log.info('Dynamic Loading Dynamic Class {}'.format(my_class))
                myclass = getattr(importlib.import_module(my_class), my_class.split('.')[-1])
                router = myclass(self.log, root, service)
                break
    except ImportError:
        raise Exception("Dynamic class '{}' not loaded in NSO.".format(my_class))

    return router


def get_device_impl_default_class(self, root, service, device, dynamic_map_list):
    device_ned_id = get_device_ned_id(self, root, device)

    self.log.debug("Device ned is {}".format(device_ned_id))
    iosxr_nc_ned_id = root.cisco_tm_tc_fp__cfp_configurations.iosxr_nc_ned_id
    if (iosxr_nc_ned_id == device_ned_id) or (device_ned_id in supported_ned_ids):
        router = NativeXR(self.log, root, service)
    elif (device_ned_id in supported_um_ned_ids):
        router = NativeUMXR(self.log, root, service)
    elif device_ned_id is not None:
        # Dynamic Loading
        router = get_device_impl_class(self, root, service, device, device_ned_id, dynamic_map_list)
        if router is None:
            raise Exception("Router NED not supported : {}".format(device_ned_id))
    else:
        raise Exception("Router NED not supported : {}".format(device_ned_id))

    return router


def apply_custom_template(self, root, node, device_name):
    if root.apply_custom_template is True:
        self.log.info('Apply Custom Template on device_name: {}'.format(device_name))
        tm_tc_vars = ncs.template.Variables()
        tm_tc_template = ncs.template.Template(node)
        tm_tc_vars.add('DEVICE', device_name)
        tm_tc_template.apply('custom-template-internal-advance', tm_tc_vars)
    else:
        self.log.info('apply_custom_template is false, Custom Template will not be " \
            "applied on  device_name: {}'.format(device_name))


def is_ha_slave():
    with maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as th:
        if th.exists("/tfnm:ncs-state/tfnm:ha"):
            mode = str(maagic.get_node(th,
                                       '/tfnm:ncs-state/tfnm:ha/tfnm:mode'))
            return mode != 'master'
        return False


def is_stacked_service():
    with maapi.single_read_trans("", "system") as th:
        root = maagic.get_root(th)
        if root.cisco_tm_tc_fp__cfp_configurations.stacked_service_enabled.exists():
            return True
        return False

