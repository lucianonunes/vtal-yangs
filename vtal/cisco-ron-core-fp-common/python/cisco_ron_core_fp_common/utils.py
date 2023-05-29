import importlib
import os
import ncs
import _ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from custom_template_utils import custom_template_utils as ctu
from lsa_utils_pkg.dmap.dm_utils import get_device_remote_nso


def is_ha_slave():
    with maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as trans:
        if trans.exists("/tfnm:ncs-state/tfnm:ha"):
            mode = str(maagic.get_node(trans, "/tfnm:ncs-state/tfnm:ha/tfnm:mode"))
            return mode != "master"
        return False


def device_sync(username, device_name, sync_direction):
    if sync_direction == "sync-to":
        return device_sync_to(username, device_name)
    else:
        return device_sync_from(username, device_name)


def device_sync_to(username, device_name):
    with maapi.single_read_trans(username, "system") as th:
        root = ncs.maagic.get_root(th)
        action_path = root.ncs__devices.device[device_name].sync_to
        return action_path()


def device_sync_from(username, device_name):
    with maapi.single_read_trans(username, "system") as th:
        root = ncs.maagic.get_root(th)
        action_path = root.ncs__devices.device[device_name].sync_from
        return action_path()


def get_device_ned_id(root, device):
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


def get_device_impl_class(
    root, service, device, device_ned_id, dynamic_map_list, error_code, my_error, log
):
    """Get runtime python class."""
    router = None
    my_class = None

    if service is None:
        service_name = ""
    else:
        service_name = service.name
    try:
        if device_ned_id in dynamic_map_list:
            my_class = dynamic_map_list[device_ned_id].python_impl_class_name
            # NED ID matching
            log.info("Dynamic Loading Dynamic Class {}".format(my_class))
            myclass = getattr(
                importlib.import_module(my_class), my_class.split(".")[-1]
            )
            router = myclass(log, root, service)
    except ImportError as e:
        raise my_error(log, error_code, str(e)).set_context(
            "Dynamic Device Class",
            "Dynamic class {} not loaded into NSO".format(my_class),
        ).add_state("Device", device).add_state("Service", service_name).add_state(
            "Device NED ID", device_ned_id
        ).finish()

    return router


def get_kp_service_id(kp):
    kpath = str(kp)
    service = kpath[kpath.find("{") + 1: len(kpath) - 1]
    return service


def get_service_operation(op):
    if op == _ncs.dp.NCS_SERVICE_CREATE:
        return "SERVICE_CREATE"
    elif op == _ncs.dp.NCS_SERVICE_UPDATE:
        return "SERVICE_UPDATE"
    elif op == _ncs.dp.NCS_SERVICE_DELETE:
        return "SERVICE_DELETE"
    else:
        return str(op)


def apply_custom_template(
    self, root, node, device_name, error_code, my_error, extra_vars=None
):
    # Check if all the params are passed as expected
    try:
        ct_list = node.custom_template
        if len(ct_list) > 0:
            apply = ctu.apply_custom_templates(
                self, root, node, device_name, ct_list, extra_vars
            )
            if apply:
                self.log.info("Successfully applied all custom-templates defined.")
            else:
                self.log.info(
                    'apply_custom_template is false, Custom Template will not be " \
                                                "applied on  device_name: {}'.format(
                        device_name
                    )
                )
    except Exception as e:
        raise my_error(self.log, error_code, str(e)).set_context(
            "Custom Template", "Failed to apply custom-template on device"
        ).add_state("Device", device_name).finish()


def update_plan_status_codes(self, root, plan, internal_plan_path, internal_service):
    try:
        # Check if internal plan exists
        if internal_service in internal_plan_path:
            # First clean up current status codes
            del plan.status_code_detail
            # Next copy over all internal plan status code details
            for status_code_detail in internal_plan_path[
                internal_service
            ].plan.status_code_detail:
                s_type, s_name = status_code_detail.type, status_code_detail.name
                nb_status_code_detail = plan.status_code_detail.create(s_type, s_name)
                nb_status_code_detail.code = status_code_detail.code
                nb_status_code_detail.severity = status_code_detail.severity
                nb_status_code_detail.recommended_action = (
                    status_code_detail.recommended_action
                )
                for context in status_code_detail.context:
                    nb_context = nb_status_code_detail.context.create(
                        context.context_name
                    )
                    nb_context.context_msg = context.context_msg
    except Exception as e:
        self.log.error("Update status code failed : {}".format(e))


def update_component_status_codes(
    self,
    root,
    internal_plan_path,
    internal_service,
    external_component,
    internal_component_key,
):
    try:
        # Update component status code
        # Check if internal plan exists
        if internal_service in internal_plan_path:
            # Get internal components
            internal_components = internal_plan_path[internal_service].plan.component
            if internal_component_key in internal_components:
                # Get internal component
                internal_component = internal_components[internal_component_key]
                # Copy component status code
                external_component.status_code = internal_component.status_code
    except Exception as e:
        self.log.error("Update status code failed : {}".format(e))


def is_cq_enabled(self, root, opaque, devices, th, is_lsa):
    device_cq_details = []
    ## Commit flag has CQ
    cp = th.get_params()
    if ("commit-queue" in str(cp)) and ("bypass" not in str(cp)):
        return [(device, True) for device in devices]

    ## Committed without CQ flags, check global & device level settings
    if is_lsa:
        ## Get RFS Node to device mapping
        device_rfs_map = get_device_remote_nso(root, devices)
        for rfs_node, rfs_node_devices in device_rfs_map.items():
            rfs_node_ls = root.ncs__devices.device[rfs_node].live_status
            get_device_cq_details(rfs_node_ls, device_cq_details, rfs_node_devices)
    else:
        get_device_cq_details(root, device_cq_details, devices)

    return device_cq_details


def get_device_cq_details(root, device_cq_details, devices):
    ## Global CQ
    global_cq_enabled = False
    if root.ncs__devices.global_settings.commit_queue.enabled_by_default:
        global_cq_enabled = True
    ## Per device CQ
    for device in devices:
        per_device_cq = root.ncs__devices.device[device].commit_queue.enabled_by_default
        if per_device_cq is None:
            device_cq_details.append((device, global_cq_enabled))
        elif per_device_cq is False:
            device_cq_details.append((device, False))
        else:
            device_cq_details.append((device, True))


def get_opaque_updated_devices(opaque):
    if opaque.get("DEVICE_UPDATED", "") != "":
        ## Opaque data is string
        ## {'DEVICE_UPDATED': "['PIOSXR-1', 'PIOSXR-0']"}
        updated_devices = opaque.get("DEVICE_UPDATED")
        ## string to list of devices
        return updated_devices.strip("][").replace("'", "").split(", ")
    else:
        return []


def set_service_cleanup_flag(self, service_path, username):
    # Adds service path to cisco-ron-core-fp-common service_cleanup_progress list
    self.log.debug(
        "Adding {} to cisco-ron-core-fp-common service-cleanup-progress list".format(
            service_path
        )
    )
    with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        cleanup_in_progress_list = (
            root.cisco_ron_core_fp_common__service_cleanup_progress
        )
        cleanup_in_progress_list.create(service_path)
        th.apply()
    self.log.debug(
        "Added {} to cisco-ron-core-fp-common service-cleanup-progress list".format(
            service_path
        )
    )


def delete_service_cleanup_flag(self, service_path, username):
    # Deletes service path from cisco-ron-core-fp-common service-cleanup-progress list
    self.log.debug(
        "Deleting {} from cisco-ron-core-fp-common service-cleanup-progress list".format(
            service_path
        )
    )
    with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        cleanup_in_progress_list = (
            root.cisco_ron_core_fp_common__service_cleanup_progress
        )
        if service_path in cleanup_in_progress_list:
            del cleanup_in_progress_list[service_path]
            th.apply()
            self.log.debug(
                "Deleted {} from cisco-ron-core-fp-common service-cleanup-progress list".format(
                    service_path
                )
            )
        else:
            self.log.debug(
                "{} is not defined in service-cleanup-progress list".format(
                    service_path
                )
            )


def check_service_cleanup_flag(self, service_path, username):
    # Checks if service path is currently in cleanup
    with maapi.single_read_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        cleanup_in_progress_list = (
            root.cisco_ron_core_fp_common__service_cleanup_progress
        )
        if service_path in cleanup_in_progress_list:
            self.log.debug(
                "{} found in cisco-ron-core-fp-common service-cleanup-progress list".format(
                    service_path
                )
            )
            return True
        self.log.debug(
            "{} not found in cisco-ron-core-fp-common service-cleanup-progress list".format(
                service_path
            )
        )
        return False


def is_netsim_device(root, device):
    localhost = "127.0.0.1"  # NOSONAR
    if (os.getenv("IS_NETSIM", "false") == "true") or (
        root.devices.device[device].address == localhost
    ):
        return True
    else:
        return False
