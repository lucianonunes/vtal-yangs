import importlib
import ncs
import _ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from custom_template_utils import custom_template_utils as ctu
from core_fp_common.common_utils import get_local_user
from datetime import datetime, timezone
import traceback
import json
from core_fp_common import instrumentation
import logging
import os
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from cisco_tsdn_core_fp_common import constants as const


def copy_rfs_plan_to_local(log, local_rfs_plan, rfs_plan, internal_zombie_service,
                           rfs_live_status):
    log.info(f"Copying RFS plan to local plan for rfs-plan={rfs_plan._path} "
             f"local-plan={local_rfs_plan._path}")
    # Copy plan component
    for comp in rfs_plan.component:
        local_comp = local_rfs_plan.component.create(comp.type, comp.name)
        local_comp.back_track = comp.back_track
        for state in comp.state:
            local_state = local_comp.state.create(state.name)
            local_state.status = state.status
            local_state.when = state.when
            local_state.service_reference = state.service_reference
            if state.post_action_status:
                local_state.post_action_status = state.post_action_status

    # Copy plan commit-queue
    if rfs_plan.commit_queue:
        local_plan_cq = local_rfs_plan.commit_queue.create()
        for cq_item in rfs_plan.commit_queue.queue_item:
            local_plan_cq.queue_item.create(cq_item.id)

    # Copy plan failed
    if rfs_plan.failed:
        local_rfs_plan.failed.create()
    if rfs_plan.error_info:
        local_error_info = local_rfs_plan.error_info.create()
        local_error_info.message = rfs_plan.error_info.message
        local_error_info.log_entry = str(rfs_plan.error_info.log_entry)

    # If zombie exists, mark zombie-exists flag
    for zombie_service in internal_zombie_service:
        if rfs_live_status.ncs__zombies.service.exists(zombie_service):
            local_rfs_plan.zombie_exists = True
            break


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


def get_device_ned_id_from_dispatch_map(self, root, device):
    return root.devices.lsa.dispatch_map.device[device].ned_id


def get_device_platform_version(self, root, device):
    return root.ncs__devices.device[device].platform.version or ""


def is_netsim_device(self, root, device):
    return ((os.getenv("IS_NETSIM", "false") == "true")
            or (root.devices.device[device].address == "127.0.0.1"))


def get_xe_interface_mapping(service_interface_name):
    if service_interface_name == const.TEN_GIG_E:
        return const.TEN_GIGABIT_ETHERNET
    elif service_interface_name == const.FORTY_GIG_E:
        return const.FORTY_GIGABIT_ETHERNET
    else:
        return service_interface_name


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


def get_device_impl_class(self, root, service, device, device_ned_id,
                          dynamic_map_list, error_code, UserErrorException):
    """Get runtime python class."""
    router = None
    my_class = None
    try:
        for dynamic_map in dynamic_map_list:
            # NED ID matching
            if dynamic_map.ned_id == device_ned_id:
                my_class = dynamic_map.python_impl_class_name
                self.log.info(f"Dynamic Loading Dynamic Class {my_class}")
                myclass = getattr(importlib.import_module(my_class), my_class.split(".")[-1])
                router = myclass(self.log, root, service)
                break
    except ImportError as e:
        raise UserErrorException(self.log, error_code, str(e)).set_context(
            "Dynamic Device Class", f"Dynamic class {my_class} not loaded into NSO",
        ).add_state("Device", device).add_state("Service", service.name).add_state(
            "Device NED ID", device_ned_id
        ).finish()

    return router


def apply_custom_template(self, root, node, device_name,
                          error_code, CustomTemplateException):
    # Check if all the params are passed as expected
    try:
        ct_list = node.custom_template
        if len(ct_list) > 0:
            extra_vars = {}
            apply = ctu.apply_custom_templates(self, root, node, device_name, ct_list, extra_vars)
            if apply:
                self.log.info("Successfully applied all custom-templates defined.")
            else:
                self.log.info("apply_custom_template is false, Custom Template "
                              f"will not be applied on  device_name: {device_name}")
    except Exception as e:
        raise CustomTemplateException(self.log, error_code, str(e)).set_context(
            "Custom Template", "Failed to apply custom-template on device"
        ).add_state("Device", device_name).finish()


def update_plan_status_codes(self, root, plan, internal_plan_path, internal_service):
    try:
        # Check if internal plan exists
        if internal_service in internal_plan_path:
            # First clean up current status codes
            del plan.status_code_detail
            # Next copy over all internal plan status code details
            for status_code_detail in internal_plan_path[internal_service].plan.status_code_detail:
                s_type, s_name = status_code_detail.type, status_code_detail.name
                nb_status_code_detail = plan.status_code_detail.create(s_type, s_name)
                nb_status_code_detail.code = status_code_detail.code
                nb_status_code_detail.severity = status_code_detail.severity
                nb_status_code_detail.recommended_action = (status_code_detail.recommended_action)
                nb_status_code_detail.impacted_device = (status_code_detail.impacted_device)
                for context in status_code_detail.context:
                    nb_context = nb_status_code_detail.context.create(context.context_name)
                    nb_context.context_msg = context.context_msg
    except Exception as e:
        self.log.error(f"Update status code failed : {e}")


def update_component_status_codes(self, root, internal_plan_path, internal_service,
                                  external_component, internal_component_key):
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
        self.log.error(f"Update status code failed : {e}")


def is_cq_enabled(self, root, devices, th, is_lsa):
    device_cq_details = []
    # Commit flag has CQ
    cp = th.get_params()
    if ("commit-queue" in str(cp)) and ("bypass" not in str(cp)):
        return [(device, True) for device in devices]
    elif ("commit-queue" in str(cp)) and ("bypass" in str(cp)):
        return [(device, False) for device in devices]

    # Committed without CQ flags, check global & device level settings
    if is_lsa:
        # Get RFS Node to device mapping
        device_rfs_map = LsaUtils.get_device_remote_nso(devices)
        for rfs_node, rfs_node_devices in device_rfs_map.items():
            rfs_node_ls = root.ncs__devices.device[rfs_node].live_status
            get_device_cq_details(rfs_node_ls, device_cq_details, rfs_node_devices)
    else:
        get_device_cq_details(root, device_cq_details, devices)

    return device_cq_details


def get_device_cq_details(root, device_cq_details, devices):
    # Global CQ
    global_cq_enabled = False
    if root.ncs__devices.global_settings.commit_queue.enabled_by_default:
        global_cq_enabled = True
    # Per device CQ
    for device in devices:
        per_device_cq = root.ncs__devices.device[device].commit_queue.enabled_by_default
        if per_device_cq is None:
            device_cq_details.append((device, global_cq_enabled))
        elif per_device_cq is False:
            device_cq_details.append((device, False))
        else:
            device_cq_details.append((device, True))


def is_cq_enabled_generic(self, root, th):
    # Commit flag has CQ
    cp = th.get_params()
    if "commit-queue" in str(cp):
        return ("bypass" not in str(cp))
    # Global commit Queue
    else:
        return root.ncs__devices.global_settings.commit_queue.enabled_by_default


def get_opaque_value_as_list(opaque, key):
    # Return list of devices with commit queue disabled
    # Make sure opaque is dict type
    if not type(opaque) == type(dict):
        opaque = dict(opaque)
    # Grab NO_CQ_DEVICE_UPDATED
    no_cq_devices = opaque.get(key, False)
    if not no_cq_devices:
        return []
    # Convert to json encoded string
    no_cq_devices = no_cq_devices.replace("'", '"')
    # Return decoded python list
    try:
        return json.loads(no_cq_devices)
    except ValueError:
        return []


@instrumentation.instrument_validate(logging.INFO)
def validate_service(self, validation_callpoint, tctx, kp):
    # First get current transaction
    t = maapi.Maapi().attach(tctx)
    # Next get root node
    root = maagic.get_root(t)
    # Get service
    service = maagic.get_node(maagic.get_trans(root), kp)

    # Check if opaque has 'VALIDATION_ERROR' property set
    prop_list = service.private.property_list.property
    if "VALIDATION_ERROR" in prop_list and prop_list["VALIDATION_ERROR"].value != "":
        raise Exception(prop_list["VALIDATION_ERROR"].value)

    return _ncs.OK


def set_service_cleanup_flag(self, service_path, username):
    # Adds service path to cisco-tsdn-core-fp-common cleanup-in-progress-for list
    self.log.debug(f"Adding {service_path} to cisco-tsdn-core-fp-common "
                   "cleanup-in-progress-for list")
    with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        cleanup_in_progress_list = (root.cisco_tsdn_core_fp_common__cleanup_in_progress_for)
        cleanup_in_progress_list.create(service_path)
        th.apply()
    self.log.debug(f"Added {service_path} "
                   "to cisco-tsdn-core-fp-common cleanup-in-progress-for list")


def delete_service_cleanup_flag(self, service_path, username):
    # Deletes service path from cisco-tsdn-core-fp-common cleanup-in-progress-for list
    self.log.debug(f"Deleting {service_path} from cisco-tsdn-core-fp-common "
                   "cleanup-in-progress-for list")
    with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        cleanup_in_progress_list = root.cisco_tsdn_core_fp_common__cleanup_in_progress_for
        if service_path in cleanup_in_progress_list:
            del cleanup_in_progress_list[service_path]
            th.apply()
            self.log.debug(f"Deleted {service_path} from cisco-tsdn-core-fp-common "
                           "cleanup-in-progress-for list")
        else:
            self.log.debug(f"{service_path} is not defined in cleanup-in-progress-for list")


def check_service_cleanup_flag(log, service_path, username):
    # Checks if service path is currently in cleanup
    with maapi.single_read_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        cleanup_in_progress_list = (root.cisco_tsdn_core_fp_common__cleanup_in_progress_for)
        if service_path in cleanup_in_progress_list:
            log.debug(f"{service_path} found in cisco-tsdn-core-fp-common "
                      "cleanup-in-progress-for list")
            return True
        log.debug(f"{service_path} not found in cisco-tsdn-core-fp-common "
                  "cleanup-in-progress-for list")
        return False


def get_action_timeout(self, username):
    # Gets the timeout for actions from the yang
    with maapi.single_read_trans(username, "system", db=ncs.RUNNING) as th:
        root = maagic.get_root(th)
        try:
            return root.cisco_tsdn_core_fp_common__action_application_timeout
        except Exception:
            return 7200


def remove_status_code_detail(plan, component):
    # Deletes status code from plan component
    del plan.status_code_detail[(plan.component[component].type,
                                 plan.component[component].name)]


def update_state_when_timestamp(self, component_name, state_node, log_entry_postfix):
    # Update the state node's when timestamp
    try:
        if state_node.status != "not-reached":
            state_node.when = datetime.now(timezone.utc).isoformat()
            self.log.info("Updated plan component {0} state {1} with local timezone, {2}".
                          format(component_name, state_node.name, log_entry_postfix))
    except Exception as e:
        traceback.print_exc()
        self.log.warning(f"Could not update plan component ({component_name}) "
                         f"state ({state_node.name}) timestamp: {e}")


def redeploy_service(service_kp, plan_kp, username, log):
    """Redeploys a service

    Args:
        service_kp (str): The keypath to the service
        plan_kp (str): The kaypath to the service plan
        username (str): The username to use for write transactions
        log (Object): Object used for logging
    """
    log.info(f"Attempting to re-deploy service: {service_kp}")
    with ncs.maapi.single_read_trans(username, "system", db=ncs.RUNNING) as th:
        if th.exists(plan_kp):
            service_plan = maagic.get_node(th, plan_kp)
            if len(service_plan.plan.component) > 0 and th.exists(service_kp):
                service = maagic.get_node(th, service_kp)
                service.reactive_re_deploy()
                log.info(f"Service re-deploy done for: {service_kp}")


def redeploy_zombie_service(service_kp, plan_kp, service_xpath, username, log):
    """Redeploys a zombie or service if the zombie doesn't exist

    Args:
        service_kp (str): The keypath to the service
        plan_kp (str): The kaypath to the service plan
        service_xpath (str): The xpath to the service
        username (str): The username to use for write transactions
        log (Object): Object used for logging
    """
    # Strip quotes for policy with comma in it
    service_path = service_xpath.replace('"', '')
    log.info(f"Attempting to re-deploy zombie: {service_path}")
    should_redeploy_service = False
    with ncs.maapi.single_read_trans(username, "system", db=ncs.OPERATIONAL) as th:
        zombie_kp = f"/ncs:zombies/ncs:service{{{service_path}}}"
        if th.exists(zombie_kp):
            zombie = maagic.get_node(th, zombie_kp)
            zombie.reactive_re_deploy()
            log.info(f"Zombie Service re-deploy done for: {service_path}")
        else:
            should_redeploy_service = True

    if should_redeploy_service:
        # Redeploy the service outside of the above transaction
        redeploy_service(service_kp, plan_kp, username, log)


def check_if_cq_id_exists_on_plan(plan_path, service_key):
    # Check if internal plan has CQ ID, avoid redeploys as eventually after CQ ID
    # removal there will be another redeploy. This will handle delete redeploys also
    # when RT-42738 is fixed & CQ is updated on plan for deletes as well.
    with ncs.maapi.single_read_trans("", "system") as th:
        service_plan = maagic.cd(maagic.get_root(th), plan_path)

        if service_key in service_plan:
            try:
                internal_plan = service_plan[service_key].plan
                if (internal_plan.commit_queue
                        and len(internal_plan.commit_queue.queue_item) > 0):
                    return True
            except Exception:
                return False
        return False


def get_plan_status(plan_path, service_key):
    with ncs.maapi.single_read_trans("", "system") as th:
        plan = maagic.cd(maagic.get_root(th), plan_path)
        if service_key in plan:
            try:
                # First check if plan failed
                if plan[service_key].plan.failed:
                    return "failed"
                # Otherwise, return self ready status
                return (plan[service_key].plan.component[("ncs:self", "self")]
                        .state["ncs:ready"].status)
            except Exception:
                return False
    return False


def is_failed_plan_converged(plan_path, service_key, log):
    with ncs.maapi.single_read_trans("", "system") as th:
        try:
            plan = maagic.get_node(th, plan_path)
            if service_key in plan:
                # First check if plan failed
                if plan[service_key].plan.failed:
                    # If plan failed but self ready is not-reached, plan not converged
                    if plan[service_key].plan.component[("ncs:self", "self")].state["ncs:ready"].\
                            status != "failed":
                        log.info(f"Failed plan not converged for {service_key}, allowing redeploy")
                        return False
        except Exception as e:
            log.debug(str(e))
            return True
    return True


def get_plan_err_msg(plan_path, service_key):
    with ncs.maapi.single_read_trans("", "system") as th:
        plan = maagic.get_node(th, plan_path)
        if service_key in plan and plan[service_key].plan.error_info.exists():
            return plan[service_key].plan.error_info.message
    return None


def match_plan_err_msg(plan_path, service_name, internal_plan_path, internal_service_name):
    nb_plan_err_msg = get_plan_err_msg(plan_path, service_name)
    if nb_plan_err_msg is not None:
        internal_plan_err_msg = get_plan_err_msg(internal_plan_path,
                                                 internal_service_name)
        if (internal_plan_err_msg is not None
                and internal_plan_err_msg in nb_plan_err_msg):
            return True
    return False


def check_if_aa_is_enabled(log):
    """
    Checks if 'cisco-aa-service-assurance' package is installed and sets
    'enable-service-assurance = True' which unhides service assurance
    related configs
    """
    local_user = get_local_user()

    try:
        with ncs.maapi.single_write_trans(local_user, "system", db=ncs.RUNNING) as wth:
            try:
                root = ncs.maagic.get_root(wth)
                # check if service assurance package exists
                root.cisco_aa_service_assurance__service_assurance
                root.cisco_tsdn_core_fp_common__enable_service_assurance = True
            except AttributeError:
                root.cisco_tsdn_core_fp_common__enable_service_assurance = False
            wth.apply()
    except _ncs.error.Error as e:
        # HA FAILOVER CHECK : operation in wrong state (17): node is in read-only mode
        # Workaround pending RT on HA ready-only-mode not being updated during failover
        if e.confd_errno == 17:
            log.warning("utils.check_if_aa_is_enabled : Node is in read-only mode, skipping AA "
                        "package existence check and enable-service-assurance flag update")
        else:
            traceback.print_exc()
            raise e


def redeploy_if_needed(self):
    cq_exists = check_if_cq_id_exists_on_plan(self.internal_plan_path, self.service_key)
    nb_plan_status = get_plan_status(self.plan_path, self.service_name)
    internal_plan_status = get_plan_status(self.internal_plan_path, self.service_key)

    # Service will only be re-deployed if one of the following is true
    #   1. NB plan is reached/failed, but there are pending commit-queue items,
    #      so we re-deploy NB service to back-track plan to not-reached
    #   2. Re-deploy if internal plan is failed (errors are not matching)
    #   3. There are no pending commit-queue items, but some change happened in
    #      the internal service, so re-deploy NB service and move to reached
    should_re_deploy = ((cq_exists and (nb_plan_status == "reached"
                                        or
                                        nb_plan_status == "failed"))
                        or internal_plan_status == "failed"
                        or not cq_exists)
    if should_re_deploy:
        redeploy_zombie_service(self.service_kp, self.plan_kp,
                                self.service_xpath, self.username,
                                self.log)


def get_all_rfs_nodes(self):
    """
    Temporary utility method for querying rfs nodes on CFS
    until LSA Utils fixes ENG-27940
    :return: reachable rfs_nodes on CFS
    """
    rfs_nodes = []
    with ncs.maapi.single_read_trans(get_local_user(), "system", db=ncs.RUNNING) as trans:
        qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                    "/ncs:devices/ncs:device",
                                    '/', 0, 1,
                                    _ncs.QUERY_STRING, ["name", "device-type/netconf/ned-id"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)

        device = maagic.get_root(trans).devices.device
        for r in res:
            if r[1] == "cisco-nso-nc-5.7:cisco-nso-nc-5.7":
                if device[r[0]].connect().result:
                    rfs_nodes.append(r[0])
                else:
                    self.log.info(f"get_all_rfs_nodes failed to connect to rfs node {r[0]}")

        _ncs.maapi.query_stop(trans.maapi.msock, qh)
    return set(rfs_nodes)


def validate_assurance_data(service_assurance, service_path):
    """
    Utility method for validating assurance data.
    if service assurance present and changes preservation , then check if
    monitoring state was already 'disable'
    :return: flag
    """
    with ncs.maapi.single_read_trans(get_local_user(), "system", db=ncs.RUNNING) as th:
        if th.exists(service_path + "/service-assurance"):
            mstate_path = f"{service_path}/service-assurance/monitoring-state"
            preservation_path = f"{service_path}/service-assurance/preservation"
            running_monitoring_state = str(maagic.get_node(th, mstate_path))
            running_preservation = str(maagic.get_node(th, preservation_path))
            if (running_preservation != service_assurance.preservation and
                    running_monitoring_state == 'disable' and
                    service_assurance.monitoring_state == 'disable'):
                return False
    return True
