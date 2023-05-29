import ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
import _ncs

from cisco_tsdn_core_fp_common.sr_te_odn import SrTeOdn
from cisco_tsdn_core_fp_common.sr_te_policy import SrTePolicy
from . import utils as Utils
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from core_fp_common import cleanup_utils as CleanupUtils
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import CustomActionException
from core_fp_common.common_utils import get_local_user
from cisco_tsdn_core_fp_common.utils import (
    set_service_cleanup_flag, delete_service_cleanup_flag,
    get_action_timeout, get_all_rfs_nodes
)


class UpdateInternalCfpConfigurations(ncs.dp.Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        username = get_local_user()
        with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            template = ncs.template.Template(root.cisco_sr_te_cfp__cfp_configurations)
            if Utils.is_lsa:
                # Get all RFS nodes
                create_vars = ncs.template.Variables()
                for rfs_node in get_all_rfs_nodes(self):
                    # TODO: How to avoid applying this on RFS node not belonging to TSDN?
                    create_vars.add("RFS_NODE", rfs_node)
                    template.apply("cisco-sr-te-cfp-copy-cfp-configurations", create_vars)
            else:
                template.apply("cisco-sr-te-cfp-copy-cfp-configurations")
            th.apply()
            self.log.info("Updated internal SR-TE CFP configurations")


class SrTeCleanupAction(ncs.dp.Action):
    """
    Action handler for SR TE services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(f"Cleanup Action for service name={name} service={input.service} "
                          f"device={input.device} service-type={input.service_type} "
                          f"no-networking={input.no_networking}")
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append(f"Cleaning up SR TE service: {input.service}")
            is_no_networking = input.no_networking
            service_type = input.service_type
            service = input.service
            device = input.device

            try:
                if service_type == "sr-odn":
                    service_kp = SrTeOdn.get_service_kp(service)
                    self._cleanup_odn_service(uinfo, service, device, is_no_networking,
                                              cleanup_log, root, th, service_type)
                else:
                    # Cleanup Policy service
                    service_kp = SrTePolicy.get_service_kp(service)
                    self._cleanup_policy_service(uinfo, service, device, is_no_networking,
                                                 cleanup_log, root, th, service_type)
            except Exception as e:
                exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                       .set_context("Cleanup Action", f"Cleanup failed for {service_type}")
                       .add_state(f"{service_type} Name", service)
                       .add_state("Device", device)
                       .finish())
                self.log.error(f"Cleanup Failed : {exp}")
                cleanup_log.append("\n Cleanup Failed \n\n")
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                # return the log
                self.log.info("Cleanup Successful")
                cleanup_log.append("\n Cleanup Successful")
                output.success = True
                output.detail = "".join(cleanup_log)
            finally:
                delete_service_cleanup_flag(self, service_kp, uinfo.username)

    def _cleanup_policy_service(self, uinfo, service, device, is_no_networking,
                                cleanup_log, root, th, service_type):
        force_back_track_comp = []
        remove_plan_paths = []

        # Detect if LSA setup or not to generate correct paths
        SrTePolicy.is_lsa_setup()

        policy_plan_path = SrTePolicy.get_plan_kp(service)
        zombie_service_path = SrTePolicy.get_service_xpath(service)
        service_path = SrTePolicy.get_service_kp(service)

        # Remove service if it exists
        set_service_cleanup_flag(self, service_path, uinfo.username)
        commit_params = ncs.maapi.CommitParams()
        if is_no_networking:
            commit_params.no_networking()
            if Utils.is_lsa:
                commit_params.no_lsa()
        CleanupUtils.delete_service(self, service_path, cleanup_log, uinfo, commit_params)

        # Collect service data to be cleaned up
        head_ends = []
        if th.exists(policy_plan_path) and (device is None):
            policy_components = root.cisco_sr_te_cfp__sr_te.\
                cisco_sr_te_cfp_sr_policies__policies.policy_plan[service].plan.component
            for component in policy_components:
                if (component.type == "cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end"):
                    head_ends.append(component.name)
                force_back_track_comp.append((component.force_back_track, zombie_service_path))
                if Utils.is_lsa:
                    remove_plan_paths.append("/cisco-tsdn-core-fp-common:rfs-policy-"
                                             f"plan{{{service} {component.name}}}")
            remove_plan_paths.append(policy_plan_path)
            self.log.info(f"head-ends for {service} policy service: {head_ends}")
        elif device is not None:
            head_ends.append(device)
            if th.exists(policy_plan_path):
                head_end_component = root.cisco_sr_te_cfp__sr_te.\
                    cisco_sr_te_cfp_sr_policies__policies.policy_plan[service].plan.component
                if ("cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end", device,) \
                        in head_end_component:
                    head_end_comp = head_end_component["cisco-sr-te-cfp-sr-policies-"
                                                       "nano-plan-services:head-end", device]
                    force_back_track_comp.append(
                        (head_end_comp.force_back_track, zombie_service_path))
                    remove_plan_paths.append(head_end_comp._path)
                    if Utils.is_lsa:
                        remove_plan_paths.append("/cisco-tsdn-core-fp-common:"
                                                 f"rfs-policy-plan{{{service} {device}}}")

        self._cleanup_service_data(uinfo, service, device, is_no_networking, cleanup_log,
                                   root, th, force_back_track_comp, head_ends, service_type,
                                   zombie_service_path, service_path, remove_plan_paths, "head-end")

    def _cleanup_odn_service(self, uinfo, service, device, is_no_networking,
                             cleanup_log, root, th, service_type):
        force_back_track_comp = []
        remove_plan_paths = []

        # Detect if LSA setup or not to generate correct paths
        SrTeOdn.is_lsa_setup()

        odn_plan_path = SrTeOdn.get_plan_kp(service)
        zombie_service_path = SrTeOdn.get_service_xpath(service)
        service_path = SrTeOdn.get_service_kp(service)

        commit_params = ncs.maapi.CommitParams()
        if is_no_networking:
            commit_params.no_networking()
            if Utils.is_lsa:
                commit_params.no_lsa()
        # If device, delete device if exists
        if device:
            service_device_kp = f"{service_path}/head-end{{{device}}}"
            CleanupUtils.delete_service(self, service_device_kp, cleanup_log, uinfo, commit_params)
        # Otherwise, delete service if exists
        else:
            set_service_cleanup_flag(self, service_path, uinfo.username)
            CleanupUtils.delete_service(self, service_path, cleanup_log, uinfo, commit_params)

        if th.exists(service_path):
            head_end_node = ncs.maagic.get_node(th, service_path + "/head-end")
        # Check if there is only one device in service ,same as the one requested,
        # cleanup should be on the entire service.
        if th.exists(service_path) and len(head_end_node) <= 1:
            # If the requested device is not in current service path,
            # cleanup only the requested device
            if (len(head_end_node) == 1 and device is not None
                    and device not in head_end_node):
                self.log.info(f"Clean up device not present in service: {device}")
            else:
                device = None

        # Collect service data to be cleaned up
        head_ends = []
        if th.exists(odn_plan_path) and (device is None):
            odn_components = (root.cisco_sr_te_cfp__sr_te
                              .cisco_sr_te_cfp_sr_odn__odn.odn_template_plan[service]
                              .plan.component)
            for component in odn_components:
                if (component.type == "cisco-sr-te-cfp-sr-odn-nano-plan-services:head-end"):
                    head_ends.append(component.name)
                force_back_track_comp.append((component.force_back_track, zombie_service_path))
                if Utils.is_lsa:
                    remove_plan_paths.append("/cisco-tsdn-core-fp-common:"
                                             f"rfs-odn-template-plan{{{service} {component.name}}}")
            remove_plan_paths.append(odn_plan_path)
            self.log.info(f"head-ends for {service} odn service: {head_ends}")
        elif device is not None:
            head_ends.append(device)
            if th.exists(odn_plan_path):
                head_end_component = (root.cisco_sr_te_cfp__sr_te.cisco_sr_te_cfp_sr_odn__odn
                                      .odn_template_plan[service].plan.component)
                if ("cisco-sr-te-cfp-sr-odn-nano-plan-services:head-end", device) \
                        in head_end_component:
                    head_end_comp = head_end_component["cisco-sr-te-cfp-sr-odn-nano"
                                                       "-plan-services:head-end", device]
                    force_back_track_comp.append(
                        (head_end_comp.force_back_track, zombie_service_path))
                    remove_plan_paths.append(head_end_comp._path)
                    if Utils.is_lsa:
                        remove_plan_paths.append("/cisco-tsdn-core-fp-common:"
                                                 f"rfs-odn-template-plan{{{service} {device}}}")

        self._cleanup_service_data(uinfo, service, device, is_no_networking, cleanup_log,
                                   root, th, force_back_track_comp, head_ends, service_type,
                                   zombie_service_path, service_path, remove_plan_paths,
                                   "head-end")

    def _cleanup_service_data(self, uinfo, service, device, is_no_networking, cleanup_log,
                              root, th, force_back_track_comp, internal_devices, service_type,
                              zombie_service_path, service_path, remove_plan_paths,
                              device_list_name_in_service):

        if len(internal_devices) > 0:
            # Check if internal service is local or on RFS node
            if Utils.is_lsa:
                # Get RFS Node to device mapping
                rfs_node_dev_dict = LsaUtils.get_device_remote_nso(internal_devices)
                self.log.info(f"rfs_node_dev_dict: {rfs_node_dev_dict}")
                # Invoke internal cleanup action on all RFS nodes with corresponding devices
                for rfs_node, rfs_node_devices in rfs_node_dev_dict.items():
                    internal_action = root.ncs__devices.device[rfs_node].config\
                        .cisco_sr_te_cfp_internal__sr_te.cleanup
                    self._call_internal_cleanup_action(internal_action, service, rfs_node_devices,
                                                       is_no_networking, service_type, cleanup_log)
            else:
                # Invoke internal cleanup action locally
                internal_action = root.cisco_sr_te_cfp_internal__sr_te.cleanup
                self._call_internal_cleanup_action(internal_action, service, internal_devices,
                                                   is_no_networking, service_type, cleanup_log)

        # In LSA, CFS cleanup messages will be sent to RFS, when no-networking is true
        # To avoid messages being sent to RFS, no-lsa should also be true
        if Utils.is_lsa:
            is_no_networking = True
            no_lsa = True
        else:
            no_lsa = False

        # force-backtrack all upper service components
        CleanupUtils.invoke_backtrack_actions(self, service, force_back_track_comp,
                                              is_no_networking, no_lsa)
        cleanup_log.append("\n Removed all external plan components")

        # remove zombies for upper service
        if device is None:
            CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        self.log.info(f"remove_plan_paths list {remove_plan_paths}")
        # Remove leftover plan paths
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)
        # remove zombies for upper service again
        # This is to cover the case where cleanup is requested without service delete.
        # In this case we will see race conditions where zombie is recreated
        # after we pass the zombie deletion check during removal of config-apply callbacks
        if device is None:
            CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        # Due to zombie removal race conditions
        # plan elements are partially recreated causing cleanup leftovers.
        # This does a final check on plan path removal.
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)

        # If the service is still present, we should re-deploy the service to reconcile
        # as the out of band deletion of data can leave the northbound plan in weird state.
        if device is not None and th.exists(service_path):
            external_service = ncs.maagic.get_node(th, service_path)
            external_service.reactive_re_deploy()

    def _call_internal_cleanup_action(self, internal_action, service, devices,
                                      is_no_networking, service_type, cleanup_log):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        internal_action_input.devices = devices
        internal_action_input.no_networking = is_no_networking
        internal_action_input.service_type = service_type
        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        # If success proceed with northbound service cleanup
        if internal_action_output.success:
            cleanup_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            cleanup_log.append(internal_action_output.detail)
            raise Exception("Internal Service Cleanup Failed")


class SrTeServiceRecoveryAction(ncs.dp.Action):
    """
    Action handler for SR TE services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"Recovery Action for service={input.service} "
                          f"device={input.device} "
                          f"service-type={input.service_type} ")
            root = maagic.get_root(th)
            recovery_log = []
            recovery_log.append(f"Recovering SR TE service: {input.service}")
            service_type = input.service_type
            service = input.service
            device = input.device
            sync_direction = input.sync_direction

            try:
                if service_type == "sr-odn":
                    self._recover_odn_service(service, device, root, th, service_type,
                                              recovery_log, sync_direction)
                else:
                    # recover Policy service
                    self._recover_policy_service(service, device, root, th,
                                                 service_type, recovery_log, sync_direction)
            except Exception as e:
                exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                       .set_context("Recovery Action", f"Recovery failed for {service_type}")
                       .add_state(f"{service_type} Name", service)
                       .add_state("Device", device)
                       .finish())
                self.log.error(f"Recovery Failed : {exp}")
                recovery_log.append("\nRecovery Failed\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            else:
                self.log.info("Recovery Complete")
                recovery_log.append("\nRecovery Complete\n")
                output.success = True
                output.detail = "".join(recovery_log)

    def _recover_odn_service(self, service, device, root, th, service_type,
                             recovery_log, sync_direction):

        odn_plan_path = SrTeOdn.get_plan_kp(service)

        head_end_comp_type = "cisco-sr-te-cfp-sr-odn-nano-plan-services:head-end"

        if not th.exists(odn_plan_path):
            raise Exception(f"No such service found for recovery: {service}")

        odn_plan = maagic.get_node(th, odn_plan_path).plan
        # To Find failed devices
        query_path = ("/cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-odn:odn/"
                      + "odn-template-plan[name='" + service + "']/plan/component"
                      + "[type='" + head_end_comp_type + "']/" + "state[status='failed']")

        self.recover_service(root, th, service, device, service_type, odn_plan,
                             head_end_comp_type, query_path, recovery_log, sync_direction)

    def _recover_policy_service(self, service, device, root, th, service_type,
                                recovery_log, sync_direction):

        policy_plan_path = SrTePolicy.get_plan_kp(service)

        head_end_comp_type = "cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end"

        if not th.exists(policy_plan_path):
            raise Exception(f"No such service found for recovery: {service}")

        policy_plan = maagic.get_node(th, policy_plan_path).plan
        # To Find failed devices
        query_path = ("/cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/"
                      + "policy-plan[name='" + service + "']/plan/component"
                      + "[type='" + head_end_comp_type + "']/" + "state[status='failed']")

        self.recover_service(root, th, service, device, service_type, policy_plan,
                             head_end_comp_type, query_path, recovery_log, sync_direction)

    def recover_service(self, root, th, service, device, service_type, service_plan,
                        head_end_comp_type, query_path, recovery_log, sync_direction):
        if device is not None:
            # Check if such a failed device exists in plan:
            if not (head_end_comp_type, device) in service_plan.component:
                raise Exception(f"No such failed device: {device} "
                                f"found for recovery in service: {service}")

            device_comp = service_plan.component[(head_end_comp_type, device)]
            if (device_comp.state["ncs:ready"].status != "failed"
                    and device_comp.state["ncs:init"].status != "failed"):
                raise Exception(f"Device: {device} is not in failed state in service: {service}")
            # Check if internal service is local or on RFS node
            if Utils.is_lsa:
                rfs_node = LsaUtils.get_remote_nso(device)
                internal_action = root.\
                    ncs__devices.device[rfs_node].config.\
                    cisco_sr_te_cfp_internal__sr_te.error_recovery
                self._call_internal_recovery_action(internal_action, service, [device],
                                                    service_type, recovery_log, sync_direction)
            else:
                # Invoke internal recovery action locally
                internal_action = root.cisco_sr_te_cfp_internal__sr_te.error_recovery
                self._call_internal_recovery_action(internal_action, service, [device],
                                                    service_type, recovery_log, sync_direction)
        else:
            failed_devices = self.get_failed_devices(th, query_path)

            self.log.info(f"Failed devices to recover: {failed_devices} on service: {service}")

            if len(failed_devices) == 0:
                raise Exception(f"No failed devices to recover in service: {service}")

            if Utils.is_lsa:
                # Get RFS Node to device mapping
                rfs_node_dev_dict = LsaUtils.get_device_remote_nso(failed_devices)
                self.log.info(f"rfs_node_dev_dict: {rfs_node_dev_dict}")
                # Invoke internal recovery action on all RFS nodes with corresponding devices
                for rfs_node, rfs_node_devices in rfs_node_dev_dict.items():
                    internal_action = root.ncs__devices.device[rfs_node].config.\
                        cisco_sr_te_cfp_internal__sr_te.error_recovery
                    self._call_internal_recovery_action(internal_action, service, rfs_node_devices,
                                                        service_type, recovery_log, sync_direction)
            else:
                # Invoke internal recovery action locally
                internal_action = root.cisco_sr_te_cfp_internal__sr_te.error_recovery
                self._call_internal_recovery_action(internal_action, service, failed_devices,
                                                    service_type, recovery_log, sync_direction)

    def _call_internal_recovery_action(self, internal_action, service, devices,
                                       service_type, recovery_log, sync_direction):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        internal_action_input.devices = devices
        internal_action_input.service_type = service_type
        internal_action_input.sync_direction = sync_direction
        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        if internal_action_output.success:
            recovery_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            recovery_log.append(internal_action_output.detail)
            raise Exception("Service Recovery Failed")

    def get_failed_devices(self, trans, query_path):
        failed_devices = []
        qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                    query_path, "/", 0, 1,
                                    _ncs.QUERY_STRING, ["../name"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)
        for r in res:
            failed_devices.append(r[0])
        _ncs.maapi.query_stop(trans.maapi.msock, qh)

        return failed_devices
