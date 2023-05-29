import ncs
import _ncs
import re
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic

from . import utils as Utils
from core_fp_common import cleanup_utils as CleanupUtils
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from cisco_tsdn_core_fp_common.ietf_te import IetfTe
from cisco_tsdn_core_fp_common.status_codes.ietf_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.ietf_te_base_exception import CustomActionException
from core_fp_common.common_utils import get_local_user
from traceback import print_exc
from cisco_tsdn_core_fp_common.utils import (
    set_service_cleanup_flag, delete_service_cleanup_flag,
    check_service_cleanup_flag, get_action_timeout, get_all_rfs_nodes
)


class InternalPlanChangeHandler(ncs.dp.Action):
    """
    Action handler for RSVP-TE plan change
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Internal plan kicker changed for: {input.kicker_id} "
                      f"{input.path} {input.tid}")

        ietf_wrapper = IetfTe(input.path, self.log)

        # If cleanup is in progress, do not take any action
        if check_service_cleanup_flag(self.log, ietf_wrapper.service_kp, uinfo.username):
            return

        try:
            ietf_wrapper.redeploy()
        except Exception:
            print_exc()

        self.log.info(f"Internal plan change handled for: {input.path}")


class IETFTeSelfTest(ncs.dp.Action):
    """
    Action handler for self-test
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Running self test for: {kp}")
        username = uinfo.username
        status = None
        final_message = None
        with ncs.maapi.single_read_trans(username, "system") as th:
            root = ncs.maagic.get_root(th)

            # Get service node from action path
            action = ncs.maagic.get_node(th, kp)
            ietf_te_service = ncs.maagic.cd(action, "..")

            try:
                messages = []
                # Invoke internal self-test on source
                rsvp_te_service_name = (ietf_te_service.name + "-"
                                        + ietf_te_service.source + "-internal")
                head_end = ietf_te_service.head_end
                (status, message) = self._call_self_test_on_endpoint(root, head_end,
                                                                     rsvp_te_service_name)
                if message is not None:
                    messages.append(message + " for Source.")

                # If self test is successful from head-end and the tunnel is bidirectional,
                # invoke internal self-test on destination
                if status == "success" and ietf_te_service.bidirectional is True:
                    rsvp_te_service_name = (ietf_te_service.name + "-"
                                            + ietf_te_service.destination + "-internal")

                    tail_end = ietf_te_service.tail_end
                    (status, tail_end_message) = self._call_self_test_on_endpoint(
                        root, tail_end, rsvp_te_service_name)

                    if tail_end_message is not None:
                        messages.append(tail_end_message + " for Destination.")

                if len(messages) > 0:
                    final_message = "".join(messages)

                if status != "success" and status != "failed":
                    raise CustomActionException(
                        self.log, StatusCodes.SELF_TEST_STATUS_ERROR
                    ).set_context(
                        "Self Test Error", "Unsupported status returned"
                    ).add_state(
                        "Status", status
                    ).add_state(
                        "IETF-TE Service Name", ietf_te_service.name
                    ).add_state(
                        "Keypath", str(kp)
                    ).finish()

            except CustomActionException:
                raise
            except Exception as e:
                print_exc()
                exp = (
                    CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))
                    .set_context("Self Test Error", "Self Test Failed")
                    .finish()
                )
                status = "failed"
                final_message = "Error: " + str(exp)

        output.status = status
        output.message = final_message

    def _call_self_test_on_endpoint(self, root, head_end, rsvp_te_service_name):
        if Utils.is_lsa:
            rfs_node = LsaUtils.get_remote_nso(head_end)
            internal_action = (root.ncs__devices.device[rfs_node].config.
                               cisco_rsvp_te_fp__rsvp_te.tunnel_te[rsvp_te_service_name]
                               .action.self_test)
            return self._call_internal_selftest_action(internal_action)
        else:
            # Invoke self-test action locally
            internal_action = root.cisco_rsvp_te_fp__rsvp_te.tunnel_te[
                rsvp_te_service_name
            ].action.self_test
            return self._call_internal_selftest_action(internal_action)

    def _call_internal_selftest_action(self, internal_action):
        internal_action_output = internal_action()
        return (internal_action_output.status, internal_action_output.message)


class UpdateInternalCfpConfigurations(ncs.dp.Action):
    """
    Action Handler for RSVP-TE CFP Configurations
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        username = get_local_user()
        with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            template = ncs.template.Template(root.te__cfp_configurations)
            if Utils.is_lsa:
                # Get all RFS nodes
                create_vars = ncs.template.Variables()
                for rfs_node in get_all_rfs_nodes(self):
                    create_vars.add("RFS_NODE", rfs_node)
                    template.apply("cisco-rsvp-te-fp-copy-fp-configurations", create_vars)
            else:
                template.apply("cisco-rsvp-te-fp-copy-fp-configurations")
            th.apply()
            self.log.info("Updated internal IETF-TE CFP configurations")


class IETFTeCleanupAction(ncs.dp.Action):
    """
    Action handler for IETF TE services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(f"Cleanup Action for service name={name} service={input.service} "
                          f"no-networking={input.no_networking}")
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append(f"Cleaning up IETF TE service: {input.service}")
            is_no_networking = input.no_networking
            service = input.service
            service_kp = f"/te:te/tunnels/tunnel{{{service}}}"

            try:
                set_service_cleanup_flag(self, service_kp, uinfo.username)
                self._cleanup_ietf_service(uinfo, service, is_no_networking,
                                           cleanup_log, root, th)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                    .set_context("Cleanup Action", "Cleanup failed for")
                    .add_state("Name", service)
                    .finish()
                )
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

    def _cleanup_ietf_service(self, uinfo, service, is_no_networking, cleanup_log, root, th):
        force_back_track_comp = []
        remove_plan_paths = []

        service_plan_path = f"/te:te/tunnels/tunnel-plan{{{service}}}"
        zombie_service_path = f"/te/tunnels/tunnel[name='{service}']"

        service_path = f"/te:te/tunnels/tunnel{{{service}}}"

        commit_params = ncs.maapi.CommitParams()
        if is_no_networking:
            commit_params.no_networking()
            if Utils.is_lsa:
                commit_params.no_lsa()
        # Delete service if exists
        CleanupUtils.delete_service(self, service_path, cleanup_log,
                                    uinfo, commit_params)

        # Collect service data to be cleaned up
        internal_device_service_dict = {}
        if th.exists(service_plan_path):
            components = root.te__te.tunnels.tunnel_plan[service].plan.component
            for component in components:
                if (component.type == "ietf-te-fp-tunnel-nano-plan-services:source"
                        or component.type == "ietf-te-fp-tunnel-nano-plan-services:destination"):
                    internal_device_service_dict[
                        component.private.property_list.property["DEVICE"].value
                    ] = component.private.property_list.property[
                        "INTERNAL_SERVICE_NAME"
                    ].value
                force_back_track_comp.append((component.force_back_track, zombie_service_path))
            if Utils.is_lsa:
                remove_plan_paths.append(f"/cisco-tsdn-core-fp-common:rfs-rsvp-plan{{{service}}}")
            remove_plan_paths.append(service_plan_path)

        self._cleanup_service_data(uinfo, service, is_no_networking, cleanup_log, root, th,
                                   force_back_track_comp, internal_device_service_dict,
                                   zombie_service_path, service_path, remove_plan_paths)

    def _cleanup_service_data(self, uinfo, service, is_no_networking, cleanup_log, root, th,
                              force_back_track_comp, internal_device_service_dict,
                              zombie_service_path, service_path, remove_plan_paths):

        for device, internal_service in internal_device_service_dict.items():
            # Check if internal service is local or on RFS node
            if Utils.is_lsa:
                # Get RFS Node to device mapping
                rfs_node = LsaUtils.get_remote_nso(device)
                # Invoke internal cleanup action on all RFS nodes with corresponding devices
                internal_action = root.\
                    ncs__devices.device[rfs_node].config.cisco_rsvp_te_fp__rsvp_te.cleanup
                self._call_internal_cleanup_action(internal_action, internal_service,
                                                   is_no_networking, cleanup_log)
            else:
                # Invoke internal cleanup action locally
                internal_action = root.cisco_rsvp_te_fp__rsvp_te.cleanup
                self._call_internal_cleanup_action(internal_action, internal_service,
                                                   is_no_networking, cleanup_log)

        # In LSA, CFS cleanup messages will be sent to RFS, when no-networking is true
        # To avoid messages being sent to RFS, no-lsa should also be true
        if is_no_networking and Utils.is_lsa:
            no_lsa = True
        else:
            no_lsa = False

        # force-backtrack all upper service components - RT#48440 Filed
        CleanupUtils.invoke_backtrack_actions(self, service, force_back_track_comp,
                                              is_no_networking, no_lsa)
        cleanup_log.append("\n Removed all external plan components")

        # remove zombies for upper service
        CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        self.log.info(f"remove_plan_paths list {remove_plan_paths}")
        # Remove leftover plan paths
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)
        # remove zombies for upper service again
        # This is to cover the case where cleanup is requested without service delete.
        # In this case we will see race conditions where zombie is recreated
        # after we pass the zombie deletion check during removal of config-apply callbacks
        CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        # Due to zombie removal race conditions
        # plan elements are partially recreated causing cleanup leftovers.
        # This does a final check on plan path removal.
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)

    def _call_internal_cleanup_action(self, internal_action, internal_service,
                                      is_no_networking, cleanup_log):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = internal_service
        internal_action_input.no_networking = is_no_networking
        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        # If success proceed with northbound service cleanup
        if internal_action_output.success:
            cleanup_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            cleanup_log.append(internal_action_output.detail)
            raise Exception("Internal Service Cleanup Failed")


class IETFTeServiceErrorRecovery(ncs.dp.Action):
    """
    Action handler for flat l2vpn services site error recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Recovery Action for kp={kp} source={input.source} "
                      f"destination={input.destination} ")
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            re_search = re.search("tunnel{(.*?)}", str(kp))
            service = re_search.group(1)

            # Get source and destination from action inputs
            source = input.source
            destination = input.destination

            # Invoke Main error recovery action for this service
            recovery_action = root.te__te.tunnels.actions.error_recovery
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service
            recovery_action_input.sync_direction = input.sync_direction
            if source is not None:
                recovery_action_input.source = source
            if destination is not None:
                recovery_action_input.destination = destination

            recovery_action_output = recovery_action(recovery_action_input)
            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail


class IETFTeErrorRecovery(ncs.dp.Action):
    """
    Action handler for IETF TE error recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"Recovery Action for service={input.service} "
                          f"source={input.source} destination={input.destination} ")
            root = maagic.get_root(th)
            recovery_log = []
            recovery_log.append(f"Recovering IETF TE service: {input.service}")
            service = input.service
            source = input.source
            destination = input.destination
            sync_direction = input.sync_direction
            try:
                self._recover_ietf_service(service, source, destination, root, th,
                                           recovery_log, sync_direction)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                    .set_context("Recovery Action", f"Recovery failed for {service}")
                    .finish()
                )
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

    def _recover_ietf_service(self, service, source, destination, root, th,
                              recovery_log, sync_direction):

        service_plan_path = f"/te:te/tunnels/tunnel-plan{{{service}}}"

        if not th.exists(service_plan_path):
            raise Exception(f"No such service found for recovery: {service}")

        device = None
        internal_service_name = None
        internal_failed_device_service_dict = {}
        components = root.te__te.tunnels.tunnel_plan[service].plan.component
        node_value = None
        if source is not None:
            node_value = source
            if ("ietf-te-fp-tunnel-nano-plan-services:source", source) in components:
                device, internal_service_name = self.get_failed_device(service, "source",
                                                                       source, components)
        elif destination is not None:
            node_value = destination
            if ("ietf-te-fp-tunnel-nano-plan-services:destination",
                    destination) in components:
                device, internal_service_name = self.get_failed_device(service, "destination",
                                                                       destination, components)

        # If source or destination is provided, but neither is in failed state
        if node_value is not None and device is None:
            raise Exception(f"No such failed node {node_value} found for "
                            f"recovery in service: {service}")
        # If source or destination is not provided, collect data for the complete service
        else:
            for component in components:
                if (component.type == "ietf-te-fp-tunnel-nano-plan-services:source"
                        or component.type == "ietf-te-fp-tunnel-nano-plan-services:destination"):
                    # Check if such a failed device exists in plan.
                    # If it does, collect the internal service details
                    if (component.state["ncs:ready"].status == "failed"
                            or component.state["ncs:init"].status == "failed"):
                        device = component.private.property_list.property[
                            "DEVICE"
                        ].value
                        internal_service_name = component.\
                            private.property_list.property["INTERNAL_SERVICE_NAME"].value
                        internal_failed_device_service_dict[device] = internal_service_name
            if len(internal_failed_device_service_dict) < 1:
                raise Exception(f"No failed devices found for recovery in service: {service}")

        self.recover_service(root, th, device, internal_service_name,
                             internal_failed_device_service_dict, recovery_log, sync_direction)

    def get_failed_device(self, service, node_type, node_value, components):
        component_type = "ietf-te-fp-tunnel-nano-plan-services:" + node_type
        component = components[(component_type, node_value)]
        if (component.state["ncs:ready"].status != "failed"
                and component.state["ncs:init"].status != "failed"):
            raise Exception(f"Node: {node_value} is not in failed state in service: {service}")
        device = component.private.property_list.property["DEVICE"].value
        internal_service_name = component.\
            private.property_list.property["INTERNAL_SERVICE_NAME"].value
        return (device, internal_service_name)

    def recover_service(self, root, th, device, internal_service_name,
                        internal_failed_device_service_dict, recovery_log, sync_direction):
        if device is not None:
            self.recover_internal_service(root, device, internal_service_name,
                                          recovery_log, sync_direction)
        else:
            for device, internal_service in internal_failed_device_service_dict.items():
                self.recover_internal_service(root, device, internal_service,
                                              recovery_log, sync_direction)

    def recover_internal_service(self, root, device, internal_service,
                                 recovery_log, sync_direction):
        # Check if internal service is local or on RFS node
        if Utils.is_lsa:
            # Get RFS Node to device mapping
            rfs_node = LsaUtils.get_remote_nso(device)
            # Invoke internal cleanup action on all RFS nodes with corresponding devices
            internal_action = root.ncs__devices.device[
                rfs_node
            ].config.cisco_rsvp_te_fp__rsvp_te.error_recovery
            self._call_internal_recovery_action(internal_action, internal_service,
                                                recovery_log, sync_direction)
        else:
            # Invoke internal cleanup action locally
            internal_action = root.cisco_rsvp_te_fp__rsvp_te.error_recovery
            self._call_internal_recovery_action(internal_action, internal_service,
                                                recovery_log, sync_direction)

    def _call_internal_recovery_action(self, internal_action, service,
                                       recovery_log, sync_direction):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        internal_action_input.sync_direction = sync_direction
        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        if internal_action_output.success:
            recovery_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            recovery_log.append(internal_action_output.detail)
            raise Exception("Service Recovery Failed")
