import ncs
import _ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
from core_fp_common import cleanup_utils as CleanupUtils
from cisco_tsdn_core_fp_common.status_codes.rsvp_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.rsvp_te_fp_base_exception import CustomActionException
from cisco_tsdn_core_fp_common import recovery_utils as RecoveryUtils
from . import utils as Utils
from traceback import print_exc
from cisco_tsdn_core_fp_common.utils import get_action_timeout


class RsvpTeCleanupAction(ncs.dp.Action):
    """
    Action handler for RSVP TE services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(f"Cleanup Action for internal service name={name} "
                          f"service={input.service} "
                          f"no-networking={input.no_networking}")
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append(f"Cleaning up RSVP TE Internal Services: {input.service}")
            is_no_networking = input.no_networking
            service = input.service

            try:
                self._cleanup_rsvp_service(uinfo, service, is_no_networking,
                                           cleanup_log, root, th)
            except Exception as e:
                exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                       .set_context("Cleanup Action", "Cleanup failed for")
                       .add_state("Name", service).finish())
                self.log.error(f"Cleanup Failed for Internal RSVP TE Services : {exp}")
                cleanup_log.append("\n Cleanup Failed for Internal RSVP TE Services\n\n")
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                # return the log
                self.log.info("Cleanup Successful for RSVP TE Internal Services")
                cleanup_log.append("\n Cleanup Successful for RSVP TE Internal Services")
                output.success = True
                output.detail = "".join(cleanup_log)

    def _cleanup_rsvp_service(self, uinfo, service, is_no_networking,
                              cleanup_log, root, th):
        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        cq_error_recovery_paths = []
        remove_plan_paths = []
        failed_device_path = root.\
            cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device
        internal_rsvp_plan = root.cisco_rsvp_te_fp__rsvp_te.tunnel_te_plan

        # Delete service if exists
        service_kp = f"/cisco-rsvp-te-fp:rsvp-te/tunnel-te{{{service}}}"
        CleanupUtils.delete_service(self, service_kp, cleanup_log, uinfo)

        if service in internal_rsvp_plan:
            internal_rsvp_components = internal_rsvp_plan[service].plan.component

            internal_zombie_path = f"/rsvp-te/tunnel-te[name='{service}']"
            internal_service_path = f"/cisco-rsvp-te-fp:rsvp-te/tunnel-te{{{service}}}"
            internal_zombie_paths.append(internal_zombie_path)
            internal_service_paths.append(internal_service_path)
            remove_plan_paths.append(internal_rsvp_plan[service]._path)

            for component in internal_rsvp_components:
                device = component.private.property_list.property["HEADEND"].value

                internal_force_back_track_comp.append(
                    (component.force_back_track, internal_zombie_path))
                if device in failed_device_path:
                    cq_err_serv_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                        f"failed-device{{{device}}}/"
                                        f'impacted-service-path{{"{internal_service_path}"}}')
                    zombie_path = f"/ncs:zombies/service{{{internal_zombie_path}}}"
                    cq_err_zombie_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                          f"failed-device{{{device}}}/"
                                          f'impacted-service-path{{"{zombie_path}"}}')
                    cq_error_recovery_paths.append((device, cq_err_serv_path))
                    cq_error_recovery_paths.append((device, cq_err_zombie_path))

        self._cleanup_service_data(uinfo, service, device, is_no_networking, cleanup_log,
                                   internal_force_back_track_comp, internal_zombie_paths,
                                   internal_service_paths, cq_error_recovery_paths,
                                   root, th, remove_plan_paths)

    def _cleanup_service_data(self, uinfo, service, device, is_no_networking, cleanup_log,
                              internal_force_back_track_comp, internal_zombie_paths,
                              internal_service_paths, cq_error_recovery_paths,
                              root, th, remove_plan_paths):

        # force-backtrack all lower head-end services - RT#48440 Filed
        CleanupUtils.invoke_backtrack_actions(self, service, internal_force_back_track_comp,
                                              is_no_networking)
        cleanup_log.append("\n Removed all internal plan components")

        #  remove zombies for lower services
        for internal_zombie_path in internal_zombie_paths:
            CleanupUtils.remove_zombie(self, internal_zombie_path, cleanup_log, uinfo)

        #  delete lower services if exists
        for internal_service_path in internal_service_paths:
            CleanupUtils.delete_service(self, internal_service_path, cleanup_log, uinfo)

        #  Remove any commit-queue error recovery poller paths if exists
        for (device, cq_error_recovery_path) in cq_error_recovery_paths:
            RecoveryUtils.remove_cq_recovery_data(self, cq_error_recovery_path, device,
                                                  cleanup_log, uinfo)
        cleanup_log.append("\n Removed commit-queue-recovery-data")

        self.log.info(f"remove_plan_paths list {remove_plan_paths}")
        #  Remove leftover plan paths
        # This can happen when forcebacktrack has failed if no-networking false is requested
        # on failing device during cleanup
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)


class RsvpTeRecovery(ncs.dp.Action):
    """
    Action handler for RSVP TE services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"Recovery Action for internal service={input.service}")
            root = maagic.get_root(th)
            recovery_log = []
            device_recovery_error = {}
            service = input.service
            sync_direction = input.sync_direction
            self._recover_rsvp_service(uinfo, service, recovery_log,
                                       device_recovery_error,
                                       root, th, sync_direction)
            if len(device_recovery_error) > 0:
                # return the log
                self.log.info("Recovery Incomplete for RSVP TE Services")
                recovery_log.append("\nWARNING: ")
                recovery_log.append(str(device_recovery_error))
                recovery_log.append("\nRecovery Incomplete for RSVP TE Services")
                output.success = False
                output.detail = "".join(recovery_log)
            else:
                self.log.info("Recovery Complete for RSVP TE Services")
                recovery_log.append("\nRecovery Complete for RSVP TE Services")
                output.success = True
                output.detail = "".join(recovery_log)

    def _recover_rsvp_service(self, uinfo, service, recovery_log,
                              device_recovery_error,
                              root, th, sync_direction):

        internal_plan = root.cisco_rsvp_te_fp__rsvp_te.tunnel_te_plan

        internal_zombie_path = f"/rsvp-te/tunnel-te[name='{service}']"
        internal_service_path = f"/cisco-rsvp-te-fp:rsvp-te/tunnel-te{{{service}}}"

        if service in internal_plan:
            rsvp_plan = internal_plan[service].plan
            if rsvp_plan.failed and rsvp_plan.error_info:
                device = (
                    rsvp_plan.component["ncs:self", "self"]
                    .private.property_list.property["HEADEND"].value)

                RecoveryUtils.recover_service(self, root, th, uinfo, rsvp_plan, device,
                                              internal_zombie_path, internal_service_path,
                                              recovery_log, device_recovery_error,
                                              service, sync_direction)
            else:
                device = rsvp_plan.component["ncs:self", "self"].private.\
                    property_list.property["HEADEND"].value
                recovery_log.append(f"\nNo failure found for {device} in service {service}")


class RsvpTeSelfTest(ncs.dp.Action):
    """
    Action handler for self-test for all head-ends
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running self test for: {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        username = uinfo.username
        with ncs.maapi.single_read_trans(username, "system") as th:
            root = ncs.maagic.get_root(th)

            #  Get service node from action path
            action = ncs.maagic.get_node(th, kp)
            rsvp_te_service = ncs.maagic.cd(action, "..")

            try:
                router = Utils.get_device_impl_default_class(
                    self, root, rsvp_te_service, rsvp_te_service.head_end,
                    root.cisco_rsvp_te_fp__cfp_configurations.dynamic_device_mapping,
                )

                (status, message) = router.ietf_te_self_test(uinfo, root, rsvp_te_service)
            except Exception as e:
                print_exc()
                exp = (CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))
                       .set_context("Self Test Error", "Self Test Failed").finish())
                status = "failed"
                message = "Error: " + str(exp)

            output.status = status
            output.message = message
