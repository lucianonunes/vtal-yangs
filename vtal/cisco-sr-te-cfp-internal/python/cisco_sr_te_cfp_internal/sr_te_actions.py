import ncs
import _ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
from core_fp_common import cleanup_utils as CleanupUtils
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import CustomActionException
from cisco_tsdn_core_fp_common import recovery_utils as RecoveryUtils
from cisco_tsdn_core_fp_common.utils import get_action_timeout


class SrTeCleanupAction(ncs.dp.Action):
    """
    Action handler for SR TE services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(f"Cleanup Action for internal service name={name} "
                          f"service={input.service} devices={input.devices} "
                          f"service-type={input.service_type} no-networking={input.no_networking}")
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append(f"Cleaning up SR TE Internal Services: {input.service}")
            is_no_networking = input.no_networking
            service_type = input.service_type
            service = input.service
            devices = input.devices

            try:
                if service_type == "sr-odn":
                    self._cleanup_odn_service(uinfo, service, devices, is_no_networking,
                                              cleanup_log, root, th)
                else:
                    # Cleanup Policy service
                    self._cleanup_policy_service(uinfo, service, devices, is_no_networking,
                                                 cleanup_log, root, th)
            except Exception as e:
                exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                       .set_context("Cleanup Action", f"Cleanup failed for {service_type}")
                       .add_state(f"{service_type} Name", service).finish())
                self.log.error(f"Cleanup Failed for Internal SR TE Services : {exp}")
                cleanup_log.append("\n Cleanup Failed for Internal SR TE Services\n\n")
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                # return the log
                self.log.info("Cleanup Successful for SR TE Internal Services")
                cleanup_log.append("\n Cleanup Successful for SR TE Internal Services")
                output.success = True
                output.detail = "".join(cleanup_log)

    def _cleanup_policy_service(self, uinfo, service, head_ends,
                                is_no_networking, cleanup_log, root, th):
        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        cq_error_recovery_paths = []
        remove_plan_paths = []
        failed_device_path = root.\
            cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device

        for head_end in head_ends:
            internal_zombie_path = ("/sr-te/cisco-sr-te-cfp-sr-policies-internal:"
                                    f"policies/policy[name='{service}'][head-end='{head_end}']")
            if Utils.external_service_model_exists(root):
                internal_zombie_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                        "cisco-sr-te-cfp-sr-policies-internal:"
                                        f"policies/policy[name='{service}'][head-end='{head_end}']")
            internal_policy_plan = root.\
                cisco_sr_te_cfp_internal__sr_te.\
                cisco_sr_te_cfp_sr_policies_internal__policies.policy_plan
            if (service, head_end) in internal_policy_plan:
                internal_policy_components = internal_policy_plan[service, head_end].plan.component
                for component in internal_policy_components:
                    # Only self component in internal service
                    internal_force_back_track_comp.append(
                        (component.force_back_track, internal_zombie_path))
                remove_plan_paths.append(internal_policy_plan[service, head_end]._path)

            internal_service_path = (
                "/cisco-sr-te-cfp-internal:sr-te/cisco-sr-te-cfp-sr-policies-internal:policies/"
                f"policy{{{service} {head_end}}}"
            )
            internal_zombie_paths.append(internal_zombie_path)
            internal_service_paths.append(internal_service_path)

            if head_end in failed_device_path:
                cq_err_serv_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                    f"failed-device{{{head_end}}}/"
                                    f'impacted-service-path{{"{internal_service_path}"}}')
                zombie_path = f"/ncs:zombies/service{{{internal_zombie_path}}}"
                cq_err_zombie_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                      f"failed-device{{{head_end}}}/"
                                      f'impacted-service-path{{"{zombie_path}"}}')
                cq_error_recovery_paths.append((head_end, cq_err_serv_path))
                cq_error_recovery_paths.append((head_end, cq_err_zombie_path))

        self._cleanup_service_data(uinfo, service, is_no_networking, cleanup_log,
                                   internal_force_back_track_comp, internal_zombie_paths,
                                   internal_service_paths, cq_error_recovery_paths,
                                   remove_plan_paths)

    def _cleanup_odn_service(self, uinfo, service, head_ends, is_no_networking,
                             cleanup_log, root, th):
        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        cq_error_recovery_paths = []
        remove_plan_paths = []
        failed_device_path = root.\
            cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device

        for head_end in head_ends:
            internal_zombie_path = ("/sr-te/cisco-sr-te-cfp-sr-odn-internal:"
                                    f"odn/odn-template[name='{service}'][head-end='{head_end}']")
            if Utils.external_service_model_exists(root):
                internal_zombie_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                        "cisco-sr-te-cfp-sr-odn-internal:"
                                        f"odn/odn-template[name='{service}']"
                                        f"[head-end='{head_end}']")
            internal_service_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                     "cisco-sr-te-cfp-sr-odn-internal:odn/"
                                     f"odn-template{{{service} {head_end}}}")
            internal_odn_plan = (root.cisco_sr_te_cfp_internal__sr_te
                                 .cisco_sr_te_cfp_sr_odn_internal__odn
                                 .odn_template_plan)
            if (service, head_end) in internal_odn_plan:
                internal_odn_components = internal_odn_plan[service, head_end].plan.component
                for component in internal_odn_components:
                    # Only self component in internal service
                    internal_force_back_track_comp.append(
                        (component.force_back_track, internal_zombie_path))
                remove_plan_paths.append(internal_odn_plan[service, head_end]._path)

            internal_zombie_paths.append(internal_zombie_path)
            internal_service_paths.append(internal_service_path)
            if head_end in failed_device_path:
                cq_err_serv_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                    f"failed-device{{{head_end}}}/"
                                    f'impacted-service-path{{"{internal_service_path}"}}')
                zombie_path = f"/ncs:zombies/service{{{internal_zombie_path}}}"
                cq_err_zombie_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                      f"failed-device{{{head_end}}}/"
                                      f'impacted-service-path{{"{zombie_path}"}}')
                cq_error_recovery_paths.append((head_end, cq_err_serv_path))
                cq_error_recovery_paths.append((head_end, cq_err_zombie_path))

        self._cleanup_service_data(uinfo, service, is_no_networking, cleanup_log,
                                   internal_force_back_track_comp, internal_zombie_paths,
                                   internal_service_paths, cq_error_recovery_paths,
                                   remove_plan_paths)

    def _cleanup_service_data(self, uinfo, service, is_no_networking, cleanup_log,
                              internal_force_back_track_comp, internal_zombie_paths,
                              internal_service_paths, cq_error_recovery_paths,
                              remove_plan_paths):
        # force-backtrack all lower head-end services - RT#48440 Filed
        CleanupUtils.invoke_backtrack_actions(self, service, internal_force_back_track_comp,
                                              is_no_networking)

        cleanup_log.append("\n Removed all internal plan components")

        # remove zombies for lower services
        for internal_zombie_path in internal_zombie_paths:
            CleanupUtils.remove_zombie(self, internal_zombie_path, cleanup_log, uinfo)

        # delete lower services if exists
        for internal_service_path in internal_service_paths:
            CleanupUtils.delete_service(self, internal_service_path, cleanup_log, uinfo)

        # Remove any commit-queue error recovery poller paths if exists
        self.log.info(f"cq_error_recovery_paths {cq_error_recovery_paths}")
        for (device, cq_error_recovery_path) in cq_error_recovery_paths:
            RecoveryUtils.remove_cq_recovery_data(self, cq_error_recovery_path, device,
                                                  cleanup_log, uinfo)
        cleanup_log.append("\n Removed commit-queue-recovery-data")

        self.log.info(f"remove_plan_paths list {remove_plan_paths}")
        # Remove leftover plan paths
        # This can happen when forcebacktrack has failed if no-networking false is requested
        # on failing device during cleanup
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)


class SrTeServiceRecoveryAction(ncs.dp.Action):
    """
    Action handler for SR TE services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"Recovery Action for internal service={input.service} "
                          f"devices={input.devices} "
                          f"service-type={input.service_type} ")
            root = maagic.get_root(th)
            recovery_log = []
            device_recovery_error = {}
            service_type = input.service_type
            service = input.service
            devices = input.devices
            sync_direction = input.sync_direction

            try:
                if service_type == "sr-odn":
                    self._recover_odn_service(uinfo, service, devices, recovery_log,
                                              device_recovery_error, root, th, sync_direction)
                else:
                    # Cleanup Policy service
                    self._recover_policy_service(uinfo, service, devices, recovery_log,
                                                 device_recovery_error, root, th, sync_direction)
            except Exception as e:
                exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                       .set_context("Recovery Action",
                                    f"Recovery failed for {service_type}")
                       .add_state(f"{service_type} Name", service)
                       .finish())
                self.log.error(f"Recovery Failed for Internal SR TE Services : {exp}")
                recovery_log.append("\nRecovery Failed for Internal SR TE Services\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            else:
                if len(device_recovery_error) > 0:
                    # return the log
                    self.log.info("Recovery Incomplete for SR TE Internal Services")
                    recovery_log.append("\nWARNING: ")
                    recovery_log.append(str(device_recovery_error))
                    recovery_log.append("\nRecovery Incomplete for SR TE Internal Services")
                    output.success = False
                    output.detail = "".join(recovery_log)
                else:
                    self.log.info("Recovery Complete for SR TE Internal Services")
                    recovery_log.append("\nRecovery Complete for SR TE Internal Services")
                    output.success = True
                    output.detail = "".join(recovery_log)

    def _recover_odn_service(self, uinfo, service, head_ends, recovery_log,
                             device_recovery_error, root, th, sync_direction):
        for head_end in head_ends:
            internal_odn_plan = (root.cisco_sr_te_cfp_internal__sr_te
                                 .cisco_sr_te_cfp_sr_odn_internal__odn
                                 .odn_template_plan)

            internal_zombie_path = ("/sr-te/cisco-sr-te-cfp-sr-odn-internal:"
                                    f"odn/odn-template[name='{service}'][head-end='{head_end}']")
            if Utils.external_service_model_exists(root):
                internal_zombie_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                        "cisco-sr-te-cfp-sr-odn-internal:"
                                        f"odn/odn-template[name='{service}']"
                                        f"[head-end='{head_end}']")

            internal_service_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                     "cisco-sr-te-cfp-sr-odn-internal:odn/"
                                     f"odn-template{{{service} {head_end}}}")

            if (service, head_end) in internal_odn_plan:
                odn_plan = internal_odn_plan[service, head_end].plan
                if odn_plan.failed and odn_plan.error_info:
                    RecoveryUtils.recover_service(self, root, th, uinfo, odn_plan,
                                                  head_end, internal_zombie_path,
                                                  internal_service_path, recovery_log,
                                                  device_recovery_error, service, sync_direction)
                else:
                    recovery_log.append(f"\nNo failure found for {head_end} in service {service}")

    def _recover_policy_service(self, uinfo, service, head_ends, recovery_log,
                                device_recovery_error, root, th, sync_direction):

        for head_end in head_ends:
            internal_policy_plan = root.\
                cisco_sr_te_cfp_internal__sr_te.\
                cisco_sr_te_cfp_sr_policies_internal__policies.policy_plan

            internal_zombie_path = ("/sr-te/cisco-sr-te-cfp-sr-policies-internal:"
                                    f"policies/policy[name='{service}'][head-end='{head_end}']")
            if Utils.external_service_model_exists(root):
                internal_zombie_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                        "cisco-sr-te-cfp-sr-policies-internal:"
                                        f"policies/policy[name='{service}'][head-end='{head_end}']")

            internal_service_path = ("/cisco-sr-te-cfp-internal:sr-te/"
                                     "cisco-sr-te-cfp-sr-policies-internal:policies/"
                                     f"policy{{{service} {head_end}}}")

            if (service, head_end) in internal_policy_plan:
                policy_plan = internal_policy_plan[service, head_end].plan
                if policy_plan.failed and policy_plan.error_info:
                    RecoveryUtils.recover_service(self, root, th, uinfo, policy_plan, head_end,
                                                  internal_zombie_path, internal_service_path,
                                                  recovery_log, device_recovery_error, service,
                                                  sync_direction)
                else:
                    recovery_log.append(f"\nNo failure found for {head_end} in service {service}")
