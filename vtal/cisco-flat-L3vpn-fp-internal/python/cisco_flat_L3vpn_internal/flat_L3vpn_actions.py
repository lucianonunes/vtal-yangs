from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import \
    CustomActionException
from core_fp_common.cleanup_utils import invoke_backtrack_actions, remove_zombie, delete_service
from .utils import get_device_impl_default_class, external_service_model_exists
import _ncs
from ncs.dp import Action
from ncs import maagic, maapi, OPERATIONAL, RUNNING
from cisco_tsdn_core_fp_common import recovery_utils as RecoveryUtils
from core_fp_common import cleanup_utils as CleanupUtils
from cisco_tsdn_core_fp_common.utils import get_action_timeout


class FlatL3vpnInternalSelfTest(Action):
    """
    Action handler for self-test
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running self test: {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system") as th:
            # Setup
            root = maagic.get_root(th)
            action = maagic.get_node(th, kp)
            service = maagic.cd(action, "..")
            endpoint = service.endpoint[service.endpoint_name]
            device = endpoint.access_pe
            vrf_name = endpoint.vrf.vrf_definition
            # Get device class
            router = get_device_impl_default_class(
                self, root, service, device,
                root.cisco_flat_L3vpn_fp_internal__cfp_configurations.dynamic_device_mapping,
            )
            # Get ip
            src_ip = input.src_ip
            dst_ip = input.dst_ip
            # Run test
            (output.status, output.message) = router.l3vpn_self_test(root, service, vrf_name,
                                                                     device, src_ip, dst_ip)


class FlatL3vpnCleanupAction(Action):
    """
    Action handler for L3VPN services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=OPERATIONAL) as th:
            self.log.info(f"Cleanup Action for internal service name={name} "
                          f"service={input.service} devices={list(input.endpoints)} "
                          f"no-networking={input.no_networking}")
            root = maagic.get_root(th)
            cleanup_log = []
            is_no_networking = input.no_networking
            service = input.service
            endpoints = input.endpoints

            cleanup_log.append(f"Cleaning up L3VPN Internal Services: {input.service}")

            try:
                # Cleanup L3VPN service
                self._cleanup_l3vpn_service(uinfo, service, endpoints, is_no_networking,
                                            cleanup_log, root, th)
            except Exception as e:
                err_msg = "Cleanup Failed for Internal L3VPN Services"
                exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                       .set_context("Cleanup Action", err_msg).add_state("Service Name", service)
                       .finish())
                self.log.error(f"{err_msg} : {exp}")
                cleanup_log.append(f"\n {err_msg} \n")
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                success_msg = "Cleanup Successful for L3VPN Internal Services"
                # return the log
                self.log.info(success_msg)
                cleanup_log.append(f"\n {success_msg} \n")
                output.success = True
                output.detail = "".join(cleanup_log)

    def _cleanup_l3vpn_service(self, uinfo, service, endpoints, is_no_networking,
                               cleanup_log, root, th):
        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        cq_error_recovery_paths = []
        remove_plan_paths = []
        failed_device_path = root.\
            cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device

        for endpoint in endpoints:
            internal_l3vpn_plan = root.\
                cisco_flat_L3vpn_fp_internal__flat_L3vpn_internal.flat_L3vpn_plan
            device = None

            internal_zombie_path = f"/flat-L3vpn[name='{service}'][endpoint-name='{endpoint}']"

            if external_service_model_exists(root):
                internal_zombie_path = ("/cisco-flat-L3vpn-fp-internal:"
                                        f"flat-L3vpn[name='{service}'][endpoint-name='{endpoint}']")

            internal_service_path = ("/cisco-flat-L3vpn-fp-internal:"
                                     f"flat-L3vpn{{{service} {endpoint}}}")

            if (service, endpoint) in internal_l3vpn_plan:
                internal_l3vpn_components = internal_l3vpn_plan[service, endpoint].plan.component
                for component in internal_l3vpn_components:
                    # Only self component in internal service
                    internal_force_back_track_comp.append(
                        (component.force_back_track, internal_zombie_path))
                    device = component.private.property_list.property["ACCESS_PE"].value
                remove_plan_paths.append(internal_l3vpn_plan[service, endpoint]._path)

            internal_zombie_paths.append(internal_zombie_path)
            internal_service_paths.append(internal_service_path)
            if device and device in failed_device_path:
                cq_err_serv_path = (
                    "/cisco-tsdn-core-fp-common:commit-queue-recovery-data/failed-"
                    f"device{{{device}}}/"
                    f'impacted-service-path{{"{internal_service_path}"}}')

                zombie_path = f"/ncs:zombies/service{{{internal_zombie_path}}}"
                cq_err_zombie_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data"
                                      f"/failed-device{{{device}}}/"
                                      f'impacted-service-path{{"{zombie_path}"}}')

                cq_error_recovery_paths.append((device, cq_err_serv_path))
                cq_error_recovery_paths.append((device, cq_err_zombie_path))

        self._cleanup_service_data(uinfo, service, is_no_networking, cleanup_log,
                                   internal_force_back_track_comp, internal_zombie_paths,
                                   internal_service_paths, cq_error_recovery_paths,
                                   remove_plan_paths)

    def _cleanup_service_data(self, uinfo, service, is_no_networking, cleanup_log,
                              internal_force_back_track_comp, internal_zombie_paths,
                              internal_service_paths, cq_error_recovery_paths,
                              remove_plan_paths):
        # force-backtrack all lower endpoint services - RT#48440 Filed
        invoke_backtrack_actions(self, service, internal_force_back_track_comp,
                                 is_no_networking)
        cleanup_log.append("\n Removed all internal plan components")

        # remove zombies for lower services
        for internal_zombie_path in internal_zombie_paths:
            remove_zombie(self, internal_zombie_path, cleanup_log, uinfo)

        # delete lower services if exists
        for internal_service_path in internal_service_paths:
            delete_service(self, internal_service_path, cleanup_log, uinfo)

        # Remove any commit-queue error recovery poller paths if exists
        for (device, cq_error_recovery_path) in cq_error_recovery_paths:
            RecoveryUtils.remove_cq_recovery_data(self, cq_error_recovery_path,
                                                  device, cleanup_log, uinfo)
        cleanup_log.append("\n Removed commit-queue-recovery-data")

        self.log.info(f"remove_plan_paths list {remove_plan_paths}")
        # Remove leftover plan paths
        # This can happen when forcebacktrack has failed if no-networking false is requested
        # on failing device during cleanup
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)


class FlatL3vpnServiceRecoveryAction(Action):
    """
    Action handler for L3VPN services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=RUNNING) as th:
            self.log.info(f"Recovery Action for internal service service={input.service} "
                          f"endpoints={input.endpoints}")
            root = maagic.get_root(th)
            recovery_log = []
            device_recovery_error = {}
            service = input.service
            endpoints = input.endpoints
            sync_direction = input.sync_direction

            try:
                self._recover_l3vpn_service(uinfo, service, endpoints, recovery_log,
                                            device_recovery_error, root, th, sync_direction)
            except Exception as e:
                exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                       .set_context("Recovery Action", "Recovery failed for L3vpn")
                       .add_state("L3vpn Name", service).finish())
                self.log.error(f"Recovery Failed for Internal L3VPN Services : {exp}")
                recovery_log.append("\nRecovery Failed for Internal L3VPN Services\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            else:
                if len(device_recovery_error) > 0:
                    # return the log
                    self.log.info("Recovery Incomplete for L3VPN Internal Services")
                    recovery_log.append("\nWARNING: ")
                    recovery_log.append(str(device_recovery_error))
                    recovery_log.append("\nRecovery Incomplete for L3VPN Internal Services")
                    output.success = False
                    output.detail = "".join(recovery_log)
                else:
                    self.log.info("Recovery Complete for L3VPN Internal Services")
                    recovery_log.append("\nRecovery Complete for L3VPN Internal Services")
                    output.success = True
                    output.detail = "".join(recovery_log)

    def _recover_l3vpn_service(self, uinfo, service, endpoints, recovery_log,
                               device_recovery_error, root, th, sync_direction):
        for endpoint in endpoints:
            internal_l3vpn_plan = root.\
                cisco_flat_L3vpn_fp_internal__flat_L3vpn_internal.flat_L3vpn_plan

            internal_zombie_path = (f"/flat-L3vpn[name='{service}'][endpoint-name='{endpoint}']")
            if external_service_model_exists(root):
                internal_zombie_path = ("/cisco-flat-L3vpn-fp-internal:flat-L3vpn"
                                        f"[name='{service}'][endpoint-name='{endpoint}']")
            internal_service_path = (f"/cisco-flat-L3vpn-fp-internal:flat-L3vpn{{{service} "
                                     f"{endpoint}}}")
            if (service, endpoint) in internal_l3vpn_plan:
                l3vpn_service_plan = internal_l3vpn_plan[service, endpoint].plan
                # There will always be only one component.
                for component in l3vpn_service_plan.component:
                    device_name = component.private.property_list.property[
                        "ACCESS_PE"
                    ].value
                    if l3vpn_service_plan.failed and l3vpn_service_plan.error_info:
                        RecoveryUtils.recover_service(self, root, th, uinfo, l3vpn_service_plan,
                                                      device_name, internal_zombie_path,
                                                      internal_service_path, recovery_log,
                                                      device_recovery_error, service,
                                                      sync_direction)
                    else:
                        recovery_log.append(f"\nNo failure found for {endpoint} "
                                            f"in service {service}")
