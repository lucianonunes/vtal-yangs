import ncs
import _ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
import traceback
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import \
    CustomActionException
from cisco_tsdn_core_fp_common.utils import get_action_timeout
from core_fp_common import cleanup_utils as CleanupUtils
from cisco_tsdn_core_fp_common import recovery_utils as RecoveryUtils
from cisco_tsdn_core_fp_common import constants


class FlatL2vpnSelfTest(ncs.dp.Action):
    """
    Action handler for self-test
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running self test: {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
            root = ncs.maagic.get_root(th)

            # Get service node from action path
            action = ncs.maagic.get_node(th, kp)
            service = ncs.maagic.cd(action, "..")

            final_status = None
            final_message = None
            try:

                if "cisco-flat-L2vpn-fp-internal-local-site" in str(kp):
                    site_name = "local-site"
                    if service.service_type == "p2p":
                        site = service.flat_L2vpn_p2p.local_site
                    else:
                        site = service.flat_L2vpn_evpn_vpws.local_site
                else:
                    site_name = "remote-site"
                    if service.service_type == "p2p":
                        site = service.flat_L2vpn_p2p.remote_site
                    else:
                        site = service.flat_L2vpn_evpn_vpws.remote_site

                router = Utils.get_device_impl_default_class(
                    self, root, service, site.pe, root
                    .cisco_flat_L2vpn_fp_internal_common__cfp_configurations.dynamic_device_mapping
                )

                (final_status, final_message) = router.l2vpn_self_test(root, service, site.pe,
                                                                       site.xconnect_group_name,
                                                                       site.p2p_name)
                self.log.info(f"{site_name} L2vpn test results: {final_status} {final_message}")
            except Exception as e:
                traceback.print_exc()
                exp = CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e)).\
                    set_context("Self Test", "Self Test Failed").finish()

                final_status = "failed"
                final_message = "Error: " + str(exp)
            output.status = final_status
            output.message = final_message


class FlatL2vpnCleanupAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))

        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            cleanup_log = []
            service_name = input.service
            root = maagic.get_root(th)
            sites = get_site_types_and_site_names(input)

            for site_type, site_name in sites:
                cleanup_log.append(f"\n Cleaning up L2vpn internal {site_type} "
                                   f"service: {service_name} {site_name} ")

                try:
                    self.log.info(f"Cleanup Action for L2vpn internal service name={name} "
                                  f"service={service_name} "
                                  f"local_site_only={input.local_site_only} "
                                  f"remote_site_only={input.remote_site_only} "
                                  f"no-networking={input.no_networking}")

                    is_no_networking = input.no_networking

                    self._cleanup_internal_l2vpn_service(uinfo, service_name, site_type, site_name,
                                                         is_no_networking, cleanup_log, root)

                    self.log.info("Cleanup Successful for L2vpn internal service")
                    cleanup_log.append(f"\n Cleanup Successful for L2vpn internal {site_type} "
                                       f"service: {service_name} {site_name} \n")
                    output.success = True
                    output.detail = "".join(cleanup_log)

                except Exception as ex:
                    traceback.print_exc()
                    self.log.error(f"Exception in FlatL2vpnCleanupAction: {ex} ")
                    exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(ex))
                           .set_context("Cleanup Action",
                                        f"Cleanup failed for L2vpn internal {site_type} "
                                        f"service {input.service} {site_name}").finish())
                    output.success = False
                    cleanup_log.append(f"\nERROR: {exp}")
                    output.detail = "".join(cleanup_log)

    def _cleanup_internal_l2vpn_service(self, uinfo, service_name, site_type, site_name,
                                        is_no_networking, cleanup_log, root):

        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        # TODO: Fix needed to retrieve path for
        # impacted-service-path: core-fp-delete-tag-service:core-fp-delete-shutdown-service
        cq_error_recovery_paths = []
        remove_plan_path = None

        # Internal local-site data
        (internal_service_path, internal_zombie_path,
            internal_plan) = get_internal_paths(site_type, service_name, site_name, root)

        if internal_plan is not None:
            internal_l2vpn_components = internal_plan.plan.component
            for component in internal_l2vpn_components:
                # Only self component in internal service
                internal_force_back_track_comp.append(
                    (component.force_back_track, internal_zombie_path))
            remove_plan_path = internal_plan._path

        internal_zombie_paths.append(internal_zombie_path)
        internal_service_paths.append(internal_service_path)
        cq_error_recovery_paths.extend(
            RecoveryUtils.get_cq_error_recovery_paths(self, root, site_name,
                                                      internal_service_path,
                                                      internal_zombie_path)
        )

        self._cleanup_service_data(
            uinfo,
            service_name,
            is_no_networking,
            cleanup_log,
            internal_force_back_track_comp,
            internal_zombie_paths,
            internal_service_paths,
            cq_error_recovery_paths,
            remove_plan_path
        )

    def _cleanup_service_data(self, uinfo, service_name, is_no_networking, cleanup_log,
                              internal_force_back_track_comp, internal_zombie_paths,
                              internal_service_paths, cq_error_recovery_paths,
                              remove_plan_path):

        self.log.info("L2VPN service cleanup data: "
                      f"internal_force_back_track_comp:{internal_force_back_track_comp}, "
                      f"internal_zombie_paths:{internal_zombie_paths}, "
                      f"cq_error_recovery_paths:{cq_error_recovery_paths} ")

        # force-backtrack all internal services - Not working with NSO 5.7 - RT Filed
        CleanupUtils.invoke_backtrack_actions(self, service_name, internal_force_back_track_comp,
                                              is_no_networking)
        cleanup_log.append("\n Removed all plan components")

        # remove zombies for lower services
        for internal_zombie_path in internal_zombie_paths:
            CleanupUtils.remove_zombie(self, internal_zombie_path, cleanup_log, uinfo)

        # delete lower services if exists
        for internal_service_path in internal_service_paths:
            CleanupUtils.delete_service(self, internal_service_path, cleanup_log, uinfo)

        # Remove any commit-queue error recovery poller paths if exists
        for (device, cq_error_recovery_path) in cq_error_recovery_paths:
            RecoveryUtils.remove_cq_recovery_data(self, cq_error_recovery_path, device,
                                                  cleanup_log, uinfo)
        cleanup_log.append("\n Removed commit-queue-recovery-data")

        # Remove leftover plan paths
        # This can happen when forcebacktrack has failed if no-networking false is requested
        # on failing device during cleanup
        CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)


class FlatL2vpnRecoveryAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services site error recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"L2vpn Recovery action on RFS : {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            recovery_log = []
            device_recovery_error = {}
            service_name = input.service
            sync_direction = input.sync_direction

            sites = get_site_types_and_site_names(input)

            for site_type, site_name in sites:
                self.log.info(f"L2vpn Recovery action for internal {site_type} : "
                              f"{service_name} {site_name} {sync_direction}")
                recovery_log.append(f"\nL2VPN Recovery action for internal {site_type} : "
                                    f"{service_name} {site_name} {sync_direction}")

                try:
                    (service_path, zombie_path, internal_plan) = \
                        get_internal_paths(site_type, service_name, site_name, root)

                    # For local and remote sites, site-name is the pe-name
                    pe_name = site_name
                    if site_type == constants.SITE:
                        pe_name = maagic.get_node(th, internal_plan).pe

                    if (internal_plan.plan.failed and internal_plan.plan.error_info) \
                            or root.ncs__zombies.service.exists(zombie_path):
                        RecoveryUtils.recover_service(
                            self, root, th, uinfo, internal_plan.plan,
                            pe_name, zombie_path, service_path, recovery_log,
                            device_recovery_error, service_name, sync_direction
                        )
                    else:
                        recovery_log.append(
                            f"\nNo failure found for {site_type} : {service_name} {pe_name}"
                        )

                except Exception as e:
                    traceback.print_exc()
                    self.log.error(f"Recovery Failed for Internal L2VPN Service : {e}")
                    err_msg = f"Recovery failed for L2vpn internal {site_type} " \
                        f"service {service_name} {pe_name}"

                    exp = CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e)) \
                        .set_context("Recovery Action", err_msg).finish()

                    recovery_log.append("\nRecovery Failed for Internal L2VPN Service\n\n")
                    output.success = False
                    recovery_log.append(str(exp))
                    output.detail = "".join(recovery_log)
                else:
                    if len(device_recovery_error) > 0:
                        self.log.error("device_recovery_error for Internal "
                                       f"L2VPN Service : {str(device_recovery_error)}")
                        recovery_log.append("\nWARNING: ")
                        recovery_log.append(str(device_recovery_error))
                        recovery_log.append("\n\nRecovery Incomplete for "
                                            f"L2VPN Internal Service for device {pe_name} \n")
                        output.success = False
                        output.detail = "".join(recovery_log)
                    else:
                        self.log.info("Recovery Complete for L2VPN Internal Service")
                        recovery_log.append("\n\nRecovery Complete for L2VPN Internal Service for "
                                            f"device {pe_name} \n")
                        output.success = True
                        output.detail = "".join(recovery_log)


def get_internal_paths(site_type, service, site_name, root):
    internal_service_path = None
    internal_zombie_path = None
    internal_site_plan_path = None

    if site_type == constants.LOCAL_SITE:
        internal_zombie_path = (f"/flat-L2vpn-internal-local-site-service[name='{service}']"
                                f"[pe='{site_name}']")
        internal_service_path = ("/cisco-flat-L2vpn-fp-internal-local-site:"
                                 f"flat-L2vpn-internal-local-site-service{{{service} {site_name}}}")
        internal_site_plan_path = root.\
            cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_internal_local_site.\
            flat_L2vpn_plan
    elif site_type == constants.REMOTE_SITE:
        internal_zombie_path = (f"/flat-L2vpn-internal-remote-site-service[name='{service}']"
                                f"[pe='{site_name}']")
        internal_service_path = ("/cisco-flat-L2vpn-fp-internal-remote-site:"
                                 f"flat-L2vpn-internal-remote-site-service{{{service} {site_name}}}"
                                 )
        internal_site_plan_path = root.\
            cisco_flat_L2vpn_fp_internal_remote_site__flat_L2vpn_internal_remote_site.\
            flat_L2vpn_plan
    elif site_type == constants.SITE:
        internal_zombie_path = (f"/flat-L2vpn-internal-site-service[name='{service}']"
                                f"[site-name='{site_name}']")
        internal_service_path = ("/cisco-flat-L2vpn-fp-internal-site:"
                                 f"flat-L2vpn-internal-site-service{{{service} {site_name}}}")
        internal_site_plan_path = root.\
            cisco_flat_L2vpn_fp_internal_site__flat_L2vpn_internal_site.\
            flat_L2vpn_plan

    internal_site_plan = None
    if (service, site_name) in internal_site_plan_path:
        internal_site_plan = internal_site_plan_path[service, site_name]

    return internal_service_path, internal_zombie_path, internal_site_plan


def get_site_types_and_site_names(input):
    sites = []
    if input.local_site_only is not None:
        sites.append((constants.LOCAL_SITE, input.local_site_only))
    if input.remote_site_only is not None:
        sites.append((constants.REMOTE_SITE, input.remote_site_only))
    if input.site_only is not None:
        sites.append((constants.SITE, input.site_only))
    return sites
