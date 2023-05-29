from traceback import print_exc

import ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic

from core_fp_common import cleanup_utils as CleanupUtils
from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common import recovery_utils
from cisco_ron_core_fp_common.status_codes import StatusCodes

from .zr_errors import CustomActionException
from . import utils as Utils
from .constants import StrConstants


class ZrDcoCleanupAction(ncs.dp.Action):
    """
    Action handler for ZR DCO services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(
                "Cleanup Action for internal service name=%s devices=%s "
                "no-networking=%s" % (input.service, input.devices, input.no_networking)
            )
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append("\n Cleaning up ZR DCO Internal Services: %s %s"
                               % (input.service, input.devices))
            is_no_networking = input.no_networking
            service = input.service
            devices = input.devices

            try:
                self._cleanup_zr_dco_service(uinfo, service, devices, is_no_networking,
                                             cleanup_log, root, th)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                    .set_context("Cleanup Action", "Cleanup failed for")
                    .add_state("Name", service)
                    .add_state("devices", devices)
                    .finish()
                )
                self.log.error(
                    "Cleanup Failed for Internal ZS DCO Services : {}".format(exp)
                )
                cleanup_log.append("\n Cleanup Failed for Internal ZR DCO Services\n\n")
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                # return the log
                self.log.info("Cleanup Successful for ZR DCO Internal Services")
                cleanup_log.append("\n Cleanup Successful for ZR DCO Internal Services")
                output.success = True
                output.detail = "".join(cleanup_log)

    def _cleanup_zr_dco_service(
        self, uinfo, service, devices, is_no_networking, cleanup_log, root, th
    ):
        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        remove_plan_paths = []

        internal_zr_dco_plan = root.cisco_zr_cfp__zr.dco_plan

        for device in devices:
            # Delete service if exists
            service_kp = "/cisco-zr-cfp:zr/dco{{{} {}}}".format(service, device)
            CleanupUtils.delete_service(self, service_kp, cleanup_log, uinfo)

            if (service, device) in internal_zr_dco_plan:
                internal_zr_dco_components = internal_zr_dco_plan[service, device].plan.component

                internal_zombie_path = "/zr/dco[name='%s'][router='%s']" % (service, device)
                internal_service_path = "/cisco-zr-cfp:zr/dco{%s %s}" % (service, device)
                internal_zombie_paths.append(internal_zombie_path)
                internal_service_paths.append(internal_service_path)
                remove_plan_paths.append(internal_zr_dco_plan[service, device]._path)

                for component in internal_zr_dco_components:
                    internal_force_back_track_comp.append(
                        (component.force_back_track, internal_zombie_path))

        self._cleanup_service_data(uinfo, service, is_no_networking, cleanup_log,
                                   internal_force_back_track_comp, internal_zombie_paths,
                                   internal_service_paths, remove_plan_paths)

    def _cleanup_service_data(
        self,
        uinfo,
        service,
        is_no_networking,
        cleanup_log,
        internal_force_back_track_comp,
        internal_zombie_paths,
        internal_service_paths,
        remove_plan_paths,
    ):

        ## force-backtrack all lower router services
        CleanupUtils.invoke_backtrack_actions(
            self, service, internal_force_back_track_comp, is_no_networking
        )

        cleanup_log.append("\n Removed all internal plan components")

        ## remove zombies for lower services
        for internal_zombie_path in internal_zombie_paths:
            CleanupUtils.remove_zombie(self, internal_zombie_path, cleanup_log, uinfo)

        ## delete lower services if exists
        for internal_service_path in internal_service_paths:
            CleanupUtils.delete_service(self, internal_service_path, cleanup_log, uinfo)

        ## remove all side-effects for lower service
        for internal_zombie_path in internal_zombie_paths:
            side_effects = CleanupUtils.get_all_side_effect_for_service(
                self, internal_zombie_path
            )
            self.log.info("side_effects for service: {}".format(side_effects))
            CleanupUtils.remove_service_side_effects(
                self, side_effects, cleanup_log, uinfo
            )

        ## Remove lingering kickers for inner services
        for internal_zombie_path in internal_zombie_paths:
            kickers = CleanupUtils.get_kicker_with_service(self, internal_zombie_path)
            self.log.info("kickers for internal service: {}".format(kickers))
            CleanupUtils.remove_kickers(self, kickers, cleanup_log, uinfo)

        self.log.info("remove_plan_paths list {}".format(remove_plan_paths))
        ## Remove leftover plan paths
        # This can happen when forcebacktrack has failed if no-networking false is requested
        # on failing device during cleanup
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)


class ZrDcoRecovery(ncs.dp.Action):
    """
    Action handler for ZR DCO services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info("Recovery Action for internal service=%s" % (input.service))
            root = maagic.get_root(th)
            recovery_log = []
            device_recovery_error = {}
            service = input.service
            device = input.router
            sync_direction = input.sync_direction
            try:
                self._recover_zr_dco_service(uinfo, service, device, recovery_log,
                                             device_recovery_error, root, th, sync_direction)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                    .set_context("Recovery Action")
                    .add_state("Name", service)
                    .finish()
                )
                self.log.error("Recovery Failed for ZR DCO Service : {}".format(exp))
                recovery_log.append("\nRecovery Failed for Internal ZR DCO Service\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            if len(device_recovery_error) > 0:
                # return the log
                self.log.info("Recovery Incomplete for ZR DCO Service")
                recovery_log.append(str(device_recovery_error))
                recovery_log.append("\nRecovery Incomplete for ZR DCO Service")
                output.success = False
                output.detail = "".join(recovery_log)
            else:
                self.log.info("Recovery Complete for ZR DCO Service")
                recovery_log.append("\nRecovery Complete for ZR DCOService")
                output.success = True
                output.detail = "".join(recovery_log)

    def _recover_zr_dco_service(self, uinfo, service, device, recovery_log, device_recovery_error,
                                root, th, sync_direction):

        dco_plan = root.cisco_zr_cfp__zr.dco_plan
        zombie_path = StrConstants.zr_service_xpath.format(service, device)
        service_path = StrConstants.zr_service_path.format(service, device)

        skey = (service, device)
        if skey in dco_plan:
            dco_plan_inst = dco_plan[skey].plan
            if dco_plan_inst.failed and dco_plan_inst.error_info:
                recovery_utils.recover_service(self, root, th, uinfo, dco_plan_inst,
                                               device, zombie_path, service_path, recovery_log,
                                               device_recovery_error, service, sync_direction)
            else:
                recovery_log.append(
                    "\nNo failure found for {} in service {}".format(device, service)
                )


class ZrDcoFetchOpticsData(ncs.dp.Action):

    """
    Action handler for ZR DCO Fetch Optics Data
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ENTRY_POINT for ZR fetch optics data callback: {kp}")
        self.log.info(AsciiArt.ninja_star)
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            device = input.end_point_device
            port = input.line_port
            bandwidth = input.bandwidth
            mode = input.mode
            dac_rate = input.dac_rate

            try:
                # Will always be one node for inner service
                router = Utils.get_device_impl_default_class(root, None, device, self.log)

                pids = router.get_pid_info(device, port, mode, bandwidth, dac_rate)
                # (Applicable - True/False, Valid - True/False, Pid value, error)
                capability = pids[0]
                flexport = pids[1]
                platform_pid = pids[2]

                output.status = "success"
                output.transceiver_pid = capability[2]
                output.flexport_linecard_pid = flexport[2]
                output.platform_pid = platform_pid[2]
                # If no capbability on the device, error out.
                if not capability[1]:
                    self.log.error(
                        "Matching Hardware capability not found on the device {}".format(
                            device
                        )
                    )
                    output.status = "failed"
                    output.message = "Error: " + capability[3]
                # If flexport port is not allowed the device, error out.
                # Update the check for flexport[0]
                if flexport[0] and not flexport[1]:
                    self.log.error(
                        "Hardware flex-port port is not allowed on device {}".format(
                            device
                        )
                    )
                    output.status = "failed"
                    output.message = "Error: " + flexport[3]

                if platform_pid[0] and not platform_pid[1]:
                    self.log.error(
                        f"Bandwidth is not supported on device with PID {platform_pid[2]}")
                    output.status = "failed"
                    output.message = "Error: " + platform_pid[3]

            except Exception as e:
                self.log.error(AsciiArt.roadblock)
                self.log.error("Exception during fetch optics " + str(e))
                exp = (
                    CustomActionException(self.log, StatusCodes.FETCH_OPTICS_ERROR)
                    .set_context("Fetch Optics Error", "Fetch Optics Failed")
                    .finish()
                )
                self.log.error(exp)
                output.status = "failed"
                output.message = f"Error: {exp.statusCode.code} Fetch Optics Failed"

        self.log.info(AsciiArt.ninja_star)
        self.log.info(f"EXIT_POINT for ZR fetch optics data callback: {kp}")


class ZrDcoZombieRedeploySync(ncs.dp.Action):

    """
    Action handler for ZR DCO Fetch Optics Data
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ENTRY_POINT for ZR Zombie Redploy and sync: {kp}")
        self.log.info(AsciiArt.ninja_star)
        self.log.info(
            "ZR Zombie Redploy and sync for internal service name=%s devices=%s "
            % (input.service, input.device)
        )
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            service = input.service
            end_point = input.device
            zr_service_xpath = "/zr/dco[name='{}'][router='{}']"

            try:
                service_path = zr_service_xpath.format(service, end_point)
                zombie = zr_service_xpath.format(service_path)
                if th.exists(zombie):
                    ncs.maagic.get_node(th, zombie).reactive_re_deploy()
                    self.log.info(
                        f"triggered zombie reactive_re_deploy on : {zombie} zombie"
                    )
                else:
                    self.log.info(f"Skipped zombie reactive_re_deploy on : {zombie}")
                # do sync-from
                dev = root.ncs__devices.device[end_point]
                state = dev.state.oper_state
                if state == "enabled" or state == "unknown":
                    self.log.info(f"Issuing sync-from on device {end_point}")
                    result = dev.sync_from()
                    self.log.info(f"sync-from result {result.result}")
                status = True
                message = "Zombie cleanup and sync successful"
            except Exception as e:
                print_exc()
                exp = (
                    CustomActionException(
                        self.log, StatusCodes.ZOMBIE_CLEAN_SYNC_ERROR, str(e)
                    )
                    .set_context(
                        "Zombie Clean and Sync Error", "Zombie Clean and Synce Failed"
                    )
                    .finish()
                )
                status = False
                message = "Error: " + str(exp)

            output.success = status
            output.detail = message
