import re
import traceback

import ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic

from .ron_ml_errors import CustomActionException
from . import utils as Utils
from .constants import StrConstants

from cisco_ron_core_fp_common.constants import AsciiArt
from core_fp_common import cleanup_utils as CleanupUtils
from core_fp_common import common_utils
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from cisco_ron_core_fp_common.utils import (
    set_service_cleanup_flag,
    delete_service_cleanup_flag,
    check_service_cleanup_flag,
)
from cisco_ron_core_fp_common.status_codes import StatusCodes


class ValidateServiceInput(Action):
    """
    Action handler for Validating Service Input
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            service = maagic.get_node(th, kp)
            self.log.info(f"ENTRY_POINT for RON ML validation callback {service.name}")
            self.log.info(AsciiArt.ninja_star)
            try:
                self._fetch_validate_sip(root, service)
                self._fetch_validate_zr(root, service)
            except Exception:
                self.log.error(AsciiArt.roadblock)
                raise
            self.log.info(AsciiArt.ninja_star)
            self.log.info(f"EXIT_POINT for RON ML validation callback {service.name}")

    def _fetch_validate_zr(self, root, service):

        for end_point in service.end_point:
            if end_point.terminal_device_packet.exists():

                if Utils.is_lsa(root):
                    rfs_device = Utils.safe_get_remote_nso(self.log, end_point.end_point_device)
                    fetch_optics_action = root.ncs__devices.device[
                        rfs_device
                    ].config.cisco_zr_cfp__zr.fetch_optics_data
                else:
                    # Invoke zr fetch optics locally
                    fetch_optics_action = root.cisco_zr_cfp__zr.fetch_optics_data

                fetch_optics_action_input = fetch_optics_action.get_input()
                fetch_optics_action_input.end_point_device = str(
                    end_point.end_point_device
                )
                fetch_optics_action_input.line_port = str(
                    end_point.terminal_device_optical.line_port
                )
                fetch_optics_action_input.mode = str(service.mode)
                fetch_optics_action_input.bandwidth = str(service.bandwidth)
                if service.dac_rate:
                    fetch_optics_action_input.dac_rate = service.dac_rate
                fetch_optics_action_output = fetch_optics_action(
                    fetch_optics_action_input
                )

                ## Check if internal action output failed/success
                ## If success populate the operational data
                if fetch_optics_action_output.status == "success":
                    self.log.debug("Retreived optics data successfully")
                    l_user = common_utils.get_local_user()
                    with ncs.maapi.single_write_trans(l_user, "system", db=ncs.OPERATIONAL) as th:
                        oproot = maagic.get_root(th)
                        if (end_point.end_point_device
                                in oproot.cisco_ron_cfp__ron.ron_ml[service.name].
                                ron_data.end_point):

                            end_point_data = oproot.cisco_ron_cfp__ron.ron_ml[service.name]. \
                                ron_data.end_point[end_point.end_point_device]
                        else:
                            end_point_data = oproot.cisco_ron_cfp__ron.ron_ml[service.name]. \
                                ron_data.end_point.create(end_point.end_point_device)

                        end_point_data.transceiver_pid = fetch_optics_action_output.transceiver_pid

                        if fetch_optics_action_output.flexport_linecard_pid is not None:
                            end_point_data.flexport_linecard_pid = \
                                fetch_optics_action_output.flexport_linecard_pid
                        if fetch_optics_action_output.platform_pid:
                            end_point_data.platform_pid = \
                                fetch_optics_action_output.platform_pid

                        try:
                            th.apply()
                            self.log.debug("Finished saving optics data for"
                                           f" end-point {end_point.end_point_device}")
                        except Exception as e:
                            self.log.error("Exception in while commiting optics data " + str(e))
                            raise
                else:
                    ## If failed, return with error.
                    self.log.error(fetch_optics_action_output.message)
                    raise Exception(fetch_optics_action_output.message)

    def _fetch_validate_sip(self, root, service):
        if service.ols_domain.exists():

            sip_lookup_keys = []
            optical_controller = None
            for end_point in service.end_point:
                managed_link = root.cisco_ron_cfp__ron.inter_layer_link[
                    end_point.end_point_device,
                    end_point.terminal_device_optical.line_port,
                ]

                optical_controller = root.cisco_ron_cfp__ron.default_optical_controller
                if managed_link.ols_domain.optical_controller is not None:
                    optical_controller = managed_link.ols_domain.optical_controller

                if managed_link.ols_domain.optical_service_interface == "sip":
                    sip_lookup_keys.append(managed_link.ols_domain.optical_sip)
                else:
                    network_elem = str(managed_link.ols_domain.network_element)
                    add_drop = str(managed_link.ols_domain.optical_add_drop)
                    sip_lookup_keys.append(network_elem + ":" + add_drop)

            if Utils.is_lsa(root):
                rfs_device = Utils.safe_get_remote_nso(self.log, optical_controller)
                fetch_sip_action = root.ncs__devices.device[
                    rfs_device
                ].config.tapi_common__context.tapi_connectivity__connectivity_context.fetch_sip
            else:
                # Invoke zr fetch optics locally
                fetch_sip_action = (
                    root.tapi_common__context.tapi_connectivity__connectivity_context.fetch_sip
                )

            if len(sip_lookup_keys) > 0:
                fetch_sip_action_input = fetch_sip_action.get_input()
                fetch_sip_action_input.optical_controller = str(optical_controller)
                fetch_sip_action_input.ids = sip_lookup_keys
                fetch_sip_action_output = fetch_sip_action(fetch_sip_action_input)

                ## Check if internal action output failed/success
                ## If success populate the operational data
                if fetch_sip_action_output.status == "success":
                    self.log.debug("Retreived sip data successfully")
                    siplist = fetch_sip_action_output.sips
                    sips = []
                    for sip in siplist:
                        sips.append(sip)
                    l_user = common_utils.get_local_user()
                    with ncs.maapi.single_write_trans(
                        l_user, "system", db=ncs.OPERATIONAL
                    ) as th:
                        oproot = maagic.get_root(th)
                        index = 0
                        for end_point in service.end_point:
                            # How are we mapping SPI vs Endpoint ?
                            # Is ordering preserved in the round trip ?
                            if (
                                end_point.end_point_device
                                in oproot.cisco_ron_cfp__ron.ron_ml[
                                    service.name
                                ].ron_data.end_point
                            ):
                                end_point_data = oproot.cisco_ron_cfp__ron.ron_ml[
                                    service.name
                                ].ron_data.end_point[end_point.end_point_device]
                            else:
                                end_point_data = oproot.cisco_ron_cfp__ron.ron_ml[
                                    service.name
                                ].ron_data.end_point.create(end_point.end_point_device)
                            end_point_data.sip = sips[index]
                            index = index + 1

                        try:
                            th.apply()
                            self.log.debug("Finished saving sip data")
                        except Exception as e:
                            self.log.error(
                                "Exception in while commiting sip data " + str(e)
                            )
                            raise
                else:
                    # If failed, return with error.
                    self.log.error(fetch_sip_action_output.message)
                    raise Exception(fetch_sip_action_output.message)


class RonCleanupAction(ncs.dp.Action):
    """
    Action handler for RON ML services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(
                "Cleanup Action for RON ML service name=%s "
                "no-networking=%s" % (input.service, input.no_networking)
            )
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append("Cleaning up RON Service: %s" % (input.service))
            is_no_networking = input.no_networking
            service = input.service
            service_kp = "/cisco-ron-cfp:ron/ron-ml{{{}}}".format(service)
            try:
                set_service_cleanup_flag(self, service_kp, uinfo.username)
                self._cleanup_ron_service(uinfo, service, is_no_networking, cleanup_log, root, th)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                    .set_context("Cleanup Action", "Cleanup failed for")
                    .add_state("Name", service)
                    .finish()
                )
                self.log.error("Cleanup Failed for RON Service : {}".format(exp))
                cleanup_log.append("\n Cleanup Failed for RON Services\n\n")
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                # return the log
                self.log.info("Cleanup Successful for RON ML Service")
                cleanup_log.append("\n Cleanup Successful for RON ML Service")
                output.success = True
                output.detail = "".join(cleanup_log)
            finally:
                delete_service_cleanup_flag(self, service_kp, uinfo.username)

    def _cleanup_ron_service(self, uinfo, service, is_no_networking, cleanup_log, root, th):
        force_back_track_comp = []
        # recovery_paths = []
        remove_plan_paths = []
        # failed_device_path = root.cisco_tsdn_core_fp_common__commit_queue_recovery_data.\
        #                                                                        failed_device
        ron_plan_path = "/cisco-ron-cfp:ron/cisco-ron-cfp:ron-ml-plan{%s}" % service

        if Utils.is_lsa(root):
            self.log.info("LSA Scenario")

        zombie_service_path = "/ron/ron-ml[name='%s']" % service

        # Delete service if exists
        service_kp = "/cisco-ron-cfp:ron/ron-ml{{{}}}".format(service)
        CleanupUtils.delete_service(self, service_kp, cleanup_log, uinfo)

        ## Collect service data to be cleaned up
        end_points = []
        has_optical = False
        if th.exists(ron_plan_path):
            ron_plan_components = root.cisco_ron_cfp__ron.ron_ml_plan[
                service
            ].plan.component

            for component in ron_plan_components:
                if component.type == "cisco-ron-cfp-nano-plan-services:router":
                    end_points.append(component.name)
                if component.type == "cisco-ron-cfp-nano-plan-services:optical-controller":
                    has_optical = True
                force_back_track_comp.append((component.force_back_track, zombie_service_path))
            remove_plan_paths.append(ron_plan_path)

        self.log.info("End points are {}".format(end_points))
        self.log.info("Has optical is {}".format(has_optical))
        self._cleanup_service_data(
            uinfo, service, end_points, has_optical, is_no_networking, cleanup_log,
            force_back_track_comp, zombie_service_path, root, remove_plan_paths,
        )

    def _cleanup_service_data(self, uinfo, service, end_points, has_optical, is_no_networking,
                              cleanup_log, force_back_track_comp, zombie_service_path,
                              root, remove_plan_paths):

        if len(end_points) > 0:
            ## Check if internal service is local or on RFS node
            internal_service = service + "-internal"
            if Utils.is_lsa(root):
                ## Get RFS Node to device mapping
                rfs_node_dev_dict = LsaUtils.get_device_remote_nso(end_points)
                self.log.info("rfs_node_dev_dict: {}".format(rfs_node_dev_dict))
                ## Invoke internal cleanup action on all RFS nodes with corresponding devices
                for rfs_node, rfs_node_devices in rfs_node_dev_dict.items():
                    internal_action = root.ncs__devices.device[
                        rfs_node
                    ].config.cisco_zr_cfp__zr.cleanup
                    self._call_internal_cleanup_action(
                        internal_action,
                        internal_service,
                        rfs_node_devices,
                        is_no_networking,
                        cleanup_log,
                    )
            else:
                ## Invoke internal cleanup action locally
                internal_action = root.cisco_zr_cfp__zr.cleanup
                self._call_internal_cleanup_action(
                    internal_action,
                    internal_service,
                    end_points,
                    is_no_networking,
                    cleanup_log,
                )
        cs_uuid = ""
        if has_optical:
            cs_uuid = root.cisco_ron_cfp__ron.uuid_lookup[service].uuid
            if Utils.is_lsa(root):
                ## Get RFS Node to device mapping
                if cs_uuid in root.cisco_ron_cfp__ron.ron_service_lookup:
                    device = root.cisco_ron_cfp__ron.ron_service_lookup[
                        cs_uuid
                    ].optical_controller
                    rfs_node = Utils.safe_get_remote_nso(self.log, device)
                    internal_action = root.ncs__devices.device[
                        rfs_node
                    ].config.tapi_common__context.tapi_connectivity__connectivity_context.cleanup
                    self._call_internal_cleanup_action(
                        internal_action, cs_uuid, [], is_no_networking, cleanup_log
                    )
                else:
                    cleanup_log.append(
                        "\n Skipping Optical as cs_uuid not found in service_lookup"
                    )
            else:
                ## Invoke internal cleanup action locally
                internal_action = (
                    root.tapi_common__context.tapi_connectivity__connectivity_context.cleanup
                )
                self._call_internal_cleanup_action(internal_action, cs_uuid,
                                                   [], is_no_networking, cleanup_log)

        ## if LSA, then no-networking for CFS part of the cleanup should always be true
        if Utils.is_lsa(root):
            is_no_networking = True

        ## force-backtrack all upper service components
        CleanupUtils.invoke_backtrack_actions(
            self, service, force_back_track_comp, is_no_networking
        )
        cleanup_log.append("\n Removed all ron plan components")

        ## remove zombies for upper service
        CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        ## remove all side-effects for upper service
        side_effects = CleanupUtils.get_all_side_effect_for_service(
            self, zombie_service_path
        )
        self.log.info("side_effects for service: {}".format(side_effects))
        CleanupUtils.remove_service_side_effects(self, side_effects, cleanup_log, uinfo)

        kickers = CleanupUtils.get_kicker_with_service(self, zombie_service_path)
        self.log.info("kickers for ron service: {}".format(kickers))
        ## Remove all lingering kickers for outer service
        CleanupUtils.remove_kickers(self, kickers, cleanup_log, uinfo)

        self.log.info("remove_plan_paths list {}".format(remove_plan_paths))
        ## Remove leftover plan paths
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)
        ## remove zombies for upper service again
        ## This is to cover the case where cleanup is requested without service delete.
        ## In this case we will see race conditions where zombie is recreated
        ## after we pass the zombie deletion check during removal of config-apply callbacks
        CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        ## Due to zombie removal race conditions
        ## plan elements are partially recreated causing cleanup leftovers.
        ## This does a final check on plan path removal.
        for remove_plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo)

        if cs_uuid != "" and cs_uuid in root.cisco_ron_cfp__ron.ron_service_lookup:
            self.log.info("Cleaning up {} in ron_service_lookup table".format(cs_uuid))
            with maapi.single_write_trans(
                uinfo.username, "system", db=ncs.OPERATIONAL
            ) as thw:
                ron_slookup_path = (
                    "/cisco-ron-cfp:ron/cisco-ron-cfp:ron-service-lookup{%s}" % cs_uuid
                )
                if thw.exists(ron_slookup_path):
                    self.log.info("removing service lookup path: %s" % ron_slookup_path)
                    cleanup_log.append(
                        "\n Removing service lookup path: %s" % ron_slookup_path
                    )
                    try:
                        thw.delete(ron_slookup_path)
                        thw.apply()
                    except KeyError:
                        pass
                    cleanup_log.append("\n Removed service lookup path")
        if service in root.cisco_ron_cfp__ron.uuid_lookup:
            self.log.info("Cleaning up {} in uuid_lookup table".format(service))
            with maapi.single_write_trans(
                uinfo.username, "system", db=ncs.OPERATIONAL
            ) as thw:
                ron_uuid_path = (
                    "/cisco-ron-cfp:ron/cisco-ron-cfp:uuid-lookup{%s}" % service
                )
                if thw.exists(ron_uuid_path):
                    self.log.info("removing uuid lookup path: %s" % ron_uuid_path)
                    cleanup_log.append(
                        "\n Removing uuid lookup path: %s" % ron_uuid_path
                    )
                    try:
                        thw.delete(ron_uuid_path)
                        thw.apply()
                    except KeyError:
                        pass
                    cleanup_log.append("\n Removed uuid lookup path")
        ## Check for leftover inter-layer-link-oper
        if has_optical and len(end_points) > 0:
            inter_layer_link_oper = root.cisco_ron_cfp__ron.inter_layer_link_oper
            paths = []
            for link in inter_layer_link_oper:
                if link.end_point_device in end_points and link.ron_ml_service == service:
                    self.log.debug(f"will remove {link._path}")
                    paths.append(link._path)
            for path in paths:
                with maapi.single_write_trans(
                    uinfo.username, "system", db=ncs.OPERATIONAL
                ) as thw:
                    self.log.info(f"removing inter-layer-link-oper path: {path}")
                    try:
                        thw.delete(path)
                        thw.apply()
                    except KeyError:
                        pass
            cleanup_log.append("\n Removed inter-layer-link-oper")

    def _call_internal_cleanup_action(
        self, internal_action, service, devices, is_no_networking, cleanup_log
    ):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        if len(devices) > 0:
            internal_action_input.devices = devices
        internal_action_input.no_networking = is_no_networking
        internal_action_output = internal_action(internal_action_input)

        ## Check if internal action output failed/success
        ## If success proceed with northbound service cleanup
        if internal_action_output.success:
            cleanup_log.append(internal_action_output.detail)
        else:
            ## If failed, return with error.
            cleanup_log.append(internal_action_output.detail)
            raise Exception("Internal Service Cleanup Failed")


class MLRedeployKickerCallback(ncs.dp.Action):
    """
    RON-ML ron-redeploy kicker callback
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ron-ml redeploy kicker for : {input.path}")
        self.log.info(f"ron-ml redeploy kicker to : {kp}")
        """
            The trigger path can be
            /cisco-zr-cfp:zr/cisco-zr-cfp:dco-plan{%s %s} (or)
            /tapi-common:context/tapi-connectivity:connectivity-context/connectivity-service-plan{%s}
            This reg-ex will parse for the name followed by plan{ until }
        """
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            service_id = None
            if "cisco-zr-cfp" in input.path:
                match = re.search("{(.*?) (.*?)}", input.path)
                internal_service = match.group(1)
                service_id = internal_service.rsplit("-internal", 1)[0]
            else:
                match = re.search("plan{(.*?)}", input.path)
                cs_uuid = match.group(1)
                if cs_uuid in root.cisco_ron_cfp__ron.ron_service_lookup:
                    service_id = root.cisco_ron_cfp__ron.ron_service_lookup[
                        cs_uuid
                    ].ron_ml_service
                else:
                    self.log.info(
                        f"Cannot de-code {cs_uuid} back to ron-ml service. Skipping"
                    )
                    return

            try:
                self.log.info(f"ron ml service :{service_id}")
                service_kp = "/cisco-ron-cfp:ron/ron-ml{{{}}}".format(service_id)
                if not check_service_cleanup_flag(self, service_kp, uinfo.username):
                    self.redeploy_service(service_id)
                else:
                    self.log.info("Ignoring re-deploy as service-cleanup-flag is set")
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                err = (
                    CustomActionException(
                        self.log, StatusCodes.INTERNAL_REDEPLOY_ACTION_ERROR, str(exp)
                    )
                    .set_context(
                        "ron-ml internal-redeploy",
                        "Failed to redeploy the parent ron-ml service",
                    )
                    .add_state("service", service_id)
                    .finish()
                )
                self.log.info(err)
            self.log.info(f"Done processing redeploy kicker for: {input.path}")

    def redeploy_service(self, service_id):
        l_user = common_utils.get_local_user()
        with ncs.maapi.single_read_trans(
            l_user, StrConstants.system, db=ncs.RUNNING
        ) as th:
            rpath = "/cisco-ron-cfp:ron"
            ron = ncs.maagic.get_node(th, rpath)
            if service_id in ron.ron_ml:
                ron.ron_ml[service_id].reactive_re_deploy()
                self.log.info(
                    f"triggered reactive_re_deploy on : {rpath}/ron-ml{{{service_id}}}"
                )
            else:
                service_path = StrConstants.ron_service_xpath.format(service_id)
                zombie = StrConstants.zombie_service.format(service_path)
                self.log.info(f"triggering reactive_re_deploy on : {zombie} zombie")
                if th.exists(zombie):
                    ncs.maagic.get_node(th, zombie).reactive_re_deploy()
                    self.log.info(f"triggered reactive_re_deploy on : {zombie} zombie")


class RonFailureHandle(ncs.dp.Action):
    """
    RON-ML Failure handler to trigger Optical Fallback if needed
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ENTRY_POINT for RON ML rollback handle callback {input.path}")
        self.log.info(AsciiArt.ninja_star)
        """
            trigger path will be /cisco-ron-cfp:ron/ron-ml-plan{%s}
        """
        match = re.search("-plan{(.*?)}", input.path)
        service_id = match.group(1)
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            self.log.debug(f"ron ml service :{service_id}")
            ron = ncs.maagic.get_node(th, kp)
            # if optical is ready then only check for zr failures else ignore
            if service_id in ron.ron_ml_plan and self._is_optical_ready(root, service_id):
                plan = ron.ron_ml_plan[service_id].plan
                (zr_failed, failed_zrs) = self._is_zr_failure(root, service_id, plan)
                if (
                    zr_failed
                    and not root.ron.ron_ml[service_id].ron_data.do_rollback.exists()
                ):
                    try:
                        if len(failed_zrs) > 0:
                            # CQ flow
                            self.log.info("Failed ZRs case")
                            for failed_zr in failed_zrs:
                                self._force_backtrack(th, service_id, failed_zr)
                    except Exception as exp:
                        self.log.error(AsciiArt.roadblock)
                        self.log.error(traceback.format_exc())
                        err = (
                            CustomActionException(
                                self.log,
                                StatusCodes.PROCESS_FAILURE_ACTION_ERROR,
                                str(exp),
                            )
                            .set_context(
                                "ron-ml process-failure",
                                "Failed while processing ron ml service",
                            )
                            .add_state("service", service_id)
                            .finish()
                        )
                        self.log.info(err)

                    with ncs.maapi.single_write_trans(
                        uinfo.username, "system", db=ncs.OPERATIONAL
                    ) as th1:
                        oproot = maagic.get_root(th1)
                        oproot.ron.ron_ml[service_id].ron_data.do_rollback.create()
                        try:
                            th1.apply()
                        except Exception as e:
                            self.log.error(AsciiArt.roadblock)
                            raise CustomActionException(
                                self.log,
                                StatusCodes.PROCESS_FAILURE_ACTION_ERROR,
                                str(e),
                            ).set_context(
                                "ron-ml process-failure action",
                                "Failed while writing zr-failed for ron ml service",
                            ).add_state(
                                "service", service_id
                            ).finish()
                    self._redeploy_service(kp, service_id)
                    for failed_zr in failed_zrs:
                        self._zombie_clean_sync(th, service_id, failed_zr)
                        # self._redeploy_zombie(service_id, failed_zr)
                        # self._device_sync_from(failed_zr)
                elif (
                    zr_failed
                    and len(failed_zrs) > 0
                    and root.ron.ron_ml[service_id].ron_data.do_rollback.exists()
                ):
                    # This case is to invoke backtrack in RFS ZR service
                    # only when CQ is enabled.
                    for failed_zr in failed_zrs:
                        if self._is_lsa_with_cq_enabled(root, failed_zr, service_id):
                            try:
                                self._force_backtrack(th, service_id, failed_zr)
                                self.log.debug(f"Backtrack explicitly invoked for {failed_zr} in"
                                               " LSA mode with CQ")
                            except Exception as exp:
                                self.log.error(AsciiArt.roadblock)
                                self.log.error(traceback.format_exc())
                                err = (
                                    CustomActionException(
                                        self.log,
                                        StatusCodes.PROCESS_FAILURE_ACTION_ERROR,
                                        str(exp),
                                    )
                                    .set_context(
                                        "ron-ml process-failure",
                                        "Failed while processing ron ml service",
                                    )
                                    .add_state("service", service_id)
                                    .finish()
                                )
                                self.log.info(err)
        self.log.info(AsciiArt.ninja_star)
        self.log.info(f"EXIT_POINT for RON ML rollback handle callback {input.path}")

    def _is_lsa_with_cq_enabled(self, root, failed_zr, service_id):
        if Utils.is_lsa(root):
            rfs_device = Utils.safe_get_remote_nso(self.log, failed_zr)
            if (
                root.ncs__devices.global_settings.commit_queue.enabled_by_default
                or root.ncs__devices.device[rfs_device].commit_queue.enabled_by_default
                or len(root.ron.ron_ml[service_id].commit_queue.queue_item) > 0
            ):
                return True
        return False

    def _is_zr_failure(self, root, service_id, plan):
        zr_count = 0
        failed_zrs = []
        for plan_component in plan.component:
            if plan_component.type == StrConstants.com_router:
                zr_count = zr_count + 1
                if plan_component.state[StrConstants.ncs_ready].status == "failed":
                    failed_zrs.append(plan_component.name)
        if zr_count > 0 and (len(failed_zrs) > 0 or plan.failed):
            # Non-CQ case zr_failed_count may be 0 so return based on plan.failed
            # CQ case zr_failed_count will be  > 0
            self.log.debug("ZR Failed scenario")
            return (True, failed_zrs)
        return (False, failed_zrs)

    def _is_optical_ready(self, root, service_id):
        if Utils.is_lsa(root):
            cs_plan = (
                root.cisco_ron_cfp__ron.remote_local_data.connectivity_service_plan
            )
        else:
            cs_plan = (
                root.context.tapi_connectivity__connectivity_context.connectivity_service_plan
            )
        try:
            cs_uuid = root.cisco_ron_cfp__ron.uuid_lookup[service_id].uuid
            if cs_uuid in cs_plan:
                plan = cs_plan[cs_uuid].plan
                self_key = ("ncs:self", "self")
                if plan.component[self_key].state["ncs:ready"].status == "reached":
                    self.log.info(f"Optical service {cs_uuid} is ready-reached")
                    return True
                else:
                    self.log.debug(f"optical service {cs_uuid} is not ready-reached")
        except KeyError:
            return False
        return False

    def _force_backtrack(self, th, service_id, failed_zr):
        zr_dco_service = service_id + "-internal"
        root = maagic.get_root(th)
        if Utils.is_lsa(root):
            rfs_device = Utils.safe_get_remote_nso(self.log, failed_zr)
            zr_plan = (
                root.devices.device[rfs_device]
                .live_status.cisco_zr_cfp__zr.dco_plan[(zr_dco_service, failed_zr)]
                .plan.component[(StrConstants.ncs_self, StrConstants.self_cn)]
            )
        else:
            zr_plan = ncs.maagic.get_node(
                th, StrConstants.zr_plan_path.format(zr_dco_service, failed_zr)
            )
        zr_force_back_track_action = zr_plan.force_back_track
        zr_force_back_track_action_input = zr_force_back_track_action.get_input()
        zr_force_back_track_action_input.no_networking.create()
        zr_force_back_track_action_input.back_tracking_goal = StrConstants.ncs_init
        zr_force_back_track_action_output = zr_force_back_track_action(
            zr_force_back_track_action_input
        )

        self.log.info(
            f"ZR forceback track result is {zr_force_back_track_action_output.result}"
        )

    def _redeploy_service(self, kp, service_id):
        l_user = common_utils.get_local_user()
        with ncs.maapi.single_read_trans(
            l_user, StrConstants.system, db=ncs.RUNNING
        ) as th:
            ron = ncs.maagic.get_node(th, kp)
            if service_id in ron.ron_ml:
                action = ron.ron_ml[service_id].reactive_re_deploy
                action_input = action.get_input()
                action_input.sync.create()
                action(action_input)
                self.log.info(
                    f"triggered reactive_re_deploy on : {kp}/ron-ml{{{service_id}}}"
                )
            else:
                self.log.info(
                    f"Skipped re-deploy as ron service with id {service_id} not found"
                )

    def _zombie_clean_sync(self, th, service_id, failed_zr):
        zr_dco_service = service_id + "-internal"
        root = maagic.get_root(th)
        if Utils.is_lsa(root):
            rfs_device = Utils.safe_get_remote_nso(self.log, failed_zr)
            zombie_clean_sync_action = root.ncs__devices.device[
                rfs_device
            ].config.cisco_zr_cfp__zr.zombie_clean_sync
        else:
            zombie_clean_sync_action = root.cisco_zr_cfp__zr.zombie_clean_sync

        zombie_clean_sync_action_input = zombie_clean_sync_action.get_input()
        zombie_clean_sync_action_input.service = zr_dco_service
        zombie_clean_sync_action_input.device = failed_zr
        zombie_clean_sync_action_output = zombie_clean_sync_action(
            zombie_clean_sync_action_input
        )
        self.log.info(
            f"ZR zombie-clean--sync result is {zombie_clean_sync_action_output.success}"
        )


class ErrorRecovery(ncs.dp.Action):
    """
    RON-ML error recovery action callback handler
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(
                f"Recovery Action for service={input.service} device={input.device}"
            )
            root = maagic.get_root(th)
            recovery_log = []
            recovery_log.append("Recovering RON-ML service: {}".format(input.service))
            service = input.service
            device = input.device
            sync_direction = input.sync_direction
            try:
                ml_plan = root.cisco_ron_cfp__ron.ron_ml_plan
                if service not in ml_plan:
                    raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                f"No such service found for recovery: {service} ") \
                        .set_context("Recovery Action") \
                        .add_state("Name", service) \
                        .add_state("Device", device).finish()

                ml_plan_inst = ml_plan[service].plan
                if device:
                    ned_id = root.core_fp_common__dispatch_map[device].ned_id
                    # IOS-XR
                    if ned_id.startswith("cisco-iosxr-nc-"):
                        comp_key = (StrConstants.com_router, device)
                        if comp_key not in ml_plan_inst.component:
                            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                        f"device {device} doesn't belong to"
                                                        f" the given service {service} ") \
                                .set_context("Recovery Action") \
                                .add_state("Name", service) \
                                .add_state("Device", device).finish()

                        component = ml_plan_inst.component[comp_key]
                        if (component.state[StrConstants.ncs_ready].status != "failed"
                                and component.state[StrConstants.ncs_init].status != "failed"):
                            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                        f"Device: {device} is not in failed"
                                                        f" state in service {service} ") \
                                .set_context("Recovery Action") \
                                .add_state("Name", service) \
                                .add_state("Device", device).finish()

                        # Call the stack-service for recovery
                        self._call_zr_recovery_action(root, service, device,
                                                      recovery_log, sync_direction)
                    # ONC
                    elif ned_id.startswith("onf-tapi-nc-"):
                        comp_key = (StrConstants.com_optical_controller, StrConstants.optical_cn)
                        if comp_key not in ml_plan_inst.component:
                            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                        f"The service {service} doesn't have"
                                                        " optical-domain configured") \
                                .set_context("Recovery Action") \
                                .add_state("Name", service) \
                                .add_state("Device", device).finish()

                        component = ml_plan_inst.component[comp_key]
                        if (component.state[StrConstants.ncs_ready].status != "failed"
                                and component.state[StrConstants.ncs_init].status != "failed"):
                            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                        "The optical domain on the service"
                                                        f" {service} is not in failed state") \
                                .set_context("Recovery Action") \
                                .add_state("Name", service) \
                                .add_state("Device", device).finish()

                        cs_uuid = root.cisco_ron_cfp__ron.uuid_lookup[service].uuid
                        if ((cs_uuid not in root.cisco_ron_cfp__ron.ron_service_lookup) or (
                            device != root.cisco_ron_cfp__ron.ron_service_lookup[cs_uuid]
                                .optical_controller)):
                            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                        f"device {device} doesn't belong to"
                                                        f" the given service {service} ") \
                                .set_context("Recovery Action") \
                                .add_state("Name", service) \
                                .add_state("Device", device).finish()

                        # Call the stack-service for recovery
                        self._call_onc_recovery_action(root, service, device,
                                                       recovery_log, sync_direction)
                    else:
                        raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                    f"device {device} doesn't belong to the"
                                                    f" given service {service} ") \
                            .set_context("Recovery Action") \
                            .add_state("Name", service) \
                            .add_state("Device", device).finish()
                else:
                    fc = []
                    for component in ml_plan_inst.component:
                        if not (component.name == "self"
                                and component.type == StrConstants.ncs_self):
                            if (component.state[StrConstants.ncs_ready].status == "failed"
                                    or component.state[StrConstants.ncs_init].status == "failed"):
                                fc.append((component.name, component.type))

                    if not fc:
                        raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                    "No component has failed on the service"
                                                    f" {service} to initiate a recovery") \
                            .set_context("Recovery Action") \
                            .add_state("Name", service) \
                            .add_state("Device", device).finish()
                    # I know we can save the number of loops but indentation problem. will comeback
                    for component in fc:
                        if component[1] == StrConstants.com_optical_controller:
                            cs_uuid = root.cisco_ron_cfp__ron.uuid_lookup[service].uuid
                            if (cs_uuid not in root.cisco_ron_cfp__ron.ron_service_lookup):
                                raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                            "couldn't lookup optical device for"
                                                            f" {service}, please try recovery with"
                                                            " the device name") \
                                    .set_context("Recovery Action") \
                                    .add_state("Name", service) \
                                    .add_state("Device", device).finish()

                            device = root.cisco_ron_cfp__ron.ron_service_lookup[cs_uuid] \
                                .optical_controller
                            # Call the stack-service for recovery
                            self._call_onc_recovery_action(root, service, device,
                                                           recovery_log, sync_direction)
                        else:
                            device = component[0]
                            # Call the stack-service for recovery
                            self._call_zr_recovery_action(root, service, device,
                                                          recovery_log, sync_direction)
            except CustomActionException as exp:
                self.log.error("Recovery Failed : {}".format(exp))
                recovery_log.append("\nRecovery Failed\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                    .set_context("Recovery Action")
                    .add_state("Name", service)
                    .add_state("Device", device)
                    .finish()
                )
                self.log.error("Recovery Failed : {}".format(exp))
                recovery_log.append("\nRecovery Failed\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            else:
                self.log.info("Recovery Complete")
                recovery_log.append("\nRecovery Complete\n")
                output.success = True
                output.detail = "".join(recovery_log)

    def _call_zr_recovery_action(self, root, service, device, recovery_log, sync_direction):
        ss_action = None
        if Utils.is_lsa(root):
            rfs_node = Utils.safe_get_remote_nso(self.log, device)
            ss_action = root.ncs__devices.device[rfs_node].config.cisco_zr_cfp__zr.error_recovery
        else:
            ## Invoke internal recovery action locally
            ss_action = root.cisco_zr_cfp__zr.error_recovery

        action_input = ss_action.get_input()
        action_input.service = service + "-internal"
        action_input.router = device
        action_input.sync_direction = sync_direction
        action_output = ss_action(action_input)

        ## Check if internal action output failed/success
        if action_output.success:
            recovery_log.append(action_output.detail)
        else:
            ## If failed, return with error.
            recovery_log.append(action_output.detail)
            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                        f"ZR device {device} recovery has failed ") \
                .set_context("Recovery Action") \
                .add_state("Name", service) \
                .add_state("Device", device).finish()

    def _call_onc_recovery_action(self, root, service, device, recovery_log, sync_direction):
        ss_action = None
        if Utils.is_lsa(root):
            rfs_node = Utils.safe_get_remote_nso(self.log, device)
            ss_action = root.ncs__devices.device[
                rfs_node
            ].config.tapi_common__context.tapi_connectivity__connectivity_context.error_recovery
        else:
            ## Invoke internal recovery action locally
            ss_action = (
                root.tapi_common__context.tapi_connectivity__connectivity_context.error_recovery
            )

        cs_uuid = root.cisco_ron_cfp__ron.uuid_lookup[service].uuid
        action_input = ss_action.get_input()
        action_input.service = cs_uuid
        action_input.sync_direction = sync_direction
        action_output = ss_action(action_input)

        ## Check if internal action output failed/success
        if action_output.success:
            recovery_log.append(action_output.detail)
        else:
            ## If failed, return with error.
            recovery_log.append(action_output.detail)
            raise CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                        f"ONC device {device} recovery has failed ") \
                .set_context("Recovery Action") \
                .add_state("Name", service) \
                .add_state("Device", device).finish()


class ListAllSips(Action):
    """
    Action handler for List All Sips
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info("ENTRY_POINT for RON ML list all sip callback")
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            optical_controller = input.optical_controller
            try:
                if Utils.is_lsa(root):
                    rfs_device = Utils.safe_get_remote_nso(self.log, optical_controller)
                    list_sip_action = root.ncs__devices.device[
                        rfs_device
                    ].config.tapi_common__context.tapi_connectivity__connectivity_context.list_sip
                else:
                    list_sip_action = (
                        root.tapi_common__context.tapi_connectivity__connectivity_context.list_sip
                    )
            except AttributeError as e:
                self.log.error(AsciiArt.roadblock)
                self.log.error("AttributeError during List SIP " + str(e))
                output.status = "failed"
                output.message = f"Error: Device {optical_controller} doesn't have " \
                                 f"support to list all sips."
                return

            try:
                action_input = list_sip_action.get_input()
                action_input.optical_controller = optical_controller
                action_input.no_networking = input.no_networking
                action_output = list_sip_action(action_input)
                output.status = action_output.status
                output.message = action_output.message
                for entry in action_output.optical_service_interface:
                    sip = output.optical_service_interface.create(entry.optical_sip)
                    sip.optical_add_drop = entry.optical_add_drop
                    sip.network_element = entry.network_element
            except Exception as e:
                output.status = "failed"
                output.message = f"Error while trying to list all sips on {optical_controller}: " \
                                 f"{str(e)}"
        self.log.info("EXIT_POINT for RON ML list all sip callback")


class ValidateInterLayerLinks(Action):
    """
    Action handler for validating inter-layer-link
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info("ENTRY_POINT for RON ML validate sip callback")
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            inter_layer_link = root.cisco_ron_cfp__ron.inter_layer_link
            sip_dict = {}
            for entry in inter_layer_link:
                optical_controller = str(entry.ols_domain.optical_controller)
                if optical_controller not in sip_dict:
                    sip_dict[optical_controller] = []
                sip_dict[optical_controller].append((entry.end_point_device, entry.line_port))

            errors = ""
            all_valid = True
            for optical_controller in sip_dict.keys():
                try:
                    if Utils.is_lsa(root):
                        rfs_device = Utils.safe_get_remote_nso(self.log, optical_controller)
                        validate_sip_action = root.ncs__devices.device[
                            rfs_device
                        ].config.tapi_common__context\
                            .tapi_connectivity__connectivity_context.validate_sips
                    else:
                        validate_sip_action = (
                            root.tapi_common__context
                                .tapi_connectivity__connectivity_context.validate_sips
                        )
                except AttributeError as e:
                    self.log.error(AsciiArt.roadblock)
                    self.log.error("AttributeError during Validate SIP " + str(e))
                    errors = errors + f"\nDevice {optical_controller} doesn't have " \
                                      f"support to validate sips."
                    continue

                try:
                    action_input = validate_sip_action.get_input()
                    action_input.optical_controller = optical_controller
                    action_input.no_networking = input.no_networking
                    for index in range(len(sip_dict[optical_controller])):
                        sip = action_input.optical_service_interface.create(index)
                        ols_domain = \
                            inter_layer_link[sip_dict[optical_controller][index]].ols_domain
                        if ols_domain.optical_sip is None:
                            sip.network_element = ols_domain.network_element
                            sip.optical_add_drop = ols_domain.optical_add_drop
                        else:
                            sip.optical_sip = ols_domain.optical_sip
                    action_output = validate_sip_action(action_input)
                    if action_output.status == "failed":
                        errors = errors + f"\nFailed to validate sips on device " \
                                          f"{optical_controller}: " \
                                          f"{action_output.message}."
                except Exception as e:
                    errors = errors + f"\nCan't validate sips on device {optical_controller}" \
                                      f" due to error: {str(e)}."
                    continue
                for entry in action_output.optical_service_interface:
                    all_valid &= entry.valid
                    if not entry.valid:
                        link_key = sip_dict[optical_controller][entry.index]
                        link = output.inter_layer_link.create(link_key)
                        ols_domain = \
                            inter_layer_link[link_key].ols_domain
                        if ols_domain.optical_sip is None:
                            link.network_element = ols_domain.network_element
                            link.optical_add_drop = ols_domain.optical_add_drop
                        else:
                            link.optical_sip = ols_domain.optical_sip
                        link.valid = entry.valid

            if all_valid and not errors:
                output.status = "successful"
                output.message = "All inter-layer-link entries are valid."
            else:
                output.status = "failed"
                output.message = errors if errors else None
        self.log.info("EXIT_POINT for RON ML validate sip callback")
