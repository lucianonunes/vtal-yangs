import re
import traceback

import ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
from core_fp_common import cleanup_utils as CleanupUtils
from core_fp_common import common_utils
from .optical_errors import (
    CustomActionException,
    UserError,
    ServiceException,
)
from . import utils as Utils
from .constants import StrConstants as str_con
from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common import recovery_utils
from cisco_ron_core_fp_common.status_codes import StatusCodes


class OpticalConnectivityCleanupAction(ncs.dp.Action):
    """
    Action handler for Connectivity Service services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            self.log.info(
                "Cleanup Action for internal service name=%s service=%s "
                "no-networking=%s" % (name, input.service, input.no_networking)
            )
            root = maagic.get_root(th)
            cleanup_log = []
            cleanup_log.append(
                "\n Cleaning up Connectivity Service Internal Services: %s" % input.service
            )
            is_no_networking = input.no_networking
            service = input.service

            try:
                self._cleanup_connectivity_service(uinfo, service, is_no_networking,
                                                   cleanup_log, root, th)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                    .set_context("Cleanup Action", "Cleanup failed for")
                    .add_state("Name", service)
                    .finish()
                )
                self.log.error(
                    "Cleanup Failed for Internal Connectivity Service Services : {}".format(
                        exp
                    )
                )
                cleanup_log.append(
                    "\n Cleanup Failed for Internal Connectivity "
                    "Service Services\n\n"
                )
                output.success = False
                cleanup_log.append(str(exp))
                output.detail = "".join(cleanup_log)
            else:
                # return the log
                self.log.info(
                    "Cleanup Successful for Connectivity Service Internal Services"
                )
                cleanup_log.append(
                    "\n Cleanup Successful for Connectivity Service "
                    "Internal Services"
                )
                output.success = True
                output.detail = "".join(cleanup_log)

    def _cleanup_connectivity_service(self, uinfo, service, is_no_networking,
                                      cleanup_log, root, th):
        internal_force_back_track_comp = []
        internal_zombie_paths = []
        internal_service_paths = []
        remove_plan_paths = []

        internal_connectivity_service_plan = (
            root.tapi_common__context.tapi_connectivity__connectivity_context.
            connectivity_service_plan
        )

        # Delete service if exists
        service_kp = (
            "/tapi-common:context/tapi-connectivity:connectivity-context/"
            "connectivity-service{{{}}}".format(service)
        )
        CleanupUtils.delete_service(self, service_kp, cleanup_log, uinfo)

        if service in internal_connectivity_service_plan:
            internal_connectivity_service_components = (
                internal_connectivity_service_plan[service].plan.component
            )

            internal_zombie_path = (
                "/context/tapi-connectivity:connectivity-context/connectivity-service[uuid='%s']"
                % (service)
            )
            internal_service_path = (
                "/tapi-common:context/tapi-connectivity:connectivity-context/"
                "connectivity-service{%s}" % (service)
            )
            internal_zombie_paths.append(internal_zombie_path)
            internal_service_paths.append(internal_service_path)
            remove_plan_paths.append(internal_connectivity_service_plan[service]._path)

            for component in internal_connectivity_service_components:
                internal_force_back_track_comp.append(
                    (component.force_back_track, internal_zombie_path))

        self._cleanup_service_data(uinfo, service, is_no_networking, cleanup_log,
                                   internal_force_back_track_comp, internal_zombie_paths,
                                   internal_service_paths, remove_plan_paths)
        if (root.tapi_common__context.tapi_connectivity__connectivity_context
                .tapi_connectivity__connectivity_service_oper_data.exists(service)):
            purge_action = (root.tapi_common__context.tapi_connectivity__connectivity_context
                                .tapi_connectivity__connectivity_service_oper_data[service].purge)
            purge_action_output = purge_action()
            cleanup_log.append(f"\n {purge_action_output.result}")

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

        ## force-backtrack all lower controller services
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


class OpticalConnectivityRecovery(ncs.dp.Action):
    """
    Action handler for Connectivity Service services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info("Recovery Action for internal service=%s" % (input.service))
            root = maagic.get_root(th)
            recovery_log = []
            device_recovery_error = {}
            service = input.service
            sync_direction = input.sync_direction
            try:
                self._recover_connectivity_service(uinfo, service, recovery_log,
                                                   device_recovery_error, root, th, sync_direction)
            except Exception as e:
                exp = CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e)) \
                    .set_context("Recovery Action") \
                    .add_state("Name", service) \
                    .finish()
                self.log.error("Recovery Failed for Connectivity Service : {}".format(exp))
                recovery_log.append("\nRecovery Failed for Internal Connectivity Service\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            if len(device_recovery_error) > 0:
                # return the log
                self.log.info("Recovery Incomplete for Connectivity Service")
                recovery_log.append(str(device_recovery_error))
                recovery_log.append("\nRecovery Incomplete for Connectivity Service")
                output.success = False
                output.detail = "".join(recovery_log)
            else:
                self.log.info("Recovery Complete for Connectivity Service")
                recovery_log.append("\nRecovery Complete for Connectivity Service")
                output.success = True
                output.detail = "".join(recovery_log)

    def _recover_connectivity_service(self, uinfo, service, recovery_log, device_recovery_error,
                                      root, th, sync_direction):

        plan = (root.tapi_common__context.tapi_connectivity__connectivity_context.
                connectivity_service_plan)
        zombie_path = str_con.cs_zombie_path.format(service)
        service_path = str_con.cs_service_path.format(service)

        if service in plan:
            cs_plan = plan[service].plan
            comp = (str_con.ncs_self, "self")
            device = cs_plan.component[comp].private.property_list.property["CONTROLLER"].value
            if cs_plan.failed and cs_plan.error_info:
                recovery_utils.recover_service(self, root, th, uinfo, cs_plan, device, zombie_path,
                                               service_path, recovery_log, device_recovery_error,
                                               service, sync_direction)
            else:
                recovery_log.append(
                    "\nNo failure found for {} in service {}".format(device, service)
                )


class NotificationHandler(ncs.dp.Action):
    """
    Action handler for get_operational data
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(AsciiArt.coffee_break)
        self.log.info(f"Notification callback {input.path}")
        self.handle_notification(uinfo, name, kp, input, output)
        self.log.info(AsciiArt.coffee_break)

    def handle_notification(self, uinfo, name, kp, input, output):
        l_user = common_utils.get_local_user()

        match = re.search("device{(.*)}/netconf-notifications", input.path)
        onc_controller = match.group(1)

        with ncs.maapi.single_read_trans(l_user, str_con.system) as th:
            ro_root = ncs.maagic.get_root(th)
            controller = Utils.get_device_impl_default_class(ro_root, None,
                                                             onc_controller, self.log)
            controller.set_connectivity_service_oper_data(uinfo, name, kp, input, output)


class RedeployKickerCallback(ncs.dp.Action):
    """
    connectivity-service redeploy kicker callback
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"connectivity-service redeploy kicker for : {input.path}")
        self.log.info(f"connectivity-service redeploy kicker to : {kp}")
        """
            The trigger path can be
            /tapi-common:context/tapi-connectivity:connectivity-context/
            connectivity-service-oper-data{%s}/lifecycle-state{%s %s}
            This reg-ex will parse for the name followed by cconnectivity-service-oper-data{ until }
        """
        match = re.search("connectivity-service-oper-data{(.*?)}", input.path)
        service_uuid = match.group(1)

        try:
            self.log.info(f"connectivity service :{service_uuid}")
            self.redeploy_service(kp, service_uuid)
        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            err = (
                CustomActionException(
                    self.log, StatusCodes.INTERNAL_REDEPLOY_ACTION_ERROR, str(exp)
                )
                .set_context(
                    "optical internal-redeploy",
                    "Failed to redeploy the connectivity service",
                )
                .add_state("service", service_uuid)
                .finish()
            )
            self.log.info(err)
        self.log.info(f"Done processing redeploy kicker for: {input.path}")

    def redeploy_service(self, kp, service_id):
        l_user = common_utils.get_local_user()
        with ncs.maapi.single_read_trans(l_user, str_con.system, db=ncs.RUNNING) as th:
            context = ncs.maagic.get_node(th, kp)
            if service_id in context.connectivity_service:
                context.connectivity_service[service_id].reactive_re_deploy()
                self.log.info(
                    f"triggered reactive_re_deploy on : {kp}/"
                    f"connectivity-service{{{service_id}}}"
                )
            else:
                service_path = str_con.cs_zombie_path.format(service_id)
                zombie = str_con.zombie_service.format(service_path)
                self.log.info(f"triggering reactive_re_deploy on : {zombie} zombie")
                if th.exists(zombie):
                    ncs.maagic.get_node(th, zombie).reactive_re_deploy()
                    self.log.info(f"triggered reactive_re_deploy on : {zombie} zombie")


class OpticalFetchSip(ncs.dp.Action):
    """
    Action handler for getting SIP data
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ENTRY_POINT for Optical fetch sip validation callback {kp}")
        self.log.info(AsciiArt.ninja_star)

        with ncs.maapi.single_write_trans(uinfo.username, "system") as th:

            root = maagic.get_root(th)
            optical_controller = input.optical_controller
            sip_lookup_keys = input.ids

            try:
                controller = Utils.get_device_impl_default_class(
                    root, None, optical_controller, self.log
                )
            except Exception as ex:
                self.log.error("Exception while getting dynamic class " + str(ex))
                exp = (
                    ServiceException(self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND)
                    .set_context(
                        "Dynamic Device Mapping",
                        "Error retrieving Dynamic Device class",
                    )
                    .add_state("Device", optical_controller)
                    .finish()
                )
                output.status = "failed"
                output.message = "Error: " + str(exp)
            try:
                controller.validate_onc_notif_sub(root, self.log, optical_controller)
                sips = controller.fetch_sip(
                    root, uinfo.username, optical_controller, sip_lookup_keys
                )
                if len(sips) == 0 or sips[0] is None or sips[1] is None:
                    self.log.info("SIP retreival failed")
                    output.status = "failed"
                    output.message = "Failed to retrieve/validate SIPs"
                else:
                    self.log.debug("SIP retreival successful " + str(sips))
                    output.status = "success"
                    output.sips = sips
                    self.log.info("output status is " + output.status)
                    self.log.info("output sips are " + str(output.sips))
            except UserError as e:
                self.log.error(AsciiArt.roadblock)
                self.log.error("Exception during validation for ONC Notifications")
                self.log.error(e)
                output.status = "failed"
                output.message = f"Error: {e.statusCode.code} {str(e.statusCode.reason)}."\
                                 f"{str(e.value)}."
            except AttributeError as e:
                self.log.error(AsciiArt.roadblock)
                self.log.error("AttributeError during Fetch SIP " + str(e))
                exp = (
                    UserError(self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND)
                    .set_context(
                        "Dynamic Method Error",
                        "get_connectivity_service_oper_data method is "
                        "missing from multi-vendor python class",
                    )
                    .add_state("Controller", optical_controller)
                    .finish()
                )
                self.log.error(exp)
                output.status = "failed"
                output.message = f"Error: {exp.statusCode.code} Dynamic Method Error; "\
                                 "get_connectivity_service_oper_data method is missing from "\
                                 "multi-vendor python class"
            except Exception:
                self.log.error(AsciiArt.roadblock)
                self.log.error("Exception during fetch optics ")
                self.log.error(traceback.format_exc())
                exp = (
                    CustomActionException(self.log, StatusCodes.FETCH_SIP_ERROR)
                    .set_context("Fetch SIP Error", "Fetch SIP Failed")
                    .finish()
                )
                output.status = "failed"
                output.message = f"Error: {exp.statusCode.code} Fetch SIP Failed"

        self.log.info(AsciiArt.ninja_star)
        self.log.info(f"EXIT_POINT for Optical fetch sip validation callback {kp}")


class Purge(Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_write_trans(
            uinfo.username, uinfo.context, db=ncs.OPERATIONAL
        ) as th:
            th.safe_delete(str(kp))
            th.apply()
        output.result = "Deleted {}".format(str(kp))


class Subscription(Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with maapi.single_write_trans(
            uinfo.username, str_con.system, db=ncs.RUNNING
        ) as th:
            root = ncs.maagic.get_root(th)
            device = input.onc
            interval = input.reconnect_interval
            try:
                hidden_srvc = (
                    root.tapi_common__context.tapi_connectivity__connectivity_context.
                    settings.netconf_subscription
                )

                if input.unsubscribe:
                    if device in hidden_srvc:
                        del hidden_srvc[device]
                        th.apply()
                        output.success = True
                        output.detail = "unsubscribed successfully"
                    else:
                        output.success = False
                        output.detail = "No device subscriptions found"
                else:
                    subscription = hidden_srvc.create(device)
                    if interval:
                        subscription.reconnect_interval = interval
                    th.apply()
                    output.success = True
                    output.detail = (
                        f"subscription created, monitor devices/device{{{device}}}/"
                        f"netconf-notifications/subscription{{{device}-nc-sub}}"
                        f" for subscription status"
                    )

            except Exception as e:
                exp = (
                    CustomActionException(
                        self.log, StatusCodes.SUBSCRIPTION_ERROR, str(e)
                    )
                    .set_context("Subscription Action")
                    .add_state("Name", device)
                    .finish()
                )
                self.log.error(exp)
                output.success = False
                output.detail = str(exp)


class OpticalListSip(ncs.dp.Action):
    """
    Action handler for listing SIP data
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ENTRY_POINT for Optical list sip callback {kp}")
        self.log.info(AsciiArt.ninja_star)
        optical_controller = input.optical_controller

        with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            try:
                controller = Utils.get_device_impl_default_class(
                    root, None, optical_controller, self.log
                )
            except Exception as ex:
                self.log.error("Exception while getting dynamic class " + str(ex))
                exp = (
                    ServiceException(self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND)
                    .set_context(
                        "Dynamic Device Mapping",
                        "Error retrieving Dynamic Device class",
                    )
                    .add_state("Device", optical_controller)
                    .finish()
                )
                output.status = "failed"
                output.message = "Error: " + str(exp)
                return
            try:
                if not input.no_networking:
                    sync_result = \
                        controller.partial_sync_optical_service_interface(optical_controller, root)
                    if not sync_result[0]:
                        output.status = "failed"
                        output.message = "Error: " + sync_result[1]
                        return
                sips = controller.list_sips(optical_controller, root)

            except AttributeError as e:
                self.log.error(AsciiArt.roadblock)
                self.log.error("AttributeError during List SIP " + str(e))
                exp = (
                    UserError(self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND)
                    .set_context(
                        "Dynamic Method Error",
                        "partial_sync_optical_service_interface "
                        "or list_sips method is "
                        "missing from multi-vendor python class",
                    )
                    .add_state("Controller", optical_controller)
                    .finish()
                )
                self.log.error(exp)
                output.status = "failed"
                output.message = f"Error: {exp.statusCode.code} Dynamic Method Error; "\
                                 "partial_sync_optical_service_interface or " \
                                 "list_sips method is missing from "\
                                 "multi-vendor python class"
                return

            for entry in sips:
                sip = output.optical_service_interface.create(entry[0])
                sip.network_element = entry[1]
                sip.optical_add_drop = entry[2]
            output.status = "successful"
        self.log.info(AsciiArt.ninja_star)
        self.log.info(f"EXIT_POINT for Optical fetch sip validation callback {kp}")


class OpticalValidateSip(ncs.dp.Action):
    """
    Action handler for validating SIP data
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ENTRY_POINT for Optical validate sip callback {kp}")
        self.log.info(AsciiArt.ninja_star)
        optical_controller = input.optical_controller

        with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)
            try:
                controller = Utils.get_device_impl_default_class(
                    root, None, optical_controller, self.log
                )
            except Exception as ex:
                self.log.error("Exception while getting dynamic class " + str(ex))
                exp = (
                    ServiceException(self.log, StatusCodes.DYNAMIC_CLASS_NOT_FOUND)
                    .set_context(
                        "Dynamic Device Mapping",
                        "Error retrieving Dynamic Device class",
                    )
                    .add_state("Device", optical_controller)
                    .finish()
                )
                output.status = "failed"
                output.message = "Error: " + str(exp)
                return
            try:
                if not input.no_networking:
                    sync_result = \
                        controller.partial_sync_optical_service_interface(optical_controller, root)
                    if not sync_result[0]:
                        output.status = "failed"
                        output.message = "Error: " + sync_result[1]
                        return
                sips_validity = controller.validate_sips(root, optical_controller,
                                                         input.optical_service_interface)

            except AttributeError as e:
                self.log.error(AsciiArt.roadblock)
                self.log.error("AttributeError during List SIP " + str(e))
                exp = (
                    UserError(self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND)
                    .set_context(
                        "Dynamic Method Error",
                        "partial_sync_optical_service_interface "
                        "or validate_sips method is "
                        "missing from multi-vendor python class",
                    )
                    .add_state("Controller", optical_controller)
                    .finish()
                )
                self.log.error(exp)
                output.status = "failed"
                output.message = f"Error: {exp.statusCode.code} Dynamic Method Error; "\
                                 "partial_sync_optical_service_interface or " \
                                 "validate_sips method is missing from "\
                                 "multi-vendor python class"
                return
            output.status = "successful"
            for entry in sips_validity:
                sip = output.optical_service_interface.create(entry[0])
                sip.valid = entry[1]
                if input.optical_service_interface[entry[0]].optical_sip is None:
                    sip.network_element = input.optical_service_interface[entry[0]].network_element
                    sip.optical_add_drop = \
                        input.optical_service_interface[entry[0]].optical_add_drop
                else:
                    sip.optical_sip = input.optical_service_interface[entry[0]].optical_sip
        self.log.info(AsciiArt.ninja_star)
        self.log.info(f"EXIT_POINT for Optical validate sip callback {kp}")
