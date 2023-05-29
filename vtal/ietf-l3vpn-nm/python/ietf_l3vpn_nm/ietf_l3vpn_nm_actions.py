import _ncs

from ncs import RUNNING, OPERATIONAL
from ncs.dp import Action
from ncs.maapi import single_read_trans, CommitParams
from ncs.maagic import get_root, get_node, cd as maagic_cd
from re import search
from traceback import print_exc

from core_fp_common.common_utils import get_local_user
from cisco_tsdn_core_fp_common.status_codes.ietf_l3vpn_nm_status_codes import StatusCodes

from cisco_tsdn_core_fp_common.status_codes.ietf_l3vpn_nm_base_exception import \
    CustomActionException
from core_fp_common.cleanup_utils import (
    remove_zombie, invoke_backtrack_actions, delete_service, remove_plan_paths
)
from cisco_tsdn_core_fp_common.utils import (
    set_service_cleanup_flag, delete_service_cleanup_flag,
    check_service_cleanup_flag, get_action_timeout
)
from . import utils


class L3NMInternalPlanChangeHandler(Action):
    """
    Action handler for L3NM plan change
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Internal plan kicker changed for: {input.kicker_id} "
                      f"{input.path} {input.tid}")
        # Grab service name
        # Example: (Extract service_name='L3'), input.path =
        # '/cisco-flat-L3vpn-fp:flat-l3vpn-plan{L3}'
        l3vpn_service_name = search("flat-L3vpn-plan{(.*)}", input.path).group(1)
        # Extract NB service name from internal service name
        # Ex. (service_name = L3NM-<service-name>-internal)
        if not ("L3NM-" and "-internal" in l3vpn_service_name):
            self.log.info(f"L3VPN service {l3vpn_service_name} not created by L3NM.")
            return
        service_name = l3vpn_service_name.split("L3NM-", 1)[1].split("-internal", 1)[0]

        # If cleanup is in progress, do not take any action
        l3nm_service_kp = utils.get_l3nm_service_kp(service_name)
        if check_service_cleanup_flag(self.log, l3nm_service_kp, uinfo.username):
            return

        try:
            username = get_local_user()
            # Redeploy zombie if exists
            with single_read_trans(username, "system", db=OPERATIONAL) as th:
                l3nm_zombie_kp = utils.get_l3nm_zombie_kp(service_name)
                if th.exists(l3nm_zombie_kp):
                    l3nm_zombie = get_node(th, l3nm_zombie_kp)
                    l3nm_zombie.reactive_re_deploy()
                    self.log.info(f"L3NM Zombie Service Redeploy done for: {service_name}")
                    return
            # Redeploy service if exists
            with single_read_trans(username, "system", db=RUNNING) as th:
                l3nm_plan_kp = utils.get_l3nm_plan_kp(service_name)
                if th.exists(l3nm_plan_kp):
                    l3nm_plan = get_node(th, l3nm_plan_kp)
                    if len(l3nm_plan.plan.component) > 0 and th.exists(l3nm_service_kp):
                        l3nm_service = get_node(th, l3nm_service_kp)
                        l3nm_service.reactive_re_deploy()
                        self.log.info(f"L3NM Service Redeploy done for: {service_name}")
        except Exception as e:
            self.log.info(f"{e}\n{print_exc()}")


class L3NMCleanupAction(Action):
    """
    Action handler for L3NM services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        # Init vars
        cleanup_log = []
        service = input.service
        internal_service_name = f"L3NM-{service}-internal"
        no_networking = input.no_networking
        vpn_node = None
        if input.vpn_node:
            vpn_node = f"{input.vpn_node}_{input.vpn_network_access_id}"

        # Init paths
        plan_kp = f"/l3vpn-ntw/vpn-services/vpn-service-plan{{{service}}}"
        service_kp = f"/l3vpn-ntw/vpn-services/vpn-service{{{service}}}"
        vpn_node_kp = f"{service_kp}/vpn-nodes/vpn-node{{{input.vpn_node}}}"
        vpn_network_access_kp = (f"{vpn_node_kp}/vpn-network-accesses/vpn-"
                                 f"network-access{{{input.vpn_network_access_id}}}")
        zombie_service_xp = ("/l3vpn-ntw/vpn-services/vpn-service"
                             f"[vpn-id='{service}']")

        try:
            with single_read_trans(uinfo.username, "system", db=OPERATIONAL) as th:
                root = get_root(th)

                commit_params = CommitParams()
                if no_networking:
                    commit_params.no_networking()

                # Validate vpn-node conditions
                if (vpn_node and th.exists(service_kp)
                        and th.exists(vpn_node_kp) and th.exists(vpn_network_access_kp)):

                    service_obj = get_node(th, service_kp)
                    vpn_node_obj = get_node(th, vpn_node_kp)

                    # Check if vpn-network-access-id is only vpn-network-access defined on vpn-node
                    if len(vpn_node_obj.vpn_network_accesses.vpn_network_access) == 1:
                        # Check if vpn-node is only one defined in service, if so delete service
                        if len(service_obj.vpn_nodes.vpn_node) == 1:
                            # Delete entire service
                            vpn_node = None
                        else:
                            # Delete entire vpn-node
                            vpn_network_access_kp = vpn_node_kp

                # If vpn_node, delete vpn_node if exists
                if vpn_node:
                    delete_service(self, vpn_network_access_kp, cleanup_log, uinfo, commit_params)
                # Otherwise, delete service if exists
                else:
                    set_service_cleanup_flag(self, service_kp, uinfo.username)
                    delete_service(self, service_kp, cleanup_log, uinfo, commit_params)

                # Get all plan components for force-backtrack
                fbt_components = []
                cleanup_plan_path = None
                if th.exists(plan_kp):
                    components = root.\
                        l3vpn_ntw__l3vpn_ntw.vpn_services.vpn_service_plan[service].plan.component
                    if (vpn_node and ("ietf-l3vpn-ntw-nano-services:vpn-node", vpn_node)
                            in components):
                        # Assign vpn_node component as cleanup path
                        component = components["ietf-l3vpn-ntw-nano-services:vpn-node", vpn_node]
                        cleanup_plan_path = component._path
                        # Add vpn-node component force-backtrack action
                        fbt_components = [(component.force_back_track, zombie_service_xp)]
                    elif not vpn_node:
                        # Get all plan components for force-backtrack
                        for component in components:
                            fbt_components.append((component.force_back_track, zombie_service_xp))
                        # Assign plan kp to cleanup
                        cleanup_plan_path = plan_kp

                # Cleanup
                self._cleanup_service(root, th, uinfo, vpn_node, service,
                                      internal_service_name, no_networking,
                                      plan_kp, zombie_service_xp, service_kp,
                                      fbt_components, cleanup_plan_path, cleanup_log)

                self.log.info("Cleanup Successful for L3NM")
                cleanup_log.append("\n Cleanup Successful for L3NM \n")
                output.success = True
                output.detail = "".join(cleanup_log)

        except Exception as e:
            print_exc()
            exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e))
                   .set_context("Cleanup Action", "Service cleanup failed").finish())
            self.log.error(f"Cleanup Failed : {exp}")
            cleanup_log.append("\n Cleanup Failed \n")
            cleanup_log.append(str(exp))
            output.success = False
            output.detail = "".join(cleanup_log)
        finally:
            delete_service_cleanup_flag(self, service_kp, uinfo.username)

    def _cleanup_service(self, root, th, uinfo,
                         vpn_node, service, internal_service_name,
                         no_networking, plan_kp, zombie_service_xp, service_kp,
                         fbt_components, cleanup_plan_path, cleanup_log):

        # Call internal cleanup action
        self._invoke_internal_cleanup(root, internal_service_name, vpn_node,
                                      no_networking, cleanup_log)

        # Force-backtrack all upper service components - RT#48440 Filed
        invoke_backtrack_actions(self, service, fbt_components, no_networking)
        cleanup_log.append("\n Removed all plan components")

        # Remove zombies
        if not vpn_node:
            remove_zombie(self, zombie_service_xp, cleanup_log, uinfo)

        # Remove leftover plan path
        if cleanup_plan_path:
            remove_plan_paths(self, cleanup_plan_path, cleanup_log, uinfo)

        # Remove zombies for upper service again
        # This is to cover the case where cleanup is requested without service delete.
        # In this case we will see race conditions where zombie is recreated
        # after we pass the zombie deletion check during removal of config-apply callbacks
        if not vpn_node:
            remove_zombie(self, zombie_service_xp, cleanup_log, uinfo)

        # Due to zombie removal race conditions
        # plan elements are partially recreated causing cleanup leftovers.
        # This does a final check on plan path removal.
        if cleanup_plan_path:
            remove_plan_paths(self, cleanup_plan_path, cleanup_log, uinfo)

        # If the service is still present, we should re-deploy the service to reconcile
        # as the out of band deletion of data can leave the northbound plan in weird state.
        if vpn_node and th.exists(service_kp):
            external_service = get_node(th, service_kp)
            external_service.reactive_re_deploy()

    def _invoke_internal_cleanup(self, root, internal_service_name,
                                 vpn_node, no_networking, cleanup_log):
        # Get internal cleanup action
        internal_cleanup_action = root.cisco_flat_L3vpn_fp__flat_L3vpn_actions.cleanup

        # Setup input
        internal_cleanup_input = internal_cleanup_action.get_input()
        internal_cleanup_input.service = internal_service_name
        internal_cleanup_input.endpoint = vpn_node
        internal_cleanup_input.no_networking = no_networking

        # Run cleanup action and get output
        internal_cleanup_output = internal_cleanup_action(internal_cleanup_input)
        cleanup_log.append(internal_cleanup_output.detail)

        # If cleanup failed raise exception
        if not internal_cleanup_output.success:
            # We raise regular exception here which will be caught by try except and turned into
            # status code
            raise Exception("Internal Service Cleanup Failed")


class L3NMSelfTest(Action):
    """
    Action handler for L3NM self-test
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        try:
            with single_read_trans(uinfo.username, "system", db=OPERATIONAL) as th:
                # Get context
                root = get_root(th)
                service = get_node(th, kp)
                internal_service_name = f"L3NM-{service.vpn_id}-internal"

                # Get internal self-test action
                self_test = root.\
                    cisco_flat_L3vpn_fp__flat_L3vpn[internal_service_name].action.self_test

                # Execute self-test
                self_test_output = self_test()

                # Return output
                output.status = self_test_output.status
                output.message = self_test_output.message

        except Exception as e:
            print_exc()
            exp = (CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))
                   .set_context("Cleanup Action", "Error while invoking L3VPN self-test action")
                   .finish())
            self.log.error(str(exp))
            output.status = False
            output.message = str(exp)


class L3NMRecoveryAction(Action):
    """
    Action handler for L3NM recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        try:
            with single_read_trans(uinfo.username, "system", db=OPERATIONAL) as th:
                # Get context
                root = get_root(th)

                # Get internal error-recovery action
                error_recovery = root.cisco_flat_L3vpn_fp__flat_L3vpn_actions.error_recovery

                # Populate input
                endpoint = None
                if input.vpn_node:
                    endpoint = f"{input.vpn_node}_{input.vpn_network_access_id}"

                error_recovery_input = error_recovery.get_input()
                error_recovery_input.service = f"L3NM-{input.service}-internal"
                error_recovery_input.endpoint = endpoint
                error_recovery_input.sync_direction = input.sync_direction

                # Execute self-test
                error_recovery_output = error_recovery(error_recovery_input)

                # Return output
                output.success = error_recovery_output.success
                output.detail = error_recovery_output.detail

        except Exception as e:
            print_exc()
            exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                   .set_context("Recovery Action", "Error while invoking L3VPN "
                                "error recovery action").finish())
            self.log.error(str(exp))
            output.success = False
            output.detail = str(exp)


class L3NMServiceRecoveryAction(Action):
    """
    Action handler for L3NM service recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        try:
            with single_read_trans(uinfo.username, "system", db=OPERATIONAL) as th:
                # Get context
                root = get_root(th)
                service = get_node(th, kp)
                internal_service_name = f"L3NM-{service.vpn_id}-internal"

                # Get internal error-recovery action
                error_recovery = root.cisco_flat_L3vpn_fp__flat_L3vpn[
                    internal_service_name
                ].error_recovery

                error_recovery_input = error_recovery.get_input()
                error_recovery_input.sync_direction = input.sync_direction
                # Execute error-recovery
                error_recovery_output = error_recovery(error_recovery_input)

                # Return output
                output.success = error_recovery_output.success
                output.detail = error_recovery_output.detail

        except Exception as e:
            print_exc()
            exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                   .set_context("Recovery Action", "Error while invoking L3VPN service "
                                "error recovery action").finish())
            self.log.error(str(exp))
            output.success = False
            output.detail = str(exp)


class L3NMEndpointRecoveryAction(Action):
    """
    Action handler for L3NM endpoint recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        try:
            with single_read_trans(uinfo.username, "system", db=OPERATIONAL) as th:
                # Get context
                root = get_root(th)
                vpn_node = get_node(th, kp)
                service = maagic_cd(vpn_node, "../..")
                internal_service_name = f"L3NM-{service.vpn_id}-internal"

                # Parse input
                endpoint = f"{vpn_node.ne_id}_{input.vpn_network_access_id}"

                # Get internal error-recovery action
                error_recovery = (root.cisco_flat_L3vpn_fp__flat_L3vpn[internal_service_name]
                                  .endpoint[endpoint].error_recovery)

                error_recovery_input = error_recovery.get_input()
                error_recovery_input.sync_direction = input.sync_direction

                # Execute error-recovery
                error_recovery_output = error_recovery(error_recovery_input)

                # Return output
                output.success = error_recovery_output.success
                output.detail = error_recovery_output.detail

        except Exception as e:
            print_exc()
            exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                   .set_context("Recovery Action", "Error while invoking L3VPN endpoint "
                                "error recovery action").finish())
            self.log.error(str(exp))
            output.success = False
            output.detail = str(exp)
