import ncs
import _ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
import re
from traceback import print_exc
from core_fp_common import cleanup_utils as CleanupUtils
from core_fp_common.common_utils import get_local_user
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import \
    CustomActionException
from cisco_tsdn_core_fp_common.utils import set_service_cleanup_flag, delete_service_cleanup_flag, \
    check_service_cleanup_flag, get_action_timeout


class L2NMInternalPlanChangeHandler(ncs.dp.Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info("L2NMInternalPlanChangeHandler Internal "
                      f"plan kicker changed for: {input.kicker_id} {input.path} {input.tid}")
        re_search = re.search('flat-L2vpn-plan{(.*?)}', input.path)
        l2vpn_service_name = re_search.group(1)

        # flat-L2vpn service name will be like L2NM-<SERVICE_NAME>-internal
        if not ("L2NM-" and "-internal" in l2vpn_service_name):
            self.log.info(f"L2VPN service {l2vpn_service_name} not created by L2NM.")
            return
        try:
            service_name = l2vpn_service_name.split("L2NM-", 1)[1].split("-internal", 1)[0]

            # If cleanup is in progress, do not take any action
            service_kp = f"/l2vpn-ntw/vpn-services/vpn-service{{{service_name}}}"
            if check_service_cleanup_flag(self.log, service_kp, uinfo.username):
                return

            self._redeploy_zombie_service(service_name, get_local_user())

        except Exception as e:
            self.log.error(f"Exception L2NMInternalPlanChangeHandler: {e}")
            print_exc()

    def _redeploy_service(self, service_name, username):
        with maapi.single_read_trans(username, "system", db=ncs.RUNNING) as th:
            l2nm_kp = f'/l2vpn-ntw/vpn-services/vpn-service{{{service_name}}}'
            l2nm_plan_kp = f'/l2vpn-ntw/vpn-services/vpn-service-plan{{{service_name}}}'
            if th.exists(l2nm_plan_kp):
                l2nm_plan = maagic.get_node(th, l2nm_plan_kp)
                if len(l2nm_plan.plan.component) > 0 and th.exists(l2nm_kp):
                    l2nm_service = maagic.get_node(th, l2nm_kp)
                    l2nm_service.reactive_re_deploy()
                    self.log.info(f"Service Redeploy done for L2NM : {service_name}")

    def _redeploy_zombie_service(self, service_name, username):
        with maapi.single_read_trans(username, "system", db=ncs.OPERATIONAL) as th:
            zombie_service_path = f"/l2vpn-ntw/vpn-services/vpn-service[vpn-id='{service_name}']"
            l2nm_zombie_kp = f"/ncs:zombies/ncs:service{{\"{zombie_service_path}\"}}"
            if th.exists(l2nm_zombie_kp):
                l2nm_zombie = maagic.get_node(th, l2nm_zombie_kp)
                l2nm_zombie.reactive_re_deploy()
                self.log.info(f"Zombie Service Redeploy done for L2NM : {service_name}")
            else:
                self._redeploy_service(service_name, username)


class L2NMSelfTest(ncs.dp.Action):
    """
    Action handler for self-test
    """
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Running Flat L2NM Self Test : {kp}")
        try:
            with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
                # Get context
                root = maagic.get_root(th)
                re_search = re.search('vpn-service{(.*?)}', str(kp))
                l2vpn_service_name = f"L2NM-{re_search.group(1)}-internal"

                # Get L2vpn self-test action
                self_test = root.cisco_flat_L2vpn_fp__flat_L2vpn[l2vpn_service_name].\
                    action.self_test

                # Execute self-test
                self_test_output = self_test()

                # Return output
                output.status = self_test_output.status
                output.message = self_test_output.message

        except Exception as e:
            self.log.error(f"Exception FlatL2vpnCFSSelfTest: {e}")
            print_exc()
            exp = CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))\
                .set_context("Self Test", "Self Test Failed").finish()
            output.status = False
            output.message = str(exp)


class L2NMCleanupAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services cleanup
    """
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Cleanup Action for L2NM service={input.service} ")

        service = input.service
        no_networking = input.no_networking
        vpn_node_id = input.vpn_node

        l2vpn_service_name = f"L2NM-{service}-internal"
        plan_kp = f"/l2vpn-ntw/vpn-services/vpn-service-plan{{{service}}}"
        zombie_service_xp = "/l2vpn-ntw/vpn-services/vpn-service" \
                            f"[vpn-id='{service}']"
        service_kp = f"/l2vpn-ntw/vpn-services/vpn-service{{{service}}}"
        vpn_node_kp = service_kp + f"/vpn-nodes/vpn-node{{{vpn_node_id} {vpn_node_id}}}"

        cleanup_log = []
        force_back_track_comp = []

        commit_params = ncs.maapi.CommitParams()
        if no_networking:
            commit_params.no_networking()

        set_service_cleanup_flag(self, service_kp, uinfo.username)

        try:
            with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:

                root = maagic.get_root(th)
                vpn_service_plan = maagic.get_node(th, plan_kp)

                cleanup_log.append(f"Cleaning up l2vpn-ntw service: {service} \n")

                cleanup_plan_path = None

                if th.exists(plan_kp):
                    if vpn_node_id is not None:
                        local_site = True if vpn_node_id in vpn_service_plan.local_sites else False

                        self.validate_vpn_node_cleanup(th, service, local_site, vpn_node_kp)

                        cleanup_plan_path = \
                            self.register_vpn_node_cleanup(vpn_service_plan, vpn_node_id,
                                                           force_back_track_comp,
                                                           zombie_service_xp)

                        if not local_site:
                            CleanupUtils.delete_service(self, vpn_node_kp, cleanup_log,
                                                        uinfo, commit_params)
                    else:
                        CleanupUtils.delete_service(self, service_kp, cleanup_log,
                                                    uinfo, commit_params)

                        # Get all plan components for force-backtrack
                        cleanup_plan_path = plan_kp
                        components = vpn_service_plan.plan.component

                        for component in components:
                            force_back_track_comp.append(
                                (component.force_back_track, zombie_service_xp))

                # Cleanup
                self._cleanup_service(root, th, uinfo, service, l2vpn_service_name,
                                      no_networking, plan_kp, zombie_service_xp, service_kp,
                                      force_back_track_comp, cleanup_plan_path, cleanup_log,
                                      vpn_node_id)

                self.log.info("Cleanup Successful for L2NM")
                cleanup_log.append("\n Cleanup Successful for L2NM \n")
                output.success = True

        except Exception as e:
            self.log.error(f"Exception in FlatL2vpnCleanupAction() : {e}")
            print_exc()
            exp = CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(e)) \
                .set_context("Cleanup Action", "Service cleanup failed").finish()
            cleanup_log.append(f'\nERROR: {exp}')
            output.success = False
        finally:
            output.detail = "".join(cleanup_log)
            delete_service_cleanup_flag(self, service_kp, uinfo.username)

    def validate_vpn_node_cleanup(self, th, service_name, local_site, vpn_node_kp):
        if local_site and th.exists(vpn_node_kp):
            err_msg = (f"Service Cleanup Failed for {service_name},"
                       " since vpn-node is used as local-site and must be deleted first")
            self.log.error(err_msg)
            raise Exception(err_msg)

    def register_vpn_node_cleanup(self, vpn_service_plan, vpn_node_id,
                                  force_back_track_comp, zombie_service_xp):
        vpn_node_key = ("ietf-l2vpn-ntw-nano-services:vpn-node", vpn_node_id)
        component = vpn_service_plan.plan.component

        cleanup_plan_path = None

        if component.exists(vpn_node_key):
            vpn_node_comp = component[vpn_node_key]
            cleanup_plan_path = vpn_node_comp._path
            force_back_track_comp.append((vpn_node_comp.force_back_track, zombie_service_xp))

        return cleanup_plan_path

    def _cleanup_service(self, root, th, uinfo, service, l2vpn_service_name,
                         no_networking, plan_kp, zombie_service_xp, service_kp,
                         force_back_track_comp, cleanup_plan_path, cleanup_log,
                         vpn_node_id):

        # Call internal cleanup action
        self._invoke_internal_cleanup(root, l2vpn_service_name, no_networking,
                                      cleanup_log, vpn_node_id)

        # Force-backtrack all upper service components - RT#48440 Filed
        CleanupUtils.invoke_backtrack_actions(self, service, force_back_track_comp,
                                              no_networking)
        cleanup_log.append("\n Removed all plan components")

        # Remove leftover plan path
        CleanupUtils.remove_plan_paths(self, cleanup_plan_path, cleanup_log, uinfo)

        # Remove zombies
        CleanupUtils.remove_zombie(self, zombie_service_xp, cleanup_log, uinfo)

        # Due to zombie removal race conditions
        # plan elements are partially recreated causing cleanup leftovers.
        # This does a final check on plan path removal.
        CleanupUtils.remove_plan_paths(self, cleanup_plan_path, cleanup_log, uinfo)

        # reactive-redeploy the service to re-align with flat-L2vpn plan
        if vpn_node_id is not None:
            maagic.get_node(th, service_kp).reactive_re_deploy()

    def _invoke_internal_cleanup(self, root, l2vpn_service_name, no_networking,
                                 cleanup_log, vpn_node_id):
        # Get L2vpn cleanup action
        internal_cleanup_action = root.cisco_flat_L2vpn_fp__flat_L2vpn_actions.cleanup
        self.log.info(f"L2vpn cleanup action: {internal_cleanup_action._path}")

        # Setup input
        internal_cleanup_input = internal_cleanup_action.get_input()
        internal_cleanup_input.service = l2vpn_service_name
        internal_cleanup_input.no_networking = no_networking

        if vpn_node_id is not None:
            internal_cleanup_input.site = vpn_node_id

        # Run cleanup action and get output
        internal_cleanup_output = internal_cleanup_action(internal_cleanup_input)
        cleanup_log.append(internal_cleanup_output.detail)

        # If cleanup failed raise exception
        if not internal_cleanup_output.success:
            raise Exception("Internal Service Cleanup Failed")


class L2NMSiteRecoveryAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services site error recovery
    """
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Running Flat L2NM Site Recovery action on CFS : {kp}")
        re_search = re.search('/vpn-service{(.*?)}/vpn-nodes/vpn-node{(.*?) (.*?)}', str(kp))
        service = re_search.group(1)
        vpn_node = re_search.group(2)
        error_recovery_action(self, uinfo, name, kp, service, vpn_node,
                              input.sync_direction, output)


class L2NMRecoveryAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services site error recovery
    """
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Running L2NM Recovery action on CFS : {kp}")
        error_recovery_action(self, uinfo, name, kp, input.service, input.vpn_node,
                              input.sync_direction, output)


def error_recovery_action(self, uinfo, name, kp, service, vpn_node, sync_direction, output):
    recovery_log = []
    recovery_log.append(f"Recovering L2NM service: {service} sync_direction: {sync_direction} \n")
    l2vpn_service_name = f"L2NM-{service}-internal"
    try:
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            # Check if service exists
            l2nm_plan_path = f"/l2vpn-ntw:l2vpn-ntw/vpn-services/vpn-service-plan{{{service}}}"
            if not th.exists(l2nm_plan_path):
                err_msg = f"No such service found for recovery: {service}"
                recovery_log.append(err_msg)
                raise Exception(err_msg)

            # Get context
            root = ncs.maagic.get_root(th)

            # Get L2vpn error-recovery action
            error_recovery = root.cisco_flat_L2vpn_fp__flat_L2vpn_actions.error_recovery
            self.log.info(f"L2vpn error_recovery action: {error_recovery._path}")

            # Populate input
            error_recovery_inputs = \
                set_error_recovery_input(self, root, vpn_node, l2vpn_service_name,
                                         sync_direction, error_recovery)

            for error_recovery_input in error_recovery_inputs:
                # Execute error-recovery action
                error_recovery_output = error_recovery(error_recovery_input)

                # Return output
                output.success = error_recovery_output.success
                recovery_log.append(error_recovery_output.detail)
                output.detail = ''.join(recovery_log)

    except Exception as e:
        self.log.error(f"Exception L2NM RecoveryAction: {e}")
        print_exc()
        exp = CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e)) \
            .set_context("Recovery Action", f"Recovery failed for {service}")\
            .finish()
        recovery_log.append("\nRecovery Failed\n\n")
        output.success = False
        recovery_log.append(str(exp))
        output.detail = ''.join(recovery_log)


def set_error_recovery_input(self, root, vpn_node, l2vpn_service_name,
                             sync_direction, error_recovery):

    error_recovery_inputs = list()
    error_recovery_input = error_recovery.get_input()
    error_recovery_input.service = l2vpn_service_name
    error_recovery_input.sync_direction = sync_direction

    if vpn_node:
        l2vpn_components = root.\
            cisco_flat_L2vpn_fp__flat_L2vpn_plan[l2vpn_service_name].plan.component

        remote_site_pe = None
        local_site_pe = None
        site_pe = None

        for component in l2vpn_components:
            self.log.info(f"L2vpn component type:{component.type}, name:{component.name}")

            if component.type == 'cisco-flat-L2vpn-fp-nano-plan-services:site':
                site_pe = component.pe
            else:
                if component.type == 'cisco-flat-L2vpn-fp-nano-plan-services:local-site':
                    local_site_pe = component.name
                elif component.type == 'cisco-flat-L2vpn-fp-nano-plan-services:remote-site':
                    remote_site_pe = component.name

            if vpn_node == site_pe:
                error_recovery_input.site_only = component.name
                error_recovery_inputs.append(error_recovery_input)

                # reset for next component
                error_recovery_input = error_recovery.get_input()
                error_recovery_input.service = l2vpn_service_name
                error_recovery_input.sync_direction = sync_direction

            else:
                if vpn_node == remote_site_pe:
                    error_recovery_input.remote_site_only = vpn_node
                    error_recovery_inputs.append(error_recovery_input)

                if vpn_node == local_site_pe:
                    error_recovery_input.local_site_only = vpn_node
                    error_recovery_inputs.append(error_recovery_input)
    else:
        error_recovery_inputs.append(error_recovery_input)

    return error_recovery_inputs
