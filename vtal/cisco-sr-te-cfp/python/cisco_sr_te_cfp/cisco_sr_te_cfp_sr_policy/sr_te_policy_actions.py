# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
import ncs.maagic as maagic
import ncs.maapi as maapi
from ncs.dp import Action
import cisco_sr_te_cfp.utils as Utils
from cisco_tsdn_core_fp_common.sr_te_policy import SrTePolicy
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import CustomActionException
import traceback
from cisco_tsdn_core_fp_common.utils import check_service_cleanup_flag, get_action_timeout


class InternalPolicyPlanChangeHandler(ncs.dp.Action):
    """
    Action handler for Policy internal plan change
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Internal plan kicker changed for: {input.kicker_id} "
                      f"{input.path} {input.tid}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))

        policy_wrapper = SrTePolicy(input.path, self.log)

        # If cleanup is in progress, do not take any action
        if check_service_cleanup_flag(self.log, policy_wrapper.service_kp, uinfo.username):
            return

        try:
            policy_wrapper.redeploy()
        except Exception:
            traceback.print_exc()

        self.log.info(f"Internal plan change handled for {input.path}")


class SRPolicyGlobalSelfTest(ncs.dp.Action):
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

            # Get service node from action path
            action = ncs.maagic.get_node(th, kp)
            policy_service = ncs.maagic.cd(action, "..")

            status = None
            message = None
            try:
                for head_end in policy_service.head_end:
                    internal_action = None
                    if Utils.is_lsa:
                        # Get RFS Node to device mapping
                        rfs_node = LsaUtils.get_remote_nso(head_end.name)
                        internal_action = (
                            root.ncs__devices.device[rfs_node]
                            .config.cisco_sr_te_cfp_internal__sr_te
                            .cisco_sr_te_cfp_sr_policies_internal__policies.policy[
                                policy_service.name, head_end.name
                            ]
                            .action.self_test
                        )
                        (status, message) = self._call_internal_selftest_action(internal_action)
                    else:
                        # Invoke internal cleanup action locally
                        internal_action = (
                            root.cisco_sr_te_cfp_internal__sr_te
                            .cisco_sr_te_cfp_sr_policies_internal__policies.policy[
                                policy_service.name, head_end.name
                            ].action.self_test
                        )
                        (status, message) = self._call_internal_selftest_action(internal_action)

                    if status != "success" and status != "failed":
                        raise CustomActionException(
                            self.log, StatusCodes.SELF_TEST_STATUS_ERROR
                        ).set_context(
                            "Self Test Error", "Unsupported status returned"
                        ).add_state(
                            "Status", status
                        ).add_state(
                            "Policy Name", policy_service.name
                        ).add_state(
                            "Keypath", str(kp)
                        ).finish()

            except CustomActionException:
                raise
            except Exception as e:
                traceback.print_exc()
                exp = (CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))
                       .set_context("Self Test Error", "Self Test Failed")
                       .finish())
                status = "failed"
                message = "Error: " + str(exp)

            output.status = status
            output.message = message

    def _call_internal_selftest_action(self, internal_action):
        internal_action_input = internal_action.get_input()
        internal_action_output = internal_action(internal_action_input)

        return (internal_action_output.status, internal_action_output.message)


class SrTePolicyRecoveryAction(ncs.dp.Action):
    """
    Action handler for SR TE Policy service create/update transient failure recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"Policy Recovery Action for: {kp}")
            root = maagic.get_root(th)
            service = maagic.get_node(th, kp)

            recovery_action = root.cisco_sr_te_cfp__sr_te.error_recovery
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service.name
            recovery_action_input.service_type = "sr-policy"
            recovery_action_input.sync_direction = input.sync_direction
            recovery_action_output = recovery_action(recovery_action_input)

            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail
