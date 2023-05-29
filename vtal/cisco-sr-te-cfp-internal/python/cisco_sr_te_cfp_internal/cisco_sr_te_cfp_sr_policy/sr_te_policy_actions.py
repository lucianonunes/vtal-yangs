# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
import cisco_sr_te_cfp_internal.utils as Utils
from cisco_tsdn_core_fp_common.status_codes.sr_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.sr_te_cfp_base_exception import CustomActionException
import traceback
from cisco_tsdn_core_fp_common.utils import get_action_timeout


class SRPolicyInternalSelfTest(ncs.dp.Action):
    """
    Action handler for self-test for all head-ends
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Running self test for: {kp}")
        username = uinfo.username
        with ncs.maapi.single_read_trans(username, "system") as th:
            root = ncs.maagic.get_root(th)

            # Get service node from action path
            action = ncs.maagic.get_node(th, kp)
            policy_service = ncs.maagic.cd(action, "..")

            status = None
            message = None
            try:
                router = Utils.\
                    get_device_impl_default_class(self, root, policy_service,
                                                  policy_service.head_end,
                                                  root.cisco_sr_te_cfp_internal__cfp_configurations
                                                  .dynamic_device_mapping)

                (status, message) = router.\
                    sr_policy_self_test(uinfo, root, policy_service, policy_service.head_end)
            except Exception as e:
                traceback.print_exc()
                exp = (CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))
                       .set_context("Self Test Error", "Self Test Failed")
                       .finish())
                status = "failed"
                message = "Error: " + str(exp)

            output.status = status
            output.message = message
