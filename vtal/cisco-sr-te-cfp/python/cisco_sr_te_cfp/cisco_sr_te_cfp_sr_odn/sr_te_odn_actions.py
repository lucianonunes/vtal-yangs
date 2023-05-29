# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
from ncs.dp import Action
import ncs.maagic as maagic
import ncs.maapi as maapi
import traceback
from cisco_tsdn_core_fp_common.utils import (
    check_service_cleanup_flag,
    get_action_timeout,
)
from cisco_tsdn_core_fp_common.sr_te_odn import SrTeOdn


class InternalOdnPlanChangeHandler(ncs.dp.Action):
    """
    Action handler for ODN internal plan change
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Internal plan kicker changed for: {input.kicker_id} "
                      f"{input.path} {input.tid}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        odn_wrapper = SrTeOdn(input.path, self.log)

        # If cleanup is in progress, do not take any action
        if check_service_cleanup_flag(self.log, odn_wrapper.service_kp, uinfo.username):
            return

        try:
            odn_wrapper.redeploy()
        except Exception:
            traceback.print_exc()

        self.log.info(f"Internal plan change handled for: {input.path}")


class SrTeOdnRecoveryAction(ncs.dp.Action):
    """
    Action handler for SR TE ODN service create/update transient failure recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"ODN Recovery Action for: {kp}")
            root = maagic.get_root(th)
            service = maagic.get_node(th, kp)

            recovery_action = root.cisco_sr_te_cfp__sr_te.error_recovery
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service.name
            recovery_action_input.service_type = "sr-odn"
            recovery_action_input.sync_direction = input.sync_direction
            recovery_action_output = recovery_action(recovery_action_input)

            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail


class SrTeOdnHeadEndRecoveryAction(ncs.dp.Action):
    """
    Action handler for SR TE ODN service create/update transient failure recovery on head-end
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"ODN Head-End Recovery Action for: {kp}")
            root = maagic.get_root(th)
            head_end = maagic.get_node(th, kp)
            service = ncs.maagic.cd(head_end, "../")

            recovery_action = root.cisco_sr_te_cfp__sr_te.error_recovery
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service.name
            recovery_action_input.device = head_end.name
            recovery_action_input.service_type = "sr-odn"
            recovery_action_input.sync_direction = input.sync_direction
            recovery_action_output = recovery_action(recovery_action_input)

            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail
