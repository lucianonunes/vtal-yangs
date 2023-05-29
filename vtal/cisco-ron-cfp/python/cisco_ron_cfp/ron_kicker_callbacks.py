import re
from datetime import datetime

import ncs
import _ncs
import ncs.maagic as maagic
from ncs.ns.ncs_ns import ns as ns_ncs
from ncs.dp import StateManager
from ncs.dp import Daemon

from .ron_ml_errors import CustomActionException
from .constants import StrConstants

from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes
from core_fp_common import common_utils


class NotifStreamSM(StateManager):
    def __init__(self, log, cb_sockets):
        self.cb_sockets = cb_sockets
        super().__init__(log)

    def setup(self, state, previous_state):
        global notifCtx
        self.cb_sockets['notif_socket'] = self.register_notification_stream(
            state, None, ncs.dp.take_worker_socket(state, "ron-plan-notif-name",
                                                          "ron-plan-notif-key"),
            "service-state-changes")

    def teardown(self, state, finished):
        ncs.dp.return_worker_socket(state, "ron-plan-notif-key")


class NotifStreamRegistrar(object):
    def __init__(self, log, cb_sockets):
        sm = NotifStreamSM(log, cb_sockets)
        self.notif_stream_daemon = Daemon('ron-plan-notif', state_mgr=sm)
        self.notif_stream_daemon.start()

    def cleanup(self):
        self.notif_stream_daemon.finish()


class MLPlanFailedKickerCallback(ncs.dp.Action):
    """
    RON-ML plan failed kicker callback.
    When service fails during initial creation of internal(optical, zr) service, ron-ml plan is
    marked failed but the plan/self or other components remain unchanged. This doesn't trigger
    platform's plan-state-change notifications.

    In such a scenarios, this kicker would send an update failed plan-state-change notification
    #CDET : CSCvz13992
    """

    def init(self, cb_sockets):
        self.cb_sockets = cb_sockets

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"ron-ml plan failure kicker : {input.path}")
        """
            The trigger is
            /cisco-ron-cfp:ron/ron-ml-plan{%s}/plan/failed
            This reg-ex will parse for the name followed by plan{ until }
        """
        match = re.search("plan{(.*?)}", input.path)
        ron_ml_nm = match.group(1)

        plan_path = input.path.split("/failed")[0]
        l_user = common_utils.get_local_user()
        try:
            with ncs.maapi.single_read_trans(l_user, StrConstants.system, db=ncs.RUNNING) as th:
                root = maagic.get_root(th)
                if ron_ml_nm in root.cisco_ron_cfp__ron.ron_ml:
                    # We are in create flow
                    ron_plan = maagic.get_node(th, plan_path)
                    self_key = (StrConstants.ncs_self, StrConstants.self_cn)
                    self_comp = ron_plan.component[self_key]
                    avoid = StrConstants.post_action_states_to_avoid
                    # We should only react when plan is marked failed and self/ready is not-reached
                    # Exception: skip post_action_status failures. When post-action fails self/ready
                    # will be in not-reached, however explicit notification_generator has been
                    # updated to send out modify failed notificaion.
                    if (ron_plan.failed
                        and self_comp.state[StrConstants.ncs_init].post_action_status not in avoid
                            and self_comp.state[StrConstants.ncs_ready].status == "not-reached"):
                        # send plan failed notification
                        path = _ncs.maapi.xpath2kpath(
                            th.maapi.msock,
                            StrConstants.ron_service_xpath.format(ron_ml_nm))
                        self.send_plan_failed_notif(path)

        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            err = CustomActionException(self.log,
                                        StatusCodes.PLAN_FAILURE_CALLBACK_ERROR, str(exp))

            self.log.error(err)
        self.log.info(f"Done processing ron-ml plan failure kicker for: {input.path}")

    def send_plan_failed_notif(self, service_kpath):

        v = _ncs.Value((ns_ncs.ncs_plan_state_change, ns_ncs.hash),
                       _ncs.C_XMLBEGIN)
        v2 = _ncs.Value((ns_ncs.ncs_plan_state_change, ns_ncs.hash),
                        _ncs.C_XMLEND)
        xmltag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_plan_state_change)
        start = _ncs.TagValue(xmltag=xmltag, v=v)
        end = _ncs.TagValue(xmltag=xmltag, v=v2)

        service_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_service)
        service_val = _ncs.Value(service_kpath, _ncs.C_OBJECTREF)

        op_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_operation)
        op_val = _ncs.Value(ns_ncs.ncs_plan_state_modified, _ncs.C_ENUM_HASH)

        status_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_status)
        status_val = _ncs.Value(ns_ncs.ncs_plan_failed, _ncs.C_ENUM_HASH)

        notif_values = [_ncs.TagValue(xmltag=service_tag, v=service_val),
                        _ncs.TagValue(xmltag=op_tag, v=op_val),
                        _ncs.TagValue(xmltag=status_tag, v=status_val)]

        final_notif_values = [start] + notif_values + [end]

        self.log.info(f"Sending Notification for Plan : {service_kpath}")

        # Send Notification on 'service-state-changes' stream.
        current_time = datetime.utcnow()
        notif_time = _ncs.DateTime(current_time.year, current_time.month,
                                   current_time.day, current_time.hour,
                                   current_time.minute, current_time.second,
                                   current_time.microsecond, 0, 0)
        _ncs.dp.notification_send(self.cb_sockets['notif_socket'], notif_time, final_notif_values)
