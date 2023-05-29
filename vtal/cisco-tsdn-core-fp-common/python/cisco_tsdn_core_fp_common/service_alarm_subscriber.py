# -*- mode: python; python-indent: 4 -*-
import _ncs
import ncs
import ncs.maagic as maagic
import ncs.maapi as maapi
import socket
import traceback

from core_fp_plan_notif_generator.PlanNotif import get_plan_change_notif, send_notification
from . import utils as Utils


class ServiceAlarmSubscriber(ncs.cdb.OperSubscriber):
    """
    This subscriber subscribes to alarms in the '/alarms/alarm-list/alarm' and updates NB plan
    when service-activation-failure alarm is detected
    """

    daemon_ctx = None
    notif_ctx = None
    s = None

    def init(self):
        self.register("/alarms/alarm-list/alarm")
        # Only support TSDN Services
        self.service_types = ["l2vpn-ntw:l2vpn-ntw", "l3vpn-ntw:l3vpn-ntw",
                              "cisco-flat-L2vpn-fp:flat-L2vpn", "cisco-flat-L3vpn-fp:flat-L3vpn",
                              "cisco-sr-te-cfp:sr-te", "te:te"]
        self.s = socket.socket()
        self.daemon_ctx = _ncs.dp.init_daemon('send_plan_notif_daemon')
        _ncs.dp.connect(dx=self.daemon_ctx, sock=self.s, type=_ncs.dp.CONTROL_SOCKET,
                        ip='127.0.0.1', port=_ncs.PORT)
        self.notif_ctx = _ncs.dp.register_notification_stream(self.daemon_ctx, None, self.s,
                                                              "service-state-changes")

    def stop(self):
        try:
            self.s.close()
        except Exception as e:
            traceback.print_exc()
            self.log.error(f"Exception while closing socket: {str(e)} ")
        _ncs.dp.release_daemon(self.daemon_ctx)
        super(ServiceAlarmSubscriber, self).stop()

    def pre_iterate(self):
        return {"service_alarms": []}

    def should_iterate(self):
        if Utils.is_ha_slave():
            self.log.debug("ValidationAlarmSubscriber: HA role is slave, skipping iteration")
            return False
        else:
            return True

    def iterate(self, kp, op, oldv, newv, state):
        if (op == ncs.MOP_CREATED or op == ncs.MOP_MODIFIED) \
                and ("service-activation-failure" in str(kp)):
            alarm_key = (str(kp[0][0]), str(kp[0][1]), str(kp[0][2]), str(kp[0][3]))
            # Only handle alarm if its related to TSDN CFP
            if any(service_type in alarm_key[2] for service_type in self.service_types):
                state["service_alarms"].append(alarm_key)
        return ncs.ITER_CONTINUE

    def should_post_iterate(self, state):
        self.log.info(f"Service alarms: {state}")
        return not state["service_alarms"] == []

    def post_iterate(self, state):
        service_alarms = state["service_alarms"]

        with maapi.single_write_trans("", "system", db=ncs.OPERATIONAL) as th:
            # Handle service alarms
            for key in service_alarms:
                # Convert from xpath to keypath
                service_kp = _ncs.maapi.xpath2kpath(th.maapi.msock, key[2])
                # Get service plan
                service_plan = None
                try:
                    plan_location_xp = maagic.get_node(th, f"{service_kp}/plan-location")
                    # Convert from xpath to keypath
                    plan_location_kp = _ncs.maapi.xpath2kpath(th.maapi.msock,
                                                              str(plan_location_xp))
                    service_plan = maagic.get_node(th, str(plan_location_kp)).plan
                except Exception as e:
                    self.log.error(f"Service or service plan does not exist : {type(e)} {e}")

                # Purge alarm and get error
                alarm_text = self._purge_alarm_and_get_error(key, th)

                # Update plan with alarm error message if plan exists
                if service_plan is not None:
                    service_plan.failed.create()
                    service_plan.error_info.create()
                    service_plan.error_info.message = f"Service Alarm : {alarm_text}"
                    # Set plan self ready to failed
                    service_plan.component[("ncs:self", "self")].\
                        state["ncs:ready"].status = "failed"
                    # Create a service state change notification
                    notif = get_plan_change_notif(service_kp, "modified", "failed")
                    self.send_plan_change_notif(notif, service_kp)

            # Apply transaction
            th.apply()

    def _purge_alarm_and_get_error(self, key, trans):
        self.log.info(f"Received alarm: {key[2]}")
        try:
            root = maagic.get_root(trans)
            if (key[0], key[1], key[2], key[3]) in root.al__alarms.alarm_list.alarm:
                alarm = root.al__alarms.alarm_list.alarm[key[0], key[1], key[2], key[3]]
                self.log.info(f"alarm text: {alarm.last_alarm_text}")
                alarm_text = alarm.last_alarm_text
                # Purge Alarm to regenerate alarms for redeployed service
                alarm.purge()
                self.log.info(f"Purged alarm: {key[2]}")
                return alarm_text
        except Exception as e:
            self.log.error(f"Purge alarm for {{{key[2]}}} failed : {e}")
            return None

    def send_plan_change_notif(self, notif, service_kp):
        # Send Notification on 'service-state-changes' stream.
        self.log.info(f"Sending Notification for Plan : {service_kp}")
        send_notification(self.notif_ctx, notif)
