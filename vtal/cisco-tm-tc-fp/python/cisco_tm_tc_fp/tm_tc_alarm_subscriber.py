import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from . import utils
from . import tm_tc_internal_handler


class TMTCAlarmSubscriber(ncs.cdb.OperSubscriber):
    """
    This subscriber subscribes to alarms in '/alarms/alarm-list/alarm'
    and writes to internal tm-tc-oper-data for failures.
    """

    def init(self):
        self.register('/alarms/alarm-list/alarm')

    def pre_iterate(self):
        return {"tm_tc_alarms": [],
                "device_alarms": [] }

    def should_iterate(self):
        # For HA check if current host is master so we can write
        if utils.is_ha_slave():
            self.log.debug("TMTCAlarmSubscriber: HA role is slave, skipping iteration")
            return False
        return True

    def iterate(self, kp, op, oldv, newv, state):
        if op == ncs.MOP_CREATED and ("service-activation-failure" in str(kp) and \
                 "cisco-tm-tc-fp-internal:tm-tc" in str(kp)):
            key = (str(kp[0][0]), str(kp[0][1]), str(kp[0][2]), str(kp[0][3]))
            state["tm_tc_alarms"].append(key)

        ## Purge all device connection-failure alarms
        if (op == ncs.MOP_CREATED) and ("connection-failure" in str(kp)):
            self.log.info("Device alarm path: {}".format(str(kp)))
            key = (str(kp[0][0]), str(kp[0][1]), str(kp[0][2]), str(kp[0][3]))
            state["device_alarms"].append(key)

        return ncs.ITER_CONTINUE

    def should_post_iterate(self, state):
        return not(state["tm_tc_alarms"] == [] and state["device_alarms"] == [])

    def post_iterate(self, state):
        tm_tc_alarms = state["tm_tc_alarms"]
        device_alarms = state["device_alarms"]
        with maapi.single_read_trans("", "system") as th:
            root = maagic.get_root(th)
            username = root.cisco_tm_tc_fp__cfp_configurations.local_user

            ## PURGE DEVICE CONNECTION FAILURE ALARMS.
            ## If these are not purged, subsequent alarms on device connection doesn't get generated
            for key in device_alarms:
                self._purge_alarm_and_get_error(key, th)

            # Handle TMTC service alarms
            for key in tm_tc_alarms:
                alarm_text = self._purge_alarm_and_get_error(key, th)
                service_path = key[2]
                if "zombies" not in service_path:
                    service_name = service_path[service_path.find("[name=\"") + len("[name=\"") \
                                                :service_path.rfind("\"][")]
                    node = \
                        service_path[service_path.find("[node-name=\"") + len("[node-name=\"") \
                                     :service_path.rfind("\"]")]
                if "zombies" in service_path:
                    service_name = service_path[service_path.find("[name=\'") + len("[name=\'") \
                                                :service_path.rfind("\'][")]
                    node = \
                        service_path[service_path.find("[node-name=\'") + len("[node-name=\'") \
                                     :service_path.rfind("\']")]

                tm_tc_service = root.cisco_tm_tc_fp__tm_tc
                tm_tc_service_plan = root.cisco_tm_tc_fp__tm_tc_plan
                auto_cleanup = root.cisco_tm_tc_fp__cfp_configurations.auto_cleanup

                # no oper-data update for alarms when autocleanup of entire service is requested
                if service_name in tm_tc_service_plan and not auto_cleanup:
                    status_message = "ALARM: {}".format(alarm_text)
                    tm_tc_internal_handler.set_oper_data(service_name, node, status_message,
                                                         self.log)

                # Check if external service exists, if not, call cleanup action
                if service_name not in tm_tc_service and auto_cleanup:
                    self._call_cleanup_action(service_name, node, root)
                elif service_name in tm_tc_service:
                    node_list = tm_tc_service[service_name].node
                    if node not in node_list and auto_cleanup:
                        self._call_cleanup_action(service_name, node, root)

    def _purge_alarm_and_get_error(self, key, th):
        self.log.info("Received alarm: {}".format(key[2]))
        root = maagic.get_root(th)
        if (key[0], key[1], key[2], key[3]) in root.al__alarms.alarm_list.alarm:
            alarm = root.al__alarms.alarm_list.alarm[key[0], key[1], key[2], key[3]]
            self.log.info("alarm text: {}".format(alarm.last_alarm_text))
            alarm_text = alarm.last_alarm_text
            ## Purge Alarm to regenerate alarms for redeployed service
            alarm.purge()
            self.log.info("Purged alarm: %s" % key[2])
            return alarm_text

    def _call_cleanup_action(self, service, node, root):
        self.log.info("Running cleanup action due to alarm for: {} {}".format(service, node))
        cleanup = root.cisco_tm_tc_fp__tm_tc_actions.cleanup
        input = cleanup.get_input()
        input.service = service
        input.device = node
        output = cleanup(input)
        self.log.info("Cleanup action result for {} {} : {} {}" \
                      .format(service, node, output.success, output.detail))
