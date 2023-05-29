# -*- mode: python; python-indent: 4 -*-
import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
import traceback
from . import constants as const
from . import utils as Utils
from core_fp_common.common_utils import (
    get_local_user, create_cdb_session,
    end_cdb_session, safe_create, set_elem
)
from .device_poller import DevicePoller


class CQAlarmSubscriber(ncs.cdb.OperSubscriber):
    """
    This subscriber subscribes to alarms in the '/alarms/alarm-list/alarm'
    If a service fails in the commit queue,
    it is important that NSO is synced with the non-completed devices before
    further commits on the failed instance or on any shared data is done.
    If for example another service instance does a shared set on the same data
    as the first service instance touched, the reference count will be increased
    but no actual change is pushed to the device(s). This will give a false positive
    that the change is actually deployed in the network. The automatic and manual
    error recovery option will automatically create a queue lock on the involved services
    and devices to prevent such a case.

    1 .CQ Failure persistent or transient
    2. If Persistent:
        Issue sync-from device on receiving CQ alarm.
        User is expected to then correct the service payload and correct the failures.
    3. If Transient:
        Store the failed service and CQ ID for which alarm was generated. (Any subsequent service
        failures until the device is down will get added to this list).
        Start a device poller with a configurable timer to check device connectivity.
        Once device is back up, issue sync-from on device & redeploy all failed services or
        load config+commit zombie services, one after the other & remove from list a.
        If device is not up within poller time, we give up & keep the failed service map as is for
        user to refer to and do manual recovery.
    """

    def init(self):
        self.register("/alarms/alarm-list/alarm")
        # Restart pollers if pollers are eliminated during NSO/python VM restart
        self._restart_pollers()

    def pre_iterate(self):
        return {"cq_alarms": []}

    def should_iterate(self):
        if Utils.is_ha_slave():
            self.log.debug("CQAlarmSubscriber: HA role is slave, skipping iteration")
            return False
        else:
            return True

    def iterate(self, kp, op, oldv, newv, state):
        if (op == ncs.MOP_CREATED) and ("commit-through-queue-failed" in str(kp)):
            self.log.info(f"cq alarm path: {str(kp)}")
            # a0 commit-through-queue-failed /devices/device[name='a0'] 1592279136184
            key = (str(kp[0][0]), str(kp[0][1]), str(kp[0][2]), str(kp[0][3]))
            state["cq_alarms"].append(key)

        return ncs.ITER_CONTINUE

    def should_post_iterate(self, state):
        self.log.debug(f"CQ Alarms received: {state}")
        return not ((state["cq_alarms"] == []))

    def post_iterate(self, state):
        context = "system"
        cq_alarms = state["cq_alarms"]

        with maapi.single_read_trans("", context) as outer:
            root = maagic.get_root(outer)
            username = get_local_user()
            auto_cleanup = root.cisco_tsdn_core_fp_common__commit_queue_recovery_data.auto_cleanup
            for key in cq_alarms:
                self.log.info(f"CQ Alarm handling for: {key}")
                # Get impacted services.
                impacted_services = []
                alarm_text = None
                if (key[0], key[1], key[2], key[3]) in root.al__alarms.alarm_list.alarm:
                    alarm = root.al__alarms.alarm_list.alarm[key[0], key[1], key[2], key[3]]
                    alarm_text = alarm.last_alarm_text
                    for impacted_object in alarm.impacted_objects:
                        impacted_services.append(str(impacted_object))

                self.log.info(f"CQ impacted services: {impacted_services}")

                # Skip unwanted CQ alarms if not related to TSDN services.
                if any(service in str(impacted_services) for service in const.cq_alarm_services):
                    device = key[0]

                    # If auto-cleanup is set to true, impacted zombie services will be redeployed
                    # and removed from failure list
                    if auto_cleanup:
                        self._handle_auto_cleanup(username, impacted_services)

                    # Transient Failure
                    if any(error_type in str(alarm_text).lower() for error_type
                           in const.transient_error_message):
                        # Start poller on failed device if one doesn't exist already
                        poll_enabled = (root.cisco_tsdn_core_fp_common__commit_queue_recovery_data.
                                        enable_polling_recovery)
                        self.log.info(f"Is poller recovery enabled: {poll_enabled}")
                        if poll_enabled:
                            self._update_failed_device_list(key, impacted_services)
                            if device not \
                                    in (root.cisco_tsdn_core_fp_common__commit_queue_recovery_data
                                        .current_device_poller):
                                self.log.info(f"Transient Failure: {alarm_text}: "
                                              f"trigger device poller on: {device}")
                                poller = DevicePoller(device, username, self.log)
                                poller.start()
                            else:
                                self.log.info("Transient Failure device poller already "
                                              "exists on" + f": {poll_enabled}")
                    else:
                        # Persistent Failure, need user intervention
                        # Issue sync from on failing device & depend on user to manually recover
                        # TODO: If there are pending CQs in line, sync-from will not be success,
                        # we will have to keep retrying ???
                        # Cannot perform action since the device has active items in the \
                        # commit-queue.
                        self.log.info(f"Persistent Failure: {alarm_text}: on device: {device}")
                        # sync_from_output = Utils.device_sync_from(username, device)
                        # self.log.info("Persistent Failure, Sync from result: {} for device: {}".\
                        #                                format(sync_from_output.result, device))
                self.log.info(f"CQ Alarm handled for: {key}")

    def _update_failed_device_list(self, alarm_key, impacted_services):
        sock_cdb_oper = create_cdb_session()
        try:
            failed_device_kp = const.join([const.failed_device, "{", alarm_key[0], "}"])
            safe_create(sock_cdb_oper, failed_device_kp)
            for service in impacted_services:
                # Only save tsdn related impacted objects
                if any(allowed_path in str(service) for allowed_path in const.cq_alarm_services):
                    service_kp = const.join([failed_device_kp, const.impacted_service,
                                             '{"', service, '"}'])
                    safe_create(sock_cdb_oper, service_kp)
                    failed_cq_kp = const.join([service_kp, const.failed_cq_id])
                    set_elem(sock_cdb_oper, alarm_key[3], failed_cq_kp)

        except Exception:
            traceback.print_exc()

        # End CDB session
        end_cdb_session(sock_cdb_oper)

    def _restart_pollers(self):
        with maapi.single_read_trans("", "system") as th:
            root = maagic.get_root(th)
            username = get_local_user()
            for (device) in root.cisco_tsdn_core_fp_common__commit_queue_recovery_data.\
                    current_device_poller:
                poller = DevicePoller(device.name, username, self.log)
                poller.start()

    def _handle_auto_cleanup(self, username, impacted_services):
        with maapi.single_read_trans(username, "system") as th:
            for service_kp in list(impacted_services):
                if "zombies" in service_kp:
                    zombie_service = maagic.get_node(th, service_kp)
                    zombie_service.reactive_re_deploy()
                    impacted_services.remove(service_kp)
                    self.log.info(f"Auto cleaned up zombie: {service_kp}")
