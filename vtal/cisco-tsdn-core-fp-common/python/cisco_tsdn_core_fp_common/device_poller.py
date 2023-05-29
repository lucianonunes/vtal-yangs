from threading import Thread
import ncs.maapi as maapi
import ncs.maagic as maagic
import traceback
import time
from . import utils as Utils
from . import constants as const
from cisco_tsdn_core_fp_common import recovery_utils as RecoveryUtils
from core_fp_common.common_utils import (
    create_cdb_session, end_cdb_session,
    safe_create, set_elem, safe_delete
)


class DevicePoller(Thread):
    """
    This thread is started by CQ alarm listener for the device which is not reachable.
    """

    def __init__(self, device, username, log):
        """ Constructor. """

        Thread.__init__(self)
        self.device = device
        self.log = log
        self.username = username

    def run(self):
        device = self.device
        username = self.username
        try:
            with maapi.single_read_trans(username, "system") as th:
                root = maagic.get_root(th)
                (_, _, _, sync_direction) = self.get_poller_config(root)
                sync_output = Utils.device_sync(username, device, sync_direction)
                self.log.info(f"{sync_direction} result for {device}: {sync_output.result}")
                failed_device_path = root.\
                    cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device
                # Sync did not succeed, start polling
                if not sync_output.result:
                    if device in failed_device_path:
                        # Update currently polling list
                        set_current_poller(device)
                        poll_timed_out = self.poll_failed_device(root, username, device,
                                                                 failed_device_path)
                        remove_current_poller(device)
                        self.log.info(f"Removed device poller on: {device}")

                        # Either poll max time has reached or sync was success
                        if poll_timed_out:
                            self.log.info(f"Disabling polling on device connection for: {device}")
                        else:
                            self.handle_post_sync_recovery(device, username,
                                                           failed_device_path, th)
                else:
                    self.handle_post_sync_recovery(device, username, failed_device_path, th)

        except Exception:
            traceback.print_exc()

    def get_poller_config(self, root):
        poll_config = root.\
            cisco_tsdn_core_fp_common__commit_queue_recovery_data.device_poller_configurations
        poll_time = poll_config.poll_time
        poll_wait_time = poll_config.poll_wait_time
        poll_time_multiplier = poll_config.poll_time_multiplier
        sync_direction = poll_config.sync_direction
        return (poll_time, poll_wait_time, poll_time_multiplier, sync_direction)

    def poll_failed_device(self, root, username, device, failed_device_path):
        (poll_time, poll_wait_time, poll_time_multiplier, sync_direction) = self.\
            get_poller_config(root)
        poll = True
        # Monitor for (poll_time*poll_time_multiplier minutes)
        poll_timeout = time.time() + 60 * poll_time * poll_time_multiplier
        self.log.info(f"Starting device connection poller for {device}")

        while poll and time.time() < poll_timeout:
            self.log.debug(f"Wait {poll_wait_time}s before {sync_direction} retry: {device}")
            time.sleep(poll_wait_time)
            self.log.debug(f"Retry {sync_direction} on device: {device}")

            sync_output = Utils.device_sync(username, device, sync_direction)
            self.log.info(f"{sync_direction} result for {device}: {sync_output.result}")
            if not sync_output.result:
                # This is to ensure poller stops running
                # if all services are manually cleaned up externally.
                if (device in failed_device_path and
                        len(failed_device_path[device].impacted_service_path) > 0):
                    poll = True
                else:
                    self.log.info(f"No more services to monitor for: {device}")
                    poll = False
            else:
                poll = False
                self.log.info(f"Successfully executed {sync_direction} on device: {device}")
        return poll

    def handle_post_sync_recovery(self, device, username, failed_device_path, th):
        handled_services = []
        # Redeploy service/zombie
        if device in failed_device_path:
            for path in failed_device_path[device].impacted_service_path:
                service_path = path.service
                self.log.info(f"Recover service: {service_path}")
                try:
                    # This node could have been removed by cleanup action or delete
                    if th.exists(service_path):
                        service = maagic.get_node(th, service_path)
                        if "zombies" in str(service_path):
                            RecoveryUtils.handle_delete_failure(self, username, device, service)
                        else:
                            RecoveryUtils.handle_create_failure(self, service)
                    # remove handled service from the failed list
                    handled_services.append(service_path)
                except Exception as e:
                    traceback.print_exc()
                    set_impacted_service_recovery_result(device, service_path, e)

            self.log.info(f"Remove handled services: {handled_services}")
            remove_handled_services(device, handled_services)
            self.log.info(f"Removed handled services: {handled_services}")

            # if no more services to be handled, remove the device entry from poller.
            if len(failed_device_path[device].impacted_service_path) == 0:
                remove_failed_device(device)
                self.log.info(f"Removed failed device: {device}")


def set_current_poller(device):
    sock_cdb_oper = create_cdb_session()
    try:
        current_poller_kp = const.join([const.current_poller, "{", device, "}"])
        safe_create(sock_cdb_oper, current_poller_kp)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)


def remove_current_poller(device):
    sock_cdb_oper = create_cdb_session()
    try:
        current_poller_kp = const.join([const.current_poller, "{", device, "}"])
        safe_delete(sock_cdb_oper, current_poller_kp)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)


def remove_handled_services(device, handled_services):
    sock_cdb_oper = create_cdb_session()
    try:
        failed_device_kp = const.join([const.failed_device, "{", device, "}"])
        for service in handled_services:
            service_kp = const.join([failed_device_kp, const.impacted_service,
                                     '{"', service, '"}'])
            safe_delete(sock_cdb_oper, service_kp)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)


def set_impacted_service_recovery_result(device, impacted_service, error):
    sock_cdb_oper = create_cdb_session()
    try:
        failed_device_kp = const.join([const.failed_device, "{", device, "}"])
        service_kp = const.join([failed_device_kp, const.impacted_service,
                                 '{"', impacted_service, '"}'])
        poller_recovery_result_kp = const.join([service_kp, const.poller_recovery_result])
        set_elem(sock_cdb_oper, str(error), poller_recovery_result_kp)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)


def remove_failed_device(device):
    sock_cdb_oper = create_cdb_session()
    try:
        failed_device_kp = const.join([const.failed_device, "{", device, "}"])
        safe_delete(sock_cdb_oper, failed_device_kp)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)
