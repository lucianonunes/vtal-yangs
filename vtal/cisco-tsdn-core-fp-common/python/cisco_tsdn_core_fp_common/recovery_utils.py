# -*- mode: python; python-indent: 4 -*-
import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
import time
import traceback
from . import constants as const
from collections import namedtuple
from cisco_tsdn_core_fp_common.utils import get_action_timeout


def get_cq_error_recovery_paths(self, root, device, service_path, zombie_path):
    cq_error_recovery_paths = []
    failed_device_path = root.cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device
    if device in failed_device_path:
        cq_error_recovery_paths.append((device,
                                        get_cq_error_recovery_service_path(device, service_path,
                                                                           failed_device_path)))
        cq_error_recovery_paths.append((device,
                                        get_cq_error_recovery_zombie_path(device, zombie_path,
                                                                          failed_device_path)))

    return cq_error_recovery_paths


def get_cq_error_recovery_service_path(device, service_path, failed_device_path):
    cq_err_serv_path = None
    if device in failed_device_path:
        cq_err_serv_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                            f"failed-device{{{device}}}/"
                            f'impacted-service-path{{"{service_path}"}}')
    return cq_err_serv_path


def get_cq_error_recovery_zombie_path(device, zombie_path, failed_device_path):
    cq_err_zombie_path = None
    if device in failed_device_path:
        zombie_path = f"/ncs:zombies/service{{{zombie_path}}}"
        cq_err_zombie_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                              f"failed-device{{{device}}}/"
                              f'impacted-service-path{{"{zombie_path}"}}')
    return cq_err_zombie_path


def remove_cq_recovery_data(self, cq_error_path, device, cleanup_log, uinfo):
    """Remove cq recovery data for this service if any"""
    with maapi.single_write_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
        if cq_error_path is not None and th.exists(cq_error_path):
            cleanup_log.append(f"\n Removing cq_error_path: {cq_error_path}")
            th.delete(cq_error_path)
            failed_device_path = ("/cisco-tsdn-core-fp-common:commit-queue-recovery-data/"
                                  f"failed-device{{{device}}}")
            if th.exists(failed_device_path):
                failed_device = maagic.get_node(th, failed_device_path)
                if len(failed_device.impacted_service_path) == 0:
                    th.delete(failed_device_path)
        th.apply()
        cleanup_log.append(f"\n Removed cq_error_path: {cq_error_path}")


# Methods for Recovery Action
def recover_service(self, root, th, uinfo, internal_plan, device,
                    internal_zombie_path, internal_service_path, recovery_log,
                    device_recovery_error, service_name, sync_direction):
    sync_output = sync_retries(self, root, device, sync_direction, uinfo)

    if sync_output.result:
        cq_err_path = None
        failed_device_path = root.\
            cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device

        if root.ncs__zombies.service.exists(internal_zombie_path):
            try:
                handle_delete_failure(self, uinfo.username, device,
                                      root.ncs__zombies.service[internal_zombie_path])
                recovery_log.append("\nSuccessful recovery for delete failure on device: "
                                    f"{device}")
                cq_err_zombie_path = get_cq_error_recovery_zombie_path(device,
                                                                       internal_zombie_path,
                                                                       failed_device_path)
                remove_cq_recovery_data(self, cq_err_zombie_path, device, recovery_log, uinfo)
                # To handle case where delete recovery is executed on a create failed service
                # In this case there will be an entry for create failure that needs removal
                cq_err_path = get_cq_error_recovery_service_path(device,
                                                                 internal_service_path,
                                                                 failed_device_path)
                remove_cq_recovery_data(self, cq_err_path, device, recovery_log, uinfo)
            except Exception as e:
                traceback.print_exc()
                recovery_log.append("\nFailed to recover delete failure on device: "
                                    f"{device}")
                device_recovery_error[device] = str(e)
        else:
            if any(error_type in str(internal_plan.error_info.message).lower()
                   for error_type in const.transient_error_message):
                try:
                    if th.exists(internal_service_path):
                        service = maagic.get_node(th, internal_service_path)
                        handle_create_failure(self, service)
                        recovery_log.append("\nSuccessful recovery for create failure on device: "
                                            f"{device}")
                        cq_err_path = get_cq_error_recovery_service_path(device,
                                                                         internal_service_path,
                                                                         failed_device_path)
                        remove_cq_recovery_data(self, cq_err_path, device, recovery_log, uinfo)
                except Exception as e:
                    traceback.print_exc()
                    recovery_log.append("\nFailed to recover create failure on device: "
                                        f"{device}\n")
                    device_recovery_error[device] = str(e)
            else:
                device_recovery_error[device] = (f"Error for device: {device} in "
                                                 f"service: {service_name} is not transient.")
    else:
        recovery_log.append(f"\nWARNING: Cannot Recover services on device: {device}, "
                            f"{sync_direction} failed with: {sync_output.info}\n")
        device_recovery_error[device] = sync_output.info


def sync_retries(self, root, device_name, sync_direction, uinfo):
    sync_output = None
    sync_action = None

    device = root.ncs__devices.device[device_name]

    if sync_direction == "sync-to":
        sync_action = device.sync_to
    else:
        sync_action = device.sync_from

    retries = 5
    retry_delay_in_sec = 3

    # Calculate wait-for-lock timeout with some buffer as we don't want to be larger
    # than the action timeout itself
    wait_for_lock_timeout = \
        (get_action_timeout(self, uinfo.username) - (retries * (retry_delay_in_sec * 2))) / retries

    # We may get a float which is not valid input for input for action timeouts, as such we convert
    # to an int()
    wait_for_lock_timeout = int(wait_for_lock_timeout)

    if wait_for_lock_timeout < 0:
        wait_for_lock_timeout = 0

    sync_action_input = sync_action.get_input()
    check_sync_input = device.check_sync.get_input()

    for input in [check_sync_input, sync_action_input]:
        input.wait_for_lock.create()
        input.wait_for_lock.timeout = wait_for_lock_timeout

    # We retry sync on failure just in case device poller sync is running
    # at the same time and device is locked.

    for _ in range(retries - 1):
        if device.check_sync(check_sync_input).result == "in-sync":
            output = namedtuple('output', ('result'))
            output.result = True
            return output

        self.log.info(f"Trying to fetch-ssh-keys for recovery action on device: {device_name}")
        fetch_keys_output = device.ssh.fetch_host_keys()

        if fetch_keys_output.result != "failed":
            self.log.info(f"Trying {sync_direction} for recovery action on device: {device_name}")
            sync_output = sync_action(sync_action_input)

            if sync_output.result:
                return sync_output
            else:
                time.sleep(retry_delay_in_sec)
        else:
            time.sleep(retry_delay_in_sec)

    # try to sync one more time to get result.info
    if sync_output is None:
        sync_output = sync_action()

    return sync_output


def handle_delete_failure(self, username, device, service):
    load_config_output = zombie_load_config_commit(self, username, device, service)
    self.log.info(f"zombie load_config_output is: {load_config_output}")
    if ((load_config_output == "skip") or (load_config_output is None)
            or (load_config_output is not None and load_config_output.result == "true")):
        service.reactive_re_deploy()
        # RT-43080: side-effect-queue is not getting cleaned up on recovery.
        # Should be harmless for service lifecycle &
        # platform will eventually clean it up based on automatic-purge settings.
        self.log.info(f"Redeployed Zombie Service: {service._path}")
    else:
        raise Exception(f"Cannot load device config on zombie, error: {load_config_output.result}")


def zombie_load_config_commit(self, username, device, service):
    with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
        load_config_output = None
        service = maagic.get_node(th, service._path)

        # We want to bypass commit-queue and wait for device to enter critical section to avoid
        # race conditions when running error-recovery for multiple internal sites using the same PE.
        # Otherwise, we'll run into failed commits due to commit-queue conflicts
        commit_params = ncs.maapi.CommitParams()
        commit_params.commit_queue_bypass()
        commit_params.wait_device([device])

        for cq in service.commit_queue.queue_item:
            if device in cq.failed_device:
                try:
                    load_config_output = cq.failed_device[device].load_device_config()
                except Exception as e:
                    # This can happen in 2 cases. 1. create of service had failed so nothing was
                    # pushed to the device. Now when the service is deleted when the device is
                    # still down, there should ideally be nothing to remove from the device.
                    # But with CQ, this behavior is a little different,in that it tries to delete
                    # config that create was expected to push & will throw a "missing element"
                    # exception as no such config exists on the device.
                    # We want to skip load config for such cases.
                    # 2. This can also occur when an OOB change has been done on the device for the
                    # config that was pushed by NSO. OOB should not be allowed as it will leave CDB
                    # and network out of sync.
                    if "missing element" in str(e):
                        load_config_output = "skip"
                    else:
                        raise e

        th.apply_params(params=commit_params)

        return load_config_output


def handle_create_failure(self, service):
    redep_options = service.re_deploy.get_input()
    redep_options.reconcile.create()
    service.re_deploy(redep_options)
    self.log.info(f"Redeployed Service: {service._path}")
