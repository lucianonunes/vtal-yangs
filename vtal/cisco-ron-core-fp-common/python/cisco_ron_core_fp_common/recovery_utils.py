# -*- mode: python; python-indent: 4 -*-
import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
import time
import traceback
from .constants import StrConstants as const


## Methods for Recovery Action
def recover_service(self, root, th, uinfo, internal_plan, device, internal_zombie_path,
                    internal_service_path, recovery_log, device_recovery_error,
                    service_name, sync_direction):
    if any(
        error_type in str(internal_plan.error_info.message).lower()
        for error_type in const.transient_error_message
    ):
        ## Sync-direction on device
        sync_output = sync_retries(self, root, device, sync_direction)
        if sync_output.result:
            # if zombie exists
            if root.ncs__zombies.service.exists(internal_zombie_path):
                try:
                    handle_delete_failure(self, uinfo.username, device,
                                          root.ncs__zombies.service[internal_zombie_path])
                    recovery_log.append("\nRecovered delete failure on {}".format(device))
                except Exception as e:
                    traceback.print_exc()
                    recovery_log.append(
                        "\nFailed to recovere delete failure on {}".format(device)
                    )
                    device_recovery_error[device] = str(e)
            else:
                try:
                    if th.exists(internal_service_path):
                        service = maagic.get_node(th, internal_service_path)
                        handle_create_failure(self, service)
                        recovery_log.append("\nRecovered create failure on {}".format(device))
                except Exception as e:
                    traceback.print_exc()
                    recovery_log.append(
                        "\nFailed to recover create failure on {}\n".format(device)
                    )
                    device_recovery_error[device] = str(e)
        else:
            recovery_log.append(
                "\nCannot Recover services on device: %s, "
                "%s failed with: %s\n" % (device, sync_direction, sync_output.info)
            )
            device_recovery_error[device] = sync_output.info
    else:
        recovery_log.append("\nError for device: {} in service: {} is not transient."
                            .format(device, service_name))


def sync_retries(self, root, device, sync_direction):
    sync_output = None
    ## We retry sync on failure just in case device poller sync is running
    ## at the same time and device is locked.
    for attempt in range(5):
        self.log.info(
            "Trying {} for recovery action on device: {}".format(sync_direction, device)
        )
        action_path = None
        if sync_direction == "sync-to":
            action_path = root.ncs__devices.device[device].sync_to
        else:
            action_path = root.ncs__devices.device[device].sync_from
        sync_output = action_path()
        if sync_output.result:
            return sync_output
        else:
            ## Sync on device, wait for 3s between each retry
            time.sleep(3)
    return sync_output


def handle_delete_failure(self, username, device, service):
    load_config_output = zombie_load_config_commit(self, username, device, service._path)
    self.log.info("zombie load_config_output is: {}".format(load_config_output))
    if (
        (load_config_output == "skip")
        or (load_config_output is None)
        or (load_config_output is not None and load_config_output.result == "true")
    ):
        service.reactive_re_deploy()
        ## RT-43080: side-effect-queue is not getting cleaned up on recovery.
        ## Should be harmless for service lifecycle &
        ## platform will eventually clean it up based on automatic-purge settings.
        self.log.info("Redeployed Zombie Service: {}".format(service._path))
    else:
        raise Exception(
            "Cannot load device config on zombie, error: {}".format(
                load_config_output.result
            )
        )


def zombie_load_config_commit(self, username, device, path):
    with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
        load_config_output = None
        service = maagic.get_node(th, path)
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
        th.apply()
        return load_config_output


def handle_create_failure(self, service):
    redep_options = service.re_deploy.get_input()
    redep_options.reconcile.create()
    service.re_deploy(redep_options)
    self.log.info("Redeployed Service: {}".format(service._path))
