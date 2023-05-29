# -*- mode: python; python-indent: 4 -*-
from core_fp_common.common_utils import get_local_user
from core_fp_common.cleanup_utils import remove_zombie
import ncs
import _ncs
import traceback

from .flat_L2vpn import FlatL2vpn
from .flat_L3vpn import FlatL3vpn
from .ietf_te import IetfTe
from .sr_te_cfp_rfs_change_handler import update_odn_template_plan, update_policy_template_plan
from .flat_L3vpn_fp_rfs_change_handler import update_L3vpn_plan
from .flat_L2vpn_fp_rfs_change_handler import update_flat_L2VPN_plan
from .ietf_te_fp_rfs_change_handler import update_ietf_plan
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from . import service_alarm_subscriber
from .device_poller import DevicePoller
from . import device_poller as device_poller
from .rfs_path_queue import WorkQueue
from .plan_change_executor import PlanChangeExecutor
from cisco_tsdn_core_fp_common.utils import get_action_timeout
from cisco_tsdn_core_fp_common import utils as utils
from .sr_te_odn import SrTeOdn
from .sr_te_policy import SrTePolicy


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("TSDN Common RUNNING")
        self.register_service("rfs-monitor-path-servicepoint", RfsPathMonitorCallback)
        self.rfs_path_queue = WorkQueue.getInstance()
        self.register_action("rfs-plan-change-netconf-notification-handler",
                             RfsPlanChangeNetconfNotificationHandler,
                             self.rfs_path_queue)
        self.register_action("sync-rfs-plan", RfsPlanSyncHandler)
        self.register_action("trigger-device-poller", TriggerDevicePoller)
        self.register_action("purge-failed-device", PurgeFailedDevice)
        self.register_action("cisco-tsdn-core-fp-common-zombie-cleanup", CleanupZombie)

        # TODO - do not start this thread if we are in LSA setup and this package is on RFS node
        self.plan_change_executor = PlanChangeExecutor(self.log)
        self.plan_change_executor.start()

        # self.cq_alarm_sub = cq_alarm_subscriber.CQAlarmSubscriber(self)
        # self.cq_alarm_sub.start()

        # Service Activation Alarm Subscriber
        self.service_alarm_sub = service_alarm_subscriber.ServiceAlarmSubscriber(self)
        self.service_alarm_sub.start()

        # Set global values
        FlatL2vpn.is_lsa_setup()
        FlatL3vpn.is_lsa_setup()
        IetfTe.is_lsa_setup()
        SrTeOdn.is_lsa_setup()
        SrTePolicy.is_lsa_setup()

        utils.check_if_aa_is_enabled(self.log)

    def teardown(self):
        # self.cq_alarm_sub.stop()
        self.service_alarm_sub.stop()
        self.rfs_path_queue.abort()
        self.log.info("TSDN Common FINISHED")


class RfsPathMonitorCallback(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        self.log.info(f"RFS path monitor for: {kp}, operation: {op}")
        service_id = get_kp_service_id(kp)
        if op == _ncs.dp.NCS_SERVICE_CREATE or op == _ncs.dp.NCS_SERVICE_UPDATE:
            service = root.cisco_tsdn_core_fp_common__rfs_monitor_path[service_id]
            vars = ncs.template.Variables()
            template = ncs.template.Template(service)
            vars.add("MONITOR_PATH", service_id)
            template.apply("rfs-path-monitor-kicker", vars)

        if op == _ncs.dp.NCS_SERVICE_DELETE:
            del root.kicker__kickers.data_kicker["rfs-path-kicker-" + service_id]

        return proplist

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        return proplist


def get_kp_service_id(kp):
    kpath = str(kp)
    service = kpath[kpath.find("{") + 1: len(kpath) - 1]
    return service


class RfsPlanChangeNetconfNotificationHandler(ncs.dp.Action):
    """
    Action handler for RFS path change notification on CFS node
    """

    def init(self, init_args):
        self.rfs_path_queue = init_args

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Received notification on path: {input.path}")
        with ncs.maapi.single_read_trans("", "system") as th:
            event = ncs.maagic.get_node(th, input.path)
            rfs_node = ncs.maagic.cd(event, "../../../../../ncs:name")
            self.rfs_path_queue.put((event.path, rfs_node))
        self.log.info(f"Received notification on path: {input.path} DONE")


class RfsPlanSyncHandler(ncs.dp.Action):
    """
    Action handler for syncing RFS plan change
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        self.log.info(f"Sync RFS plan for service={input.service} "
                      f"device={input.device} service-type={input.service_type} ")
        service_type = input.service_type
        service = input.service
        device = input.device

        if LsaUtils.get_remote_nso(device) is None:
            # this is non-LSA or RFS setup, should return
            output.success = False
            output.detail = "Cannot run this action on RFS node."

        try:
            if service_type == "sr-odn":
                update_odn_template_plan(self.log, service, device)
            elif service_type == "sr-policy":
                update_policy_template_plan(self.log, service, device)
            elif service_type == "flat-l3vpn":
                update_L3vpn_plan(self.log, service, device)
            elif service_type == "rsvp-te":
                update_ietf_plan(self.log, service)
            elif service_type == "flat-l2vpn":
                update_flat_L2VPN_plan(self.log, service, device)
        except Exception as e:
            traceback.print_exc()
            output.success = False
            output.detail = str(e)
        else:
            output.success = True
            output.detail = "Sync Successful"


class TriggerDevicePoller(ncs.dp.Action):
    """
    Action handler to retrigger device poller
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with ncs.maapi.single_read_trans("", "system") as th:
            root = ncs.maagic.get_root(th)
            username = get_local_user()
            if input.device \
                    not in (root.cisco_tsdn_core_fp_common__commit_queue_recovery_data
                            .current_device_poller):
                self.log.info(f"Start device poller on: {input.device}")
                poller = DevicePoller(input.device, username, self.log)
                poller.start()
                self.log.info(f"Started device poller on: {input.device}")
                output.success = True
                output.detail = "Device poller started."
            else:
                output.success = False
                output.detail = "A poller is already running on this device."


class PurgeFailedDevice(ncs.dp.Action):
    """
    Action handler to purge failed device or impacted services
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        with ncs.maapi.single_read_trans("", "system") as th:
            root = ncs.maagic.get_root(th)
            failed_device_path = root.\
                cisco_tsdn_core_fp_common__commit_queue_recovery_data.failed_device
            if input.device in failed_device_path:
                if (len(failed_device_path[input.device].impacted_service_path) == 0
                        or input.force):
                    device_poller.remove_failed_device(input.device)
                    output.success = True
                    output.detail = "Purged failed device entry."
                    return
                elif len(input.impacted_service_path) > 0:
                    impacted_services = []
                    for path in input.impacted_service_path:
                        impacted_services.append(path.service)
                    device_poller.remove_handled_services(input.device, impacted_services)
                    output.success = True
                    output.detail = ("Purged requested impacted_service_path from failed device "
                                     + "entry.")
                    return
                else:
                    output.success = False
                    output.detail = ("Failed device has impacted-services. Either provide "
                                     + "impacted-service-path or use force option to forcefully "
                                     + "remove entire failed-device entry.")
                    return
            else:
                output.success = False
                output.detail = ("No such failed device exists under "
                                 + "commit-queue-recovery-data/failed-device")
                return


class CleanupZombie(ncs.dp.Action):
    """
    Action handler to cleanup zombies
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        zombie_service_xp = input.zombie_path
        force = input.force
        cleanup_log = ["Starting zombie cleanup"]
        self.log.info("Starting tsdn-core-fp-common zombie cleanup for service : "
                      f"{zombie_service_xp}")
        try:
            # Validate
            if not force:
                zombie_kp = f'/ncs:zombies/ncs:service{{"{zombie_service_xp}"}}'
                with ncs.maapi.single_read_trans(uinfo.username, uinfo.context) as th:
                    if th.exists(zombie_kp):
                        zombie = ncs.maagic.get_node(th, zombie_kp)
                        if len(zombie.plan.component) > 0:
                            raise Exception("\nWarning : Zombie references service plan. "
                                            "Removing this zombie can result in an orphaned plan."
                                            "\nUse service level cleanup action to clear service "
                                            "oper-data including plan and zombies."
                                            "\nUse force flag to bypass")
            # Delete zombie
            remove_zombie(self, zombie_service_xp, cleanup_log, uinfo)
            output.success = True
        except Exception as e:
            cleanup_log.append(f"\nZombie cleanup failed : {e}")
            self.log.error(f"Zombie cleanup failed : {e}")
            traceback.print_exc()
            output.success = False
        cleanup_log.append("\nZombie cleanup finished")
        output.detail = "".join(cleanup_log)
        self.log.info("tsdn-core-fp-common zombie cleanup finished")
        return
