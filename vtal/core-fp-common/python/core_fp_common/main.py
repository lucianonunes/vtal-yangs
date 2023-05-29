from ncs.application import Application
from . import status_codes_util
from . import validate_call_back
from . import device_tree_subscriber
from . import common_utils as common_utils
import ncs
import _ncs


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------
class Main(Application):
    def setup(self):
        self.log.info('Main RUNNING')

        # Status Code Registrations
        self.vp_reg = validate_call_back.ValPointRegistrar(self.log,
                                                           'sc_val_daemon',
                                                           'status-code-validation-py',
                                                           status_codes_util.StatusCodeValidator(
                                                               self.log))

        status_codes_util.validate_status_codes(self)

        # LSA Dispatch Map Registrations
        self.register_action("rfs-dispatch-map-netconf-notification-handler",
                             RfsDMNetconfNotificationHandler)
        self.register_action("sync-dispatch-map", SyncDispatchMap)
        self.dispatch_map_notif_reg = device_tree_subscriber.\
            DispatchMapNotifStreamRegistrar(self.log)
        self.device_tree_sub = device_tree_subscriber.DeviceTreeSubscriber(self)
        self.device_tree_sub.start()

    def teardown(self):
        self.vp_reg.cleanup()
        self.device_tree_sub.stop()
        self.dispatch_map_notif_reg.cleanup()
        self.log.info('Main FINISHED')


class RfsDMNetconfNotificationHandler(ncs.dp.Action):
    """
    Action handler for RFS dispatch map change notification on CFS node
    """
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Received notification on path: {input.path}")
        _ncs.dp.action_set_timeout(uinfo, 7200)
        with ncs.maapi.single_read_trans("", "system") as th:
            event = ncs.maagic.get_node(th, input.path)
            notification = ncs.maagic.cd(event, "../../")
            rfs_node = ncs.maagic.cd(event, "../../../../../ncs:name")
            added_device = []
            deleted_device = []

            for device in notification.data.dispatch_map_events.device:
                if device.operation == "created":
                    added_device.append(tuple((device.name, device.ned_id)))
                if device.operation == "deleted":
                    deleted_device.append(device.name)

            device_tree_subscriber.update_dispatch_map(self, added_device, deleted_device, rfs_node)

            self.log.info(f"Dispatch Map Updated for: {input.path}")


class SyncDispatchMap(ncs.dp.Action):
    """
    Action handler to sync dispatch-map
    """
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info("Start Dispatch Map Sync")
        _ncs.dp.action_set_timeout(uinfo, 7200)
        with ncs.maapi.single_read_trans("", "system") as th:
            root = ncs.maagic.get_root(th)

            try:
                # This action should only be allowed on RFS nodes.
                if device_tree_subscriber.is_dispatch_map_autopopulate_enabled():
                    # First remove extra devices in dispatch map & notify CFS
                    dispatch_map_extras = self.get_dispatch_map_extras(root)
                    # Sync with latest device list
                    added_device = []
                    for device in root.ncs__devices.device:
                        added_device.append(tuple((device.name,
                                            common_utils.get_device_ned_id(device.name))))
                    device_tree_subscriber.update_dispatch_map(self, added_device,
                                                               dispatch_map_extras)
                    device_tree_subscriber.send_dispatch_map_update_notif(self, added_device,
                                                                          dispatch_map_extras)
                    output.success = True
                    output.detail = "Dispatch Map Synced Successfully"
                else:
                    output.success = False
                    output.detail = "Dispatch Map Sync action not allowed on CFS node."
            except Exception as e:
                output.success = False
                output.detail = f"Failed Dispatch Map Sync {str(e)}"

            self.log.info("Done Dispatch Map Sync")

    def get_dispatch_map_extras(self, root):
        dispatch_map_extras = []
        for dm in root.core_fp_common__dispatch_map:
            if dm.device not in root.ncs__devices.device:
                dispatch_map_extras.append(dm.device)
        return dispatch_map_extras
