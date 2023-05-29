# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
import ncs.maagic as maagic
import ncs.maapi as maapi
from ncs.dp import StateManager
from ncs.dp import Daemon
from datetime import datetime
from .coreFpCommon_ns import ns as common_ns
import traceback

dispatchMapNotifCtx = None


class DeviceTreeSubscriber(ncs.cdb.Subscriber):
    """
    This subscriber subscribes to changes in the '/devices/device'
    tree and updates dispatch-map on addition/deletion of device.
    """
    def init(self):
        self.register('/ncs:devices/ncs:device/ncs:name', iter_flags=ncs.ITER_WANT_ANCESTOR_DELETE)
        self.register('/ncs:devices/ncs:device/ncs:device-type')

    def pre_iterate(self):
        return {"added": [],
                "deleted": []}

    def should_iterate(self):
        if is_ha_slave():
            self.log.debug("DeviceTreeSubscriber: HA role is slave, skipping iteration")
            return False
        else:
            if is_dispatch_map_autopopulate_enabled():
                return True
            else:
                self.log.debug("DeviceTreeSubscriber: Dispatch Map Auto Populate disabled for CFS.")
                return False

    def iterate(self, kp, op, oldv, newv, state):
        # kp: /ncs:devices/device{xyz} Op: MOP_DELETED
        if str(kp[1]) == 'device' and str(kp[2]) == 'devices':
            key = kp[0][0]
            if op == ncs.MOP_DELETED:
                state["deleted"].append(key)
                return ncs.ITER_CONTINUE
        # kp: /ncs:devices/device{xyz}/device-type/cli/ned-id Op: MOP_VALUE_SET
        elif op == ncs.MOP_VALUE_SET and str(kp[0]) == "ned-id":
            key = kp[3][0]
            ned_id = str(newv) + ":" + str(newv)
            state["added"].append(tuple((key, ned_id)))
            return ncs.ITER_CONTINUE
        return ncs.ITER_RECURSE

    def should_post_iterate(self, state):
        self.log.info(f"Updated devices: {state}")
        return not (state["added"] == [] and state["deleted"] == [])

    def post_iterate(self, state):
        # Update local RFS map & send CFS notification.
        update_dispatch_map(self, state["added"], state["deleted"])
        self.log.info("Local Dispatch Map updated")
        send_dispatch_map_update_notif(self, state["added"], state["deleted"])
        self.log.info("Dispatch Map update notified")


def is_ha_slave():
    with maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as trans:
        if trans.exists("/tfnm:ncs-state/tfnm:ha"):
            mode = str(maagic.get_node(trans,
                                       '/tfnm:ncs-state/tfnm:ha/tfnm:mode'))
            return (mode != 'master')
        return False


def is_dispatch_map_autopopulate_enabled():
    with maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
        root = ncs.maagic.get_root(trans)
        return root.core_fp_common__auto_populate_dispatch_map


def update_dispatch_map(self, added_device, deleted_device=[], rfs_node=None):
    # Add devices
    with maapi.single_write_trans("", "system", db=ncs.RUNNING) as th:
        root = ncs.maagic.get_root(th)
        for device, ned_id in added_device:
            if th.exists(f'/dispatch-map{{{device}}}') and \
                    rfs_node is not None and \
                    root.core_fp_common__dispatch_map[device].rfs_node != rfs_node:
                self.log.error(f"Device {device} from rfs node "
                               f"{root.core_fp_common__dispatch_map[device].rfs_node}  "
                               f"is being overridden on dispatch map with device from {rfs_node}")
            dispatch_entry = root.core_fp_common__dispatch_map.create(device)
            dispatch_entry.ned_id = ned_id
            dispatch_entry.rfs_node = rfs_node
        th.apply()
    # Remove devices
    try:
        for device in deleted_device:
            with maapi.single_write_trans("", "system", db=ncs.RUNNING) as th:
                root = ncs.maagic.get_root(th)
                if th.exists(f'/dispatch-map{{{device}}}') and \
                        root.core_fp_common__dispatch_map[device].rfs_node != rfs_node:
                    self.log.error(f"Ignoring Device {device} deletion from rfs node "
                                   f"{rfs_node}")
                else:
                    del root.core_fp_common__dispatch_map[device]
                th.apply()
    except Exception:
        # Ignore if deletion fails, mostly due to:
        # _ncs.error.Error: bad keyref (14): illegal reference
        traceback.print_exc()


class DispatchMapNotifStreamSM(StateManager):
    def __init__(self, log):
        self.log = log
        StateManager.__init__(self, log)

    def setup(self, state, previous_state):
        global dispatchMapNotifCtx
        dispatchMapNotifCtx = self.register_notification_stream(
            state, None, ncs.dp.take_worker_socket(state, "dispatch-map-events-ws-name",
                                                   "dispatch-map-events-ws-key"),
            'dispatch-map-events')

    def teardown(self, state, finished):
        ncs.dp.return_worker_socket(state, "dispatch-map-events-ws-key")


class DispatchMapNotifStreamRegistrar(object):
    def __init__(self, log):
        sm = DispatchMapNotifStreamSM(log)
        self.notif_stream_daemon = Daemon('dispatch-map-events-notif', state_mgr=sm)
        self.notif_stream_daemon.start()

    def cleanup(self):
        self.notif_stream_daemon.finish()


def send_dispatch_map_update_notif(self, added_device, deleted_device=[]):

    # <dispatch-map-events xmlns="http://com/cisco/corefpcommon">
    #   <device>
    #     <name>abc</name>
    #     <ned-id>cisco-ios-cli-6.67:cisco-ios-cli-6.67</ned-id>
    #     <operation>deleted</operation>
    #   </device>
    #   <device>
    #     <name>xyz</name>
    #     <ned-id>cisco-iosxr-nc-7.3:cisco-iosxr-nc-7.3</ned-id>
    #     <operation>created</operation>
    #   </device>
    # </dispatch-map-events>

    begin_val = _ncs.Value((common_ns.core_fp_common_dispatch_map_events,
                            common_ns.hash), _ncs.C_XMLBEGIN)
    end_val = _ncs.Value((common_ns.core_fp_common_dispatch_map_events,
                          common_ns.hash), _ncs.C_XMLEND)

    xmltag = _ncs.XmlTag(common_ns.hash, common_ns.core_fp_common_dispatch_map_events)
    start = _ncs.TagValue(xmltag=xmltag, v=begin_val)
    end = _ncs.TagValue(xmltag=xmltag, v=end_val)

    device_tag = _ncs.XmlTag(common_ns.hash, common_ns.core_fp_common_device)
    start_device = _ncs.TagValue(xmltag=device_tag, v=begin_val)
    end_device = _ncs.TagValue(xmltag=device_tag, v=end_val)

    device_name_tag = _ncs.XmlTag(common_ns.hash, common_ns.core_fp_common_name)
    ned_name_tag = _ncs.XmlTag(common_ns.hash, common_ns.core_fp_common_ned_id)
    operation_tag = _ncs.XmlTag(common_ns.hash, common_ns.core_fp_common_operation)

    notif_values = []

    for device, ned_id in added_device:
        device_val = _ncs.Value(device, _ncs.C_BUF)
        ned_val = _ncs.Value(ned_id, _ncs.C_BUF)
        operation_val = _ncs.Value(get_operation_enum("created"), _ncs.C_ENUM_HASH)
        notif_values.append(start_device)
        notif_values.append(_ncs.TagValue(xmltag=device_name_tag, v=device_val))
        notif_values.append(_ncs.TagValue(xmltag=ned_name_tag, v=ned_val))
        notif_values.append(_ncs.TagValue(xmltag=operation_tag, v=operation_val))
        notif_values.append(end_device)
    for device in deleted_device:
        device_val = _ncs.Value(device, _ncs.C_BUF)
        operation_val = _ncs.Value(get_operation_enum("deleted"), _ncs.C_ENUM_HASH)
        notif_values.append(start_device)
        notif_values.append(_ncs.TagValue(xmltag=device_name_tag, v=device_val))
        notif_values.append(_ncs.TagValue(xmltag=operation_tag, v=operation_val))
        notif_values.append(end_device)

    notif_values.insert(0, start)
    notif_values.append(end)
    # Send Notification on 'dispatch-map-events' stream.
    global dispatchMapNotifCtx
    current_time = datetime.now()
    notif_time = _ncs.DateTime(current_time.year, current_time.month,
                               current_time.day, current_time.hour,
                               current_time.minute, current_time.second,
                               current_time.microsecond, 0, 0)
    _ncs.dp.notification_send(dispatchMapNotifCtx, notif_time, notif_values)


def get_operation_enum(operation):
    # enum created;
    # enum modified;
    # enum deleted;

    if operation is not None:
        if operation == "deleted":
            return common_ns.core_fp_common_device_deleted
        if operation == "modified":
            return common_ns.core_fp_common_device_modified
        if operation == "created":
            return common_ns.core_fp_common_device_created
    return None
