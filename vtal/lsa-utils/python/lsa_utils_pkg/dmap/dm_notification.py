from datetime import datetime
from typing import Any, List, Tuple

import _ncs
from ..namespaces.lsaUtils_ns import ns as lsa_ns
from . import dm_stream_config


def send_dmap_update_notif(log: Any, added: List[Tuple[str, str]],
                           deleted: List[str] = []) -> None:
    '''Generates dispatch-map-update notification for the devices in added and deleted.
    '''

    # <dispatch-map-update xmlns="http://cisco.com/ns/nso/lsa-utils">
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
    # </dispatch-map-update>

    begin_val = _ncs.Value(
        (lsa_ns.lsa_utils_dispatch_map_update, lsa_ns.hash), _ncs.C_XMLBEGIN
    )
    end_val = _ncs.Value(
        (lsa_ns.lsa_utils_dispatch_map_update, lsa_ns.hash), _ncs.C_XMLEND
    )

    xmltag = _ncs.XmlTag(lsa_ns.hash, lsa_ns.lsa_utils_dispatch_map_update)
    start = _ncs.TagValue(xmltag=xmltag, v=begin_val)
    end = _ncs.TagValue(xmltag=xmltag, v=end_val)

    device_tag = _ncs.XmlTag(lsa_ns.hash, lsa_ns.lsa_utils_device)
    start_device = _ncs.TagValue(xmltag=device_tag, v=begin_val)
    end_device = _ncs.TagValue(xmltag=device_tag, v=end_val)

    device_name_tag = _ncs.XmlTag(lsa_ns.hash, lsa_ns.lsa_utils_name)
    ned_name_tag = _ncs.XmlTag(lsa_ns.hash, lsa_ns.lsa_utils_ned_id)
    operation_tag = _ncs.XmlTag(lsa_ns.hash, lsa_ns.lsa_utils_operation)

    notif_values = []

    for device, ned_id in added:
        device_val = _ncs.Value(device, _ncs.C_BUF)
        ned_val = _ncs.Value(ned_id, _ncs.C_BUF)
        operation_val = _ncs.Value(lsa_ns.lsa_utils_device_created, _ncs.C_ENUM_HASH)
        notif_values.append(start_device)
        notif_values.append(_ncs.TagValue(xmltag=device_name_tag, v=device_val))
        notif_values.append(_ncs.TagValue(xmltag=ned_name_tag, v=ned_val))
        notif_values.append(_ncs.TagValue(xmltag=operation_tag, v=operation_val))
        notif_values.append(end_device)
    for device in deleted:
        device_val = _ncs.Value(device, _ncs.C_BUF)
        operation_val = _ncs.Value(lsa_ns.lsa_utils_device_deleted, _ncs.C_ENUM_HASH)
        notif_values.append(start_device)
        notif_values.append(_ncs.TagValue(xmltag=device_name_tag, v=device_val))
        notif_values.append(_ncs.TagValue(xmltag=operation_tag, v=operation_val))
        notif_values.append(end_device)

    notif_values.insert(0, start)
    notif_values.append(end)
    # Send Notification on 'dispatch-map-events' stream.
    current_time = datetime.now()
    notif_time = _ncs.DateTime(
        current_time.year,
        current_time.month,
        current_time.day,
        current_time.hour,
        current_time.minute,
        current_time.second,
        current_time.microsecond,
        0,
        0,
    )
    log.debug(f"Notification Ctx {dm_stream_config.notifCtx}")
    _ncs.dp.notification_send(dm_stream_config.notifCtx, notif_time, notif_values)
    log.info("Notified remote nso of the dispatch-map updates")
