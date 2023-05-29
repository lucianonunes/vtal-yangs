/*
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 * This file has been auto-generated by the confdc compiler.
 * Source: ../load-dir/ietf-subscribed-notifications.fxs
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 */

package com.example.onftapi.namespaces;

import com.tailf.conf.ConfNamespace;

/** Autogenerated namespace class for YANG module ietf-subscribed-notifications.yang */
public class ietfSubscribedNotifications extends ConfNamespace {
    public static final int hash = 824051678;
    public int hash() {
        return ietfSubscribedNotifications.hash;
    }

    public static final String id = "_onf-tapi-nc-2.0:onf-tapi-nc-2.0#urn:ietf:params:xml:ns:yang:ietf-subscribed-notifications";
    public String id() {
        return ietfSubscribedNotifications.id;
    }

    public static final String uri = "_onf-tapi-nc-2.0:onf-tapi-nc-2.0#urn:ietf:params:xml:ns:yang:ietf-subscribed-notifications";
    public String uri() {
        return ietfSubscribedNotifications.uri;
    }

    public String xmlUri() {
        return ConfNamespace.truncateToXMLUri(ietfSubscribedNotifications.uri);
    }

    public static final String prefix = "sn";
    public String prefix() {
        return ietfSubscribedNotifications.prefix;
    }

    public ietfSubscribedNotifications() {}

    public static int stringToHash(String str) {
        return ConfNamespace.stringToHash(str);
    }

    public static String hashToString(int hash) {
        return ConfNamespace.hashToString(hash);
    }

    public static final int    _stream = 1561687317;
    public static final String _stream_ = "stream";
    public static final int    _subscription = 982993959;
    public static final String _subscription_ = "subscription";
    public static final int    _filter_failure_hint = 293424073;
    public static final String _filter_failure_hint_ = "filter-failure-hint";
    public static final int    _encoding_unsupported = 325110823;
    public static final String _encoding_unsupported_ = "encoding-unsupported";
    public static final int    _info = 2062105651;
    public static final String _info_ = "info";
    public static final int    _receivers = 1989386850;
    public static final String _receivers_ = "receivers";
    public static final int    _suspension_timeout = 963276891;
    public static final String _suspension_timeout_ = "suspension-timeout";
    public static final int    _replay_support = 1991147815;
    public static final String _replay_support_ = "replay-support";
    public static final int    _rpc_delete_subscription = 1832746858;
    public static final String _rpc_delete_subscription_ = "rpc-delete-subscription";
    public static final int    _streams = 1043459567;
    public static final String _streams_ = "streams";
    public static final int    _configured_replay = 1551204505;
    public static final String _configured_replay_ = "configured-replay";
    public static final int    _subscription_resumed = 103228861;
    public static final String _subscription_resumed_ = "subscription-resumed";
    public static final int    _establish_subscription = 636289624;
    public static final String _establish_subscription_ = "establish-subscription";
    public static final int    _subscription_terminated_reason = 1669520593;
    public static final String _subscription_terminated_reason_ = "subscription-terminated-reason";
    public static final int    _modify_subscription = 315204316;
    public static final String _modify_subscription_ = "modify-subscription";
    public static final int    _no_such_subscription = 735721563;
    public static final String _no_such_subscription_ = "no-such-subscription";
    public static final int    _dependency = 703076977;
    public static final String _dependency_ = "dependency";
    public static final int    _encoding = 180862443;
    public static final String _encoding_ = "encoding";
    public static final int    _replay_unsupported = 637749287;
    public static final String _replay_unsupported_ = "replay-unsupported";
    public static final int    _within_subscription = 1926240005;
    public static final String _within_subscription_ = "within-subscription";
    public static final int    _result = 401396173;
    public static final String _result_ = "result";
    public static final int    _subscription_completed = 1464445382;
    public static final String _subscription_completed_ = "subscription-completed";
    public static final int    _stream_subtree_filter = 794261360;
    public static final String _stream_subtree_filter_ = "stream-subtree-filter";
    public static final int    _modify_subscription_stream_error_info = 1690961663;
    public static final String _modify_subscription_stream_error_info_ = "modify-subscription-stream-error-info";
    public static final int    _target = 1914145912;
    public static final String _target_ = "target";
    public static final int    _weighting = 316732155;
    public static final String _weighting_ = "weighting";
    public static final int    _time = 190171445;
    public static final String _time_ = "time";
    public static final int    _device = 617911018;
    public static final String _device_ = "device";
    public static final int    _source_address = 554848846;
    public static final String _source_address_ = "source-address";
    public static final int    _address_originated = 745787873;
    public static final String _address_originated_ = "address-originated";
    public static final int    _state = 630973766;
    public static final String _state_ = "state";
    public static final int    _rpc_establish_subscription = 1527730549;
    public static final String _rpc_establish_subscription_ = "rpc-establish-subscription";
    public static final int    _dscp_unavailable = 2076845623;
    public static final String _dscp_unavailable_ = "dscp-unavailable";
    public static final int    _purpose = 760715521;
    public static final String _purpose_ = "purpose";
    public static final int    _by_reference = 1956288752;
    public static final String _by_reference_ = "by-reference";
    public static final int    _establish_subscription_stream_error_info = 1693440292;
    public static final String _establish_subscription_stream_error_info_ = "establish-subscription-stream-error-info";
    public static final int    _delete_subscription_error = 973299380;
    public static final String _delete_subscription_error_ = "delete-subscription-error";
    public static final int    _rpc_kill_subscription = 596546373;
    public static final String _rpc_kill_subscription_ = "rpc-kill-subscription";
    public static final int    _subscription_started = 1544792268;
    public static final String _subscription_started_ = "subscription-started";
    public static final int    _configurable_encoding = 445041650;
    public static final String _configurable_encoding_ = "configurable-encoding";
    public static final int    _stream_filter = 1051720806;
    public static final String _stream_filter_ = "stream-filter";
    public static final int    _unsupportable_volume = 29466054;
    public static final String _unsupportable_volume_ = "unsupportable-volume";
    public static final int    _kill_subscription = 2011978673;
    public static final String _kill_subscription_ = "kill-subscription";
    public static final int    _sent_event_records = 124909821;
    public static final String _sent_event_records_ = "sent-event-records";
    public static final int    _excluded_event_records = 2023619574;
    public static final String _excluded_event_records_ = "excluded-event-records";
    public static final int    _filter_unsupported = 1766278786;
    public static final String _filter_unsupported_ = "filter-unsupported";
    public static final int    _notification_message_origin = 1339019163;
    public static final String _notification_message_origin_ = "notification-message-origin";
    public static final int    _rpc_modify_subscription = 1092915995;
    public static final String _rpc_modify_subscription_ = "rpc-modify-subscription";
    public static final int    _stream_filter_name = 435086494;
    public static final String _stream_filter_name_ = "stream-filter-name";
    public static final int    _replay_start_time = 260208840;
    public static final String _replay_start_time_ = "replay-start-time";
    public static final int    _stream_xpath_filter = 1079167847;
    public static final String _stream_xpath_filter_ = "stream-xpath-filter";
    public static final int    _reset = 888276656;
    public static final String _reset_ = "reset";
    public static final int    _modify_subscription_error = 704315833;
    public static final String _modify_subscription_error_ = "modify-subscription-error";
    public static final int    _receiver = 1608687009;
    public static final String _receiver_ = "receiver";
    public static final int    _encode_json = 205258720;
    public static final String _encode_json_ = "encode-json";
    public static final int    _filters = 611375488;
    public static final String _filters_ = "filters";
    public static final int    _filter_unavailable = 1881994272;
    public static final String _filter_unavailable_ = "filter-unavailable";
    public static final int    _stream_unavailable = 1857551741;
    public static final String _stream_unavailable_ = "stream-unavailable";
    public static final int    _filter_spec = 354841189;
    public static final String _filter_spec_ = "filter-spec";
    public static final int    _source_vrf = 1676409291;
    public static final String _source_vrf_ = "source-vrf";
    public static final int    _reason = 565156039;
    public static final String _reason_ = "reason";
    public static final int    _replay_log_aged_time = 1588166633;
    public static final String _replay_log_aged_time_ = "replay-log-aged-time";
    public static final int    _delete_subscription = 683800725;
    public static final String _delete_subscription_ = "delete-subscription";
    public static final int    _name = 1998270519;
    public static final String _name_ = "name";
    public static final int    _interface_originated = 1906017485;
    public static final String _interface_originated_ = "interface-originated";
    public static final int    _transport = 514190987;
    public static final String _transport_ = "transport";
    public static final int    _configured_subscription_state = 1543968618;
    public static final String _configured_subscription_state_ = "configured-subscription-state";
    public static final int    _insufficient_resources = 473621368;
    public static final String _insufficient_resources_ = "insufficient-resources";
    public static final int    _source_interface = 1180772123;
    public static final String _source_interface_ = "source-interface";
    public static final int    _subscriptions = 886823438;
    public static final String _subscriptions_ = "subscriptions";
    public static final int    _replay_previous_event_time = 2059289863;
    public static final String _replay_previous_event_time_ = "replay-previous-event-time";
    public static final int    _subscription_modified = 268790463;
    public static final String _subscription_modified_ = "subscription-modified";
    public static final int    _subscription_suspended_reason = 755093032;
    public static final String _subscription_suspended_reason_ = "subscription-suspended-reason";
    public static final int    _id = 1279252765;
    public static final String _id_ = "id";
    public static final int    _stop_time = 397011733;
    public static final String _stop_time_ = "stop-time";
    public static final int    _delete_subscription_error_info = 945582394;
    public static final String _delete_subscription_error_info_ = "delete-subscription-error-info";
    public static final int    _encode_xml = 1982625157;
    public static final String _encode_xml_ = "encode-xml";
    public static final int    _description = 202559156;
    public static final String _description_ = "description";
    public static final int    _dscp = 1118112132;
    public static final String _dscp_ = "dscp";
    public static final int    _subscription_terminated = 1539093693;
    public static final String _subscription_terminated_ = "subscription-terminated";
    public static final int    _subscription_suspended = 1845885169;
    public static final String _subscription_suspended_ = "subscription-suspended";
    public static final int    _establish_subscription_error = 1478451510;
    public static final String _establish_subscription_error_ = "establish-subscription-error";
    public static final int    _replay_start_time_revision = 1451909706;
    public static final String _replay_start_time_revision_ = "replay-start-time-revision";
    public static final int    _replay_log_creation_time = 896118574;
    public static final String _replay_log_creation_time_ = "replay-log-creation-time";
    public static final int    _replay_completed = 478147800;
    public static final String _replay_completed_ = "replay-completed";
}