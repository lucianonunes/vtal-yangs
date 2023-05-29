import re
from datetime import datetime
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper

import ncs
import ncs.maagic as maagic
import _ncs
import _ncs.dp
from ncs.dp import StateManager
from ncs.dp import Daemon

from ncs.application import Application

from ._namespaces.cisco_aa_service_assurance_ns import ns as aa_ns

notifCtx = None


def get_kp_service_id(kp):
    kpath = str(kp)
    service = kpath[kpath.find("{") + 1: len(kpath) - 1]
    return service


class ServiceAssuranceActionHandler(ncs.dp.Action):
    """
    Action handler for invoking a specific service action
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        try:
            premod_diter = self.diff_iterate(input)

            if (not premod_diter.is_redeploy) and premod_diter.service_assurance:
                # Get the key part of the service in tuple formart
                service_key_str = re.sub(r"^.*\{(.*?)\}[^\(]*$", r"\g<1>", input.path)
                service_key_tuple = tuple(service_key_str.split(" "))
                # Get service path without key portion
                service_path = input.path[: input.path.rfind("{")]

                with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
                    root = maagic.get_root(th)
                    service_key_elements = root.\
                        cisco_aa_service_assurance__service_path_for_subscription[service_path].\
                        service_key_elements

                    # Form xpath for service
                    notif_service_path = service_path
                    for index, ske in enumerate(service_key_elements):
                        notif_service_path += ("[" + ske + "=" + service_key_tuple[index] + "]")

                    # Sending out AA notif
                    operation = get_operation_string(premod_diter.service_assurance_op)
                    self.log.info(f"CRUD on AA config for Service: {notif_service_path}")
                    send_aa_change_notification(self.log, notif_service_path, operation)

        except Exception as e:
            self.log.error(f"Exception while processing service: {str(e)} ")

    @staticmethod
    def diff_iterate(input) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            kp_len = len(keypath)
            is_key_path0_tuple = isinstance(keypath[0], tuple)
            key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])

            # Only looking for change in the service-assurance container
            if not is_key_path0_tuple:
                # First check if redeploy
                if key_path0 == "re-deploy-counter" and isinstance(newv, int) and newv > 0:
                    self.is_redeploy = True
                    return ncs.ITER_STOP
                else:
                    # The 3 is for Flat L2/L3 - inside the container change
                    # The 5 is for IETF L2/L3 - inside the container change
                    if (kp_len > 3 and str(keypath[kp_len - 3]) == "service-assurance") or (
                            kp_len > 5 and str(keypath[kp_len - 5]) == "service-assurance"):
                        self.service_assurance = True
                        self.service_assurance_op = op
                        return ncs.ITER_STOP
            return ncs.ITER_RECURSE

        diff_iter = DiffIterateWrapper(diter,
                                       is_redeploy=False,
                                       service_assurance=False,
                                       service_assurance_op=None)

        with ncs.maapi.Maapi() as m:
            with m.attach(input.tid, 0) as t:
                t.diff_iterate(diff_iter, 0)

        return diff_iter


class ServiceAssuranceSubscriptionCallback(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        service_id = get_kp_service_id(kp)
        self.log.info("Service assurance subscription kicker for: {}, operation: {}".
                      format(service_id, op))

        global plan_to_service_key_dict
        if op == _ncs.dp.NCS_SERVICE_CREATE or op == _ncs.dp.NCS_SERVICE_UPDATE:
            service = root.cisco_aa_service_assurance__service_path_for_subscription[service_id]
            vars = ncs.template.Variables()
            template = ncs.template.Template(service)
            vars.add("SERVICE_PATH", service_id)
            template.apply("service-assurance-subsciption-kicker", vars)

        if op == _ncs.dp.NCS_SERVICE_DELETE:
            del root.kicker__kickers.data_kicker["service-assurance-subsciption-kicker"
                                                 + service_id]

        return proplist

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        return proplist


def get_operation_string(operation):
    if operation == _ncs.MOP_CREATED:
        return "created"
    else:
        return "modified"


def send_aa_change_notification(log, service_path, operation):
    v = _ncs.Value(
        (aa_ns.cisco_aa_service_assurance_service_assurance_config_change, aa_ns.hash),
        _ncs.C_XMLBEGIN,
    )
    v2 = _ncs.Value(
        (aa_ns.cisco_aa_service_assurance_service_assurance_config_change, aa_ns.hash),
        _ncs.C_XMLEND,
    )
    xmltag = _ncs.XmlTag(
        aa_ns.hash, aa_ns.cisco_aa_service_assurance_service_assurance_config_change
    )
    start = _ncs.TagValue(xmltag=xmltag, v=v)
    end = _ncs.TagValue(xmltag=xmltag, v=v2)

    service_tag = _ncs.XmlTag(aa_ns.hash, aa_ns.cisco_aa_service_assurance_service)
    service_val = _ncs.Value(service_path, _ncs.C_BUF)

    op_tag = _ncs.XmlTag(aa_ns.hash, aa_ns.cisco_aa_service_assurance_operation)
    op_val = _ncs.Value(operation, _ncs.C_BUF)

    notif_values = [
        _ncs.TagValue(xmltag=service_tag, v=service_val),
        _ncs.TagValue(xmltag=op_tag, v=op_val),
    ]

    final_notif_values = [start] + notif_values + [end]

    # Send Notification on 'service-aa-changes' stream.
    global notifCtx
    current_time = datetime.utcnow()
    notif_time = _ncs.DateTime(current_time.year, current_time.month, current_time.day,
                               current_time.hour, current_time.minute, current_time.second,
                               current_time.microsecond, 0, 0)
    _ncs.dp.notification_send(notifCtx, notif_time, final_notif_values)


class NotifStreamSM(StateManager):
    def __init__(self, log):
        self.log = log
        StateManager.__init__(self, log)

    def setup(self, state, previous_state):
        global notifCtx
        notifCtx = self.register_notification_stream(state, None, ncs.dp.take_worker_socket(
            state, "service-aa-changes-notif-ws-name",
            "service-aa-changes-notif-ws-key"), "service-aa-changes")

    def teardown(self, state, finished):
        ncs.dp.return_worker_socket(state, "service-aa-changes-notif-ws-key")


class NotifStreamRegistrar(object):
    def __init__(self, log):
        sm = NotifStreamSM(log)
        self.notif_stream_daemon = Daemon("service-aa-changes-notif", state_mgr=sm)
        self.notif_stream_daemon.start()

    def cleanup(self):
        self.notif_stream_daemon.finish()


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(Application):
    """This class is referred to from the package-meta-data.xml."""

    def setup(self):
        self.log.debug("service-assurance app start")
        self.register_action("cisco-aa-service-assurance-actionpoint",
                             ServiceAssuranceActionHandler)

        self.register_service("cisco-aa-service-assurance-subscription-servicepoint",
                              ServiceAssuranceSubscriptionCallback)

        self.notif_reg = NotifStreamRegistrar(self.log)

    def teardown(self):
        self.log.debug("service-assurance app stop")
        self.notif_reg.cleanup()
