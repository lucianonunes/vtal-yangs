import ncs
import _ncs
import ncs.maagic as maagic
from ncs.dp import StateManager
from ncs.dp import Daemon
from datetime import datetime
from ncs.ns.ncs_ns import ns as ns_ncs
import re
import traceback

notifCtx = None


class NotifStreamSM(StateManager):
    def __init__(self, log):
        self.log = log
        StateManager.__init__(self, log)

    def setup(self, state, previous_state):
        global notifCtx
        notifCtx = self.register_notification_stream(
            state, None, ncs.dp.take_worker_socket(state, "common-plan-notif-name",
                                                          "common-plan-notif-key"),
            "service-state-changes")

    def teardown(self, state, finished):
        ncs.dp.return_worker_socket(state, "common-plan-notif-key")


class NotifStreamRegistrar(object):
    def __init__(self, log):
        sm = NotifStreamSM(log)
        self.notif_stream_daemon = Daemon('common-plan-notif', state_mgr=sm)
        self.notif_stream_daemon.start()

    def cleanup(self):
        self.notif_stream_daemon.finish()


class PlanNotifHandler(ncs.application.Application):
    def setup(self):
        self.log.info('PlanNotifHandler RUNNING')
        self.register_service("plan-notification-servicepoint", PlanNotifCallback)
        self.register_action("generate-plan-notifications", PlanNotifAction)
        self.register_action("generate-zombie-notifications", ZombieNotifAction)
        self.notif_reg = NotifStreamRegistrar(self.log)

    def teardown(self):
        self.notif_reg.cleanup()
        self.log.info('PlanNotifHandler FINISHED')


class PlanNotifCallback(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        service_id = get_kp_service_id(kp)
        self.log.info(f"Plan notification kicker for: {service_id}, operation: {op}")

        if op == _ncs.dp.NCS_SERVICE_CREATE or op == _ncs.dp.NCS_SERVICE_UPDATE:
            service = root.core_fp_plan_notif_generator__plan_path_for_notification[service_id]
            vars = ncs.template.Variables()
            template = ncs.template.Template(service)
            vars.add("PLAN_PATH", service_id)
            template.apply('plan-notification-kicker', vars)

        if op == _ncs.dp.NCS_SERVICE_DELETE:
            del root.kicker__kickers.data_kicker[f"plan-notification-kicker-{service_id}"]

        return proplist

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        return proplist


def get_kp_service_id(kp):
    kpath = str(kp)
    service = kpath[kpath.find("{") + 1: len(kpath) - 1]
    return service


class PlanNotifAction(ncs.dp.Action):
    """
    Action handler for sending notification for plan deletion
    """
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        self.log.info(f"PlanNotifAction plan path: {input.path}")
        try:
            with ncs.maapi.Maapi() as m:
                with m.attach(input.tid, 0) as t:
                    diter = PlanNotifDiffIterator(input.path)
                    t.diff_iterate(diter, 0)

            if diter.plan_deleted or diter.plan_post_action_failed:
                self.log.info(f"Plan changed for delete or post-action for : {input.path}")
                # Get the key part of the plan in tuple formart
                service_key_str = re.sub(r'^.*\{(.*?)\}[^\(]*$', r'\g<1>', input.path)
                service_key_tuple = tuple(service_key_str.split(' '))

                # Get external plan path without key portion
                plan_path = input.path[:input.path.rfind("{")]

                with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
                    root = maagic.get_root(th)
                    service_path = root.\
                        core_fp_plan_notif_generator__plan_path_for_notification[plan_path].\
                        service_path
                    service_key_elements = root.\
                        core_fp_plan_notif_generator__plan_path_for_notification[plan_path].\
                        service_key_elements

                    # Form xpath for service
                    notif_service_path = service_path
                    for index, ske in enumerate(service_key_elements):
                        notif_service_path += f"[{ske}={service_key_tuple[index]}]"

                    # Get HKeypathRef from service xpath
                    notif_service_kpath = _ncs.maapi.xpath2kpath(th.maapi.msock, notif_service_path)
                    if diter.plan_deleted:
                        self.log.debug("Plan deletion case")
                        send_plan_change_notification(self.log, notif_service_kpath, "deleted")
                    elif diter.plan_post_action_failed:
                        self.log.info("Plan post-action failure case")
                        send_plan_change_notification(self.log, notif_service_kpath, "modified",
                                                      "failed")
        except Exception as e:
            traceback.print_exc()
            self.log.error(f"Exception while sending notification for plan change: {str(e)} ")


class PlanNotifDiffIterator(object):
    def __init__(self, path):
        self.plan_deleted = False
        self.plan_post_action_failed = False
        self.path = path

    def __call__(self, keypath, op, oldv, newv):
        if str(self.path) == str(keypath) and op == _ncs.MOP_DELETED:
            self.plan_deleted = True
            return ncs.ITER_STOP
        elif 'post-action-status' in str(keypath) and newv == ns_ncs.ncs_plan_action_failed\
                and op != _ncs.MOP_DELETED:
            self.plan_post_action_failed = True
            return ncs.ITER_STOP
        return ncs.ITER_RECURSE


class ZombieNotifAction(ncs.dp.Action):
    """
    Action handler for sending notification for failed zombies
    """
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        self.log.info(f"ZombieNotifAction plan path: {input.path}")

        try:
            with ncs.maapi.Maapi() as m:
                with m.attach(input.tid, 0) as t:
                    diter = ZombieNotifDiffIterator(input.path)
                    t.diff_iterate(diter, 0)

            if diter.delete_failed:
                self.log.info(f"Service Delete Failed for: {input.path}")
                # /ncs:zombies/service{"xyz"}/plan/failed
                #  Get the key part of the plan in tuple formart
                service_path = re.sub(r'^.*\{\"(.*?)\"\}[^\(]*$', r'\g<1>', input.path)

                with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
                    # Get HKeypathRef from service xpath
                    notif_service_kpath = _ncs.maapi.xpath2kpath(th.maapi.msock, service_path)
                    send_plan_change_notification(self.log, notif_service_kpath, "deleted",
                                                                                 "failed")
        except Exception as e:
            traceback.print_exc()
            self.log.error(f"Exception while sending notification for plan deletion: {str(e)} ")


class ZombieNotifDiffIterator(object):
    def __init__(self, path):
        self.delete_failed = False
        self.path = path

    def __call__(self, keypath, op, oldv, newv):
        # If zombie is created, failure could be from create failed plan copy
        # stop iterating changes
        if str(self.path.replace("\"", "")) == str(keypath) and op == _ncs.MOP_CREATED:
            return ncs.ITER_STOP

        # If plan/failed is created when zombie is modified, it is delete failure.
        plan_failed_path = f'{self.path}/plan/failed'
        if str(plan_failed_path.replace("\"", "")) == str(keypath) and op == _ncs.MOP_CREATED:
            self.delete_failed = True
            return ncs.ITER_STOP

        return ncs.ITER_RECURSE


def send_plan_change_notification(log, service_kpath, operation, status=None):

    # If plan is deleted, send notification in this format
    # plan-state-change {
    #     service /service-path[name='service-id']
    #     operation deleted
    # }

    notif = get_plan_change_notif(service_kpath, operation, status)
    send_plan_change_notif(log, notif, service_kpath)


def get_plan_change_notif(service_kp, operation, status=None):
    notif_values = [get_service_tag_value(service_kp), get_op_tag_value(operation)]
    if status is not None:
        notif_values.append(get_status_tag_value(status))

    return create_plan_state_notif(notif_values)


def send_plan_change_notif(log, notif, service_kp):
    # Send Notification on 'service-state-changes' stream.
    log.info(f"Sending Notification for Plan : {service_kp}")
    global notifCtx
    send_notification(notifCtx, notif)


def send_notification(ctx, notif):
    current_time = datetime.utcnow()
    notif_time = _ncs.DateTime(current_time.year, current_time.month,
                               current_time.day, current_time.hour,
                               current_time.minute, current_time.second,
                               current_time.microsecond, 0, 0)
    _ncs.dp.notification_send(ctx, notif_time, notif)


def create_plan_state_notif(values):
    xml_begin = _ncs.Value((ns_ncs.ncs_plan_state_change, ns_ncs.hash),
                           _ncs.C_XMLBEGIN)
    xml_end = _ncs.Value((ns_ncs.ncs_plan_state_change, ns_ncs.hash),
                         _ncs.C_XMLEND)
    state_change_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_plan_state_change)
    start = _ncs.TagValue(xmltag=state_change_tag, v=xml_begin)
    end = _ncs.TagValue(xmltag=state_change_tag, v=xml_end)

    return [start] + values + [end]


def get_service_tag_value(service_kp):
    service_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_service)
    service_val = _ncs.Value(service_kp, _ncs.C_OBJECTREF)
    return _ncs.TagValue(xmltag=service_tag, v=service_val)


def get_op_tag_value(op):
    op_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_operation)
    op_val = _ncs.Value(get_operation_enum(op), _ncs.C_ENUM_HASH)
    return _ncs.TagValue(xmltag=op_tag, v=op_val)


def get_status_tag_value(status):
    status_tag = _ncs.XmlTag(ns_ncs.hash, ns_ncs.ncs_status)
    status_val = _ncs.Value(get_status_enum(status), _ncs.C_ENUM_HASH)
    return _ncs.TagValue(xmltag=status_tag, v=status_val)


def get_status_enum(status):
    # enum not-reached;
    # enum reached;
    # enum failed;

    if status is not None:
        if status == "failed":
            return ns_ncs.ncs_plan_failed
        if status == "not-reached":
            return ns_ncs.ncs_not_reached
        if status == "reached":
            return ns_ncs.ncs_reached
    return None


def get_operation_enum(operation):
    # enum created;
    # enum modified;
    # enum deleted;

    if operation is not None:
        if operation == "deleted":
            return ns_ncs.ncs_plan_state_deleted
        if operation == "modified":
            return ns_ncs.ncs_plan_state_modified
        if operation == "created":
            return ns_ncs.ncs_plan_state_created
    return None
