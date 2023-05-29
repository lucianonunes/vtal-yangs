from threading import Thread

import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
import traceback
from datetime import datetime


class TMTCInternalHandler(Thread):
    """
    This thread is started by external tm-tc subscriber to handle creating/deleting internal
    tm-tc service.
    """

    def __init__(self, username, service_name, node, operation, log):
        Thread.__init__(self)
        self.username = username
        self.service_name = service_name
        self.node = node
        self.operation = operation
        self.log = log

    def run(self):
        service_name = self.service_name
        node = self.node
        operation = self.operation
        username = self.username
        self.log.info("Handle {} internal tm-tc service for: {} {}". \
                      format(operation, service_name, node))
        tm_tc_kp = ("/cisco-tm-tc-fp:tm-tc{{{}}}".format(service_name))

        if operation == "create":
            # Remove oper-data from previous commit to set northbound plan to not-reached
            #     and to indicate change in plan
            reset_oper_data(service_name, node, self.log)
            with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
                tm_tc_service = maagic.get_node(th, tm_tc_kp)
                self.log.info("Copy internal tm-tc service for {} {}".format(service_name, node))
                create_internal_service(tm_tc_service, service_name, node)
                try:
                    th.apply()
                    self.log.info("Added/Updated tm-tc: {} {}".format(service_name, node))
                    set_oper_data(service_name, node, None, self.log)
                except Exception as e:
                    traceback.print_exc()
                    self.log.error("Received {} when commiting: {} {}".format(e, service_name,
                                                                              node))
                    set_oper_data(service_name, node, str(e), self.log)
        elif operation == "delete":
            with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
                self.log.info("Delete tm-tc: {} {}".format(service_name, node))
                internal_tm_tc_kp = "/cisco-tm-tc-fp-internal:tm-tc"
                internal_tm_tc = maagic.get_node(th, internal_tm_tc_kp)
                th.safe_delete(internal_tm_tc[(service_name, node)]._path)
                try:
                    th.apply()
                    self.log.info("Deleted tm-tc: {} {}".format(service_name, node))
                    reset_oper_data(service_name, node, self.log)
                except Exception as e:
                    traceback.print_exc()
                    self.log.error("Received {} when deleting: {} {}".format(e, service_name, node))
                    set_oper_data(service_name, node, str(e), self.log)


def set_oper_data(service_name, node, error_msg, log):
    with ncs.maapi.single_write_trans("", "system", db=ncs.OPERATIONAL) as th:
        root = ncs.maagic.get_root(th)
        internal_result = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_oper_data. \
            create(service_name, node)
        internal_result.status_type = "service-commit"
        if error_msg is not None:
            internal_result.status = "failed"
            internal_result.status_message = error_msg
        else:
            internal_result.status = "success"
        internal_result.time = datetime.utcnow().isoformat()
        th.apply()
        log.info("set_oper_data for tm-tc: {} {}".format(service_name, node))


def reset_oper_data(service_name, node, log):
    with ncs.maapi.single_write_trans("", "system", db=ncs.OPERATIONAL) as th:
        root = ncs.maagic.get_root(th)
        internal_result = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_oper_data
        if (service_name, node) in internal_result:
            th.safe_delete(internal_result[(service_name, node)]._path)
        th.apply()
        log.info("reset_oper_data for tm-tc: {} {}".format(service_name, node))


def create_internal_service(tm_tc_service, service_name, node):
    # Create node tm-tc services
    template = ncs.template.Template(tm_tc_service)
    template_vars = ncs.template.Variables()
    template_vars.add('SERVICE_NAME', service_name)
    template_vars.add('NODE', node)
    template.apply('cisco-tm-tc-fp-copy-internal', template_vars)
