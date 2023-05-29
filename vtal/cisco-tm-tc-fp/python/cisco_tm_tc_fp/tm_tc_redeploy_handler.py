from threading import Thread

from . import tm_tc_internal_handler
import time


class TMTCRedeployHandler(Thread):
    """
    This thread is started by external tm-tc subscriber to handle re-deploying of
    TMTC service (by creating internal kicker and setting/resetting internal oper-data).
    """

    def __init__(self, username, nodes, operation, log):
        Thread.__init__(self)
        self.username = username
        self.nodes = nodes
        self.operation = operation
        self.log = log

    def run(self):
        username = self.username
        operation = self.operation
        nodes = self.nodes
        self.log.info("TMTCRedeployHandler: operation {} on nodes {}".
                      format(operation, nodes))
        if operation == "created" or operation == "updated":
            # New nodes
            if len(nodes) > 0:
                # Toggle oper-data for external plan to run
                for node in nodes:
                    tm_tc_internal_handler.reset_oper_data(node[0], node[1], self.log)
                time.sleep(2)
                for node in nodes:
                    tm_tc_internal_handler.set_oper_data(node[0], node[1], None, self.log)
        elif operation == "deleted":
            # Deleted nodes
            for node in nodes:
                # Reset oper-data for external plan to run
                tm_tc_internal_handler.reset_oper_data(node[0], node[1], self.log)
