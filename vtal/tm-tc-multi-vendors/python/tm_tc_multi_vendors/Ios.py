import ncs

# ------------------------
# PLUG-In CLASS has to implement all funtions mentioned below
# ------------------------

class Ios():
    def conf_tm_tc(self, node):
        self.log.info("Configuring TMTC on IOS device {}".format(node.name))

    def check_if_interface_exists(self, root, node, service_interface_name, service_interface_id):
        self.log.info("Checking {} on IOS device {}"
                      .format(service_interface_name+service_interface_id, node.name))
        return False

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
