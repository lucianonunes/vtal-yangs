# ------------------------
# PLUG-In CLASS has to implement all funtions mentioned below
# ------------------------


class Ios:
    def conf_l2vpn(self, site, local):
        self.log.info(f"Configuring Flat L2VPN/SR on IOS for service {site.pe}")

    def conf_l2vpn_rp(self, rr_parent_route_policy):
        self.log.info("Configuring Route Policy on IOSXR for "
                      f"service {rr_parent_route_policy.name} "
                      f"on {rr_parent_route_policy.device}")

    def validate_parent_policy_exists(self, root, device, parent_policy):
        pass

    def get_original_policy(self, root, device, parent_policy):
        pass

    def check_if_interface_exists(self, root, site, service_interface_name, service_interface_id):
        self.log.info(f"Checking {service_interface_name + service_interface_id} "
                      f"on IOS device {site.pe}")
        return False

    def get_interface_shutdown_template(self):
        return None

    def l2vpn_self_test(self, root, service, device, xc_group, xc_name):
        self.log.info(f"Running l2vpn self test on IOS device {device}")
        return ("success", "Not Implemented")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
