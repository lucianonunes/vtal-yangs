# ------------------------
# PLUG-In CLASS has to implement all funtions mentioned below
# ------------------------


class Ios:
    def conf_l3vpn(self, endpoint):
        self.log.info(f"Configuring Flat L3VPN/SR on IOS device {endpoint.access_pe}")

    def check_if_interface_exists(self, root, endpoint,
                                  service_interface_name, service_interface_id):
        self.log.info(f"Checking {service_interface_name + service_interface_id} "
                      f"on IOS device {endpoint.access_pe}")
        return False

    def is_vrf_address_family_active(self, root, device, bgp_as_no, vrf_name):
        # Check if vrf-address family for given VRF is active on the device
        return False

    def get_vrf_rd(self, root, device, vrf_name):
        # Return RD from global VRF
        return None

    def get_bgp_vrf_rd(self, root, device, bgp_as_no, vrf_name):
        # Return RD from BGP VRF instance
        return None

    def get_interface_shutdown_template(self):
        return None

    def l3vpn_self_test(self, root, service, vrf_name, device, src, dst):
        self.log.info(f"Running L3vpn self test on IOS device {device}")
        return ("success", "Not Implemented")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
