# ------------------------
# PLUG-In CLASS has to implement all funtions mentioned below
# ------------------------


class Ios:
    def get_interface_loopback(self, root, service):
        """
        Gets interface Loopback on given head-end

        """
        self.log.info(f"Find Loopback interface on {service.head_end}")

    def conf_rsvp_te_tunnel_p2p(self, device, loopback):
        """
        Applies RSVP-TE config on the device.
        Path: /cisco-rsvp-te-fp:rsvp-te/tunnel-te
        """
        self.log.info(f"Configuring SegmentLIST on IOS device {device}")

    def ietf_te_self_test(self, uinfo, root, rsvp_te_service):
        """
        This method is run for RSVP-TE tunnel.
        It checks admin & operational state of the tunnel.
            Response should be either of these:
            ("success", None)
            ("failed", "some failure message")
        """
        self.log.info(f"Executing self-test on IOS device {rsvp_te_service.head_end}")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
