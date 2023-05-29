# ------------------------
# PLUG-In CLASS has to implement all funtions mentioned below
# ------------------------


class Ios:
    def conf_sr_policy(self, device):
        """
        Applies SR explicit/dynamic policy on device.
        Path: /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/policy/

        """
        self.log.info(f"Configuring SR policy on IOS device {device}")

    def conf_segment_list(self, device):
        """
        Applies SR sid-list on the device.
        Path: /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/
                                                            sid-list referred by:
        Path: /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/
                                                        policy/path/explicit/sid-list

        """
        self.log.info(f"Configuring SegmentLIST on IOS device {device}")

    def conf_sr_template(self, device):
        """
        Applies SR odn-template on device.
        Path: /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-odn:odn/odn-template

        """
        self.log.info(f"Configuring SR ODN template on IOS device {device}")

    def get_max_sid_depth(self, root, device):
        """
        This method is required for SR-TE policy service to validate the configured paths
        do not exceed max-sid-depth pre-configured on the device.
        Path: /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/
                                                        policy/path/explicit/sid-list/sid
        """
        self.log.info(f"Getting max-sid-depth on IOS device {device.name}")
        return 0

    def validate_srv6_te(self, root, service, device):
        """
        This method is required for SR-TE service to validate if SRv6-TE is supported on device
        Should raise exception if SRv6-TE is not supported
        """
        self.log.info(f"Validating if SRv6-TE is supported on IOS device {device}")

    @staticmethod
    def sr_policy_self_test(self, uinfo, root, service, policy, device):
        """
        This method is run for the explicit/dynamic SR-TE policy.
        It checks admin & operational state of the policy.
            Response should be either of these:
            ("success", None)
            ("in-progress", None)
            ("failed", "some failure message")
        """
        return ("success", None)

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
