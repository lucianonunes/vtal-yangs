# -*- mode: python; python-indent: 4 -*-
import ncs
from . import flat_L2vpn_local_site_nano_services as flat_L2vpn_local_site_nano_services
from . import flat_L2vpn_remote_site_nano_services as flat_L2vpn_remote_site_nano_services
from . import flat_L2vpn_site_nano_services as flat_L2vpn_site_nano_services
from . import flat_L2vpn_actions as flat_L2vpn_actions
from .flat_L2vpn_rr_parent_route_policy import RRParentRoutePolicyCallback
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("cisco-flat-L2-vpn-fp-internal RUNNING")

        # L2VPN RR PARENT ROUTE POLICY
        self.register_service(
            "l2vpn-rr-parent-route-policy", RRParentRoutePolicyCallback
        )

        # L2VPN NANO PREMOD
        self.register_service(
            "flat-L2vpn-internal-local-site",
            flat_L2vpn_local_site_nano_services.FlatL2vpnCallBack,
        )
        self.register_service(
            "flat-L2vpn-internal-remote-site",
            flat_L2vpn_remote_site_nano_services.FlatL2vpnCallBack,
        )
        self.register_service(
            "flat-L2vpn-internal-site",
            flat_L2vpn_site_nano_services.FlatL2vpnCallBack,
        )

        # L2VPN INTERNAL NANO SERVICE
        self.register_nano_service(
            "flat-L2vpn-internal-local-site", "ncs:self", "ncs:ready",
            flat_L2vpn_local_site_nano_services.FlatL2vpnSelfCallBack,
        )
        self.register_nano_service(
            "flat-L2vpn-internal-local-site", "ncs:self",
            "cisco-flat-L2vpn-fp-local-site-nano-services:config-apply",
            flat_L2vpn_local_site_nano_services.FlatL2vpnSelfCallBack,
        )
        self.register_nano_service(
            "flat-L2vpn-internal-remote-site", "ncs:self",
            "cisco-flat-L2vpn-fp-remote-site-nano-services:config-apply",
            flat_L2vpn_remote_site_nano_services.FlatL2vpnSelfCallBack,
        )
        self.register_nano_service(
            "flat-L2vpn-internal-remote-site", "ncs:self", "ncs:ready",
            flat_L2vpn_remote_site_nano_services.FlatL2vpnSelfCallBack,
        )
        self.register_nano_service(
            "flat-L2vpn-internal-site", "ncs:self",
            "cisco-flat-L2vpn-fp-site-nano-services:config-apply",
            flat_L2vpn_site_nano_services.FlatL2vpnSelfCallBack,
        )
        self.register_nano_service(
            "flat-L2vpn-internal-site", "ncs:self", "ncs:ready",
            flat_L2vpn_site_nano_services.FlatL2vpnSelfCallBack,
        )

        # L2VPN Internal Validation
        self.flat_l2vpn_local_site_validation = validate_callback.ValPointRegistrar(
            self.log, "flat_L2vpn_internal_local_site_validation",
            "flat-L2vpn-internal-local-site-validation",
            flat_L2vpn_local_site_nano_services.FlatL2vpnValidator(self.log),
        )
        self.flat_l2vpn_remote_site_validation = validate_callback.ValPointRegistrar(
            self.log, "flat_L2vpn_internal_remote_site_validation",
            "flat-L2vpn-internal-remote-site-validation",
            flat_L2vpn_remote_site_nano_services.FlatL2vpnValidator(self.log),
        )
        self.flat_l2vpn_site_validation = validate_callback.ValPointRegistrar(
            self.log, "flat_L2vpn_internal_site_validation",
            "flat-L2vpn-internal-site-validation",
            flat_L2vpn_site_nano_services.FlatL2vpnValidator(self.log),
        )
        # L2VPN INTERNAL ACTIONS
        # L2VPN Cleanup Action
        self.register_action(
            "cisco-flat-L2vpn-fp-internal-cleanup", flat_L2vpn_actions.FlatL2vpnCleanupAction)
        # L2VPN Self Test Action
        self.register_action(
            "cisco-flat-L2vpn-fp-internal-self-test", flat_L2vpn_actions.FlatL2vpnSelfTest)
        # L2VPN Recovery Actions
        self.register_action(
            "cisco-flat-L2vpn-fp-internal-error-recovery",
            flat_L2vpn_actions.FlatL2vpnRecoveryAction)

    def teardown(self):
        self.flat_l2vpn_local_site_validation.cleanup()
        self.flat_l2vpn_remote_site_validation.cleanup()
        self.flat_l2vpn_site_validation.cleanup()
        self.log.info("cisco-flat-L2-vpn-fp-internal FINISHED")
