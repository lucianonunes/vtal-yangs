# -*- mode: python; python-indent: 4 -*-
import ncs
from cisco_flat_L3vpn_internal import flat_L3vpn_nano_services, flat_L3vpn_actions
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("cisco-flat-L3-vpn-fp-internal RUNNING")

        # L3VPN NANO PREMOD
        self.register_service("flat-L3vpn", flat_L3vpn_nano_services.FlatL3vpnCallBack)

        # L3VPN INTERNAL NANO SERVICE
        self.register_nano_service(
            "flat-L3vpn", "ncs:self",
            "cisco-flat-L3vpn-fp-nano-services:config-apply",
            flat_L3vpn_nano_services.FlatL3vpnEndpointCallBack,
        )
        self.register_nano_service(
            "flat-L3vpn", "ncs:self", "ncs:ready",
            flat_L3vpn_nano_services.FlatL3vpnEndpointCallBack,
        )
        # L3VPN Internal Validation
        self.flat_L3vpn_validation = validate_callback.ValPointRegistrar(
            self.log, "flat_L3vpn_internal_validation",
            "flat-L3vpn-internal-validation",
            flat_L3vpn_nano_services.FlatL3vpnValidator(self.log),
        )

        # L3VPN INTERNAL ACTIONS
        self.register_action(
            "flat-L3vpn-internal-self-test-actionpoint",
            flat_L3vpn_actions.FlatL3vpnInternalSelfTest,
        )
        self.register_action(
            "cisco-flat-L3vpn-fp-cleanup-internal",
            flat_L3vpn_actions.FlatL3vpnCleanupAction,
        )
        self.register_action(
            "cisco-flat-L3vpn-fp-error-recovery-internal",
            flat_L3vpn_actions.FlatL3vpnServiceRecoveryAction,
        )

    def teardown(self):
        self.flat_L3vpn_validation.cleanup()
        self.log.info("cisco-flat-L3vpn-fp-internal FINISHED")
