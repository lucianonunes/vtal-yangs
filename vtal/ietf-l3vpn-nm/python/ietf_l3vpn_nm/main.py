# -*- mode: python; python-indent: 4 -*-
from ncs.application import Application
from . import utils as Utils
from . import ietf_l3vpn_nm_route_policy_subscriber
from ietf_l3vpn_nm.ietf_l3vpn_nm_nano_plan import IETFL3vpnNMVpnNodeCallback, IETFL3vpnNMCallback, \
    IETFL3vpnNMSelfCallback, IETFL3vpnNMValidator
from ietf_l3vpn_nm.ietf_l3vpn_nm_actions import L3NMInternalPlanChangeHandler, L3NMCleanupAction, \
    L3NMSelfTest, L3NMRecoveryAction, L3NMServiceRecoveryAction, L3NMEndpointRecoveryAction
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(Application):
    def setup(self):
        self.log.info("ietf-L3vpn-nm-fp RUNNING")

        # IETF L3NM SERVICE PREMOD
        self.register_service("ietf-l3vpn-ntw-servicepoint", IETFL3vpnNMCallback)

        # IETF L3NM PLAN NANO
        self.register_nano_service(
            "ietf-l3vpn-ntw-servicepoint",
            "ncs:self",
            "ietf-l3vpn-ntw-nano-services:config-apply",
            IETFL3vpnNMSelfCallback,
        )
        self.register_nano_service(
            "ietf-l3vpn-ntw-servicepoint",
            "ncs:self",
            "ncs:ready",
            IETFL3vpnNMSelfCallback,
        )
        self.register_nano_service(
            "ietf-l3vpn-ntw-servicepoint",
            "ietf-l3vpn-ntw-nano-services:vpn-node",
            "ncs:init",
            IETFL3vpnNMVpnNodeCallback,
        )
        self.register_nano_service(
            "ietf-l3vpn-ntw-servicepoint",
            "ietf-l3vpn-ntw-nano-services:vpn-node",
            "ietf-l3vpn-ntw-nano-services:config-apply",
            IETFL3vpnNMVpnNodeCallback,
        )
        self.register_nano_service(
            "ietf-l3vpn-ntw-servicepoint",
            "ietf-l3vpn-ntw-nano-services:vpn-node",
            "ncs:ready",
            IETFL3vpnNMVpnNodeCallback,
        )
        # IETF TE Validation
        self.ietf_l3vpn_nm_validation = validate_callback.ValPointRegistrar(
            self.log,
            "ietf_l3vpn_nm_validation",
            "ietf-l3vpn-nm-validation",
            IETFL3vpnNMValidator(self.log),
        )
        # IETF L3NM Actions
        self.register_action(
            "ietf-l3vpn-nm-internal-plan-change-handler", L3NMInternalPlanChangeHandler
        )
        self.register_action("ietf-l3vpn-nm-cleanup", L3NMCleanupAction)
        self.register_action("ietf-l3vpn-nm-self-test-actionpoint", L3NMSelfTest)
        self.register_action("ietf-l3vpn-nm-fp-error-recovery", L3NMRecoveryAction)
        self.register_action(
            "ietf-l3vpn-nm-vpn-node-error-recovery", L3NMEndpointRecoveryAction
        )
        self.register_action(
            "ietf-l3vpn-nm-service-error-recovery", L3NMServiceRecoveryAction
        )
        self.l3vpn_route_policy_sub = (ietf_l3vpn_nm_route_policy_subscriber.
                                       L3vpnRoutePolicySubscriber(self))
        self.l3vpn_route_policy_sub.start()

        # Set global values
        Utils.is_lsa_setup()

    def teardown(self):
        self.ietf_l3vpn_nm_validation.cleanup()
        self.l3vpn_route_policy_sub.stop()
        self.log.info("Main FINISHED")
