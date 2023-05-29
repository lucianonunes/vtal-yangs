# -*- mode: python; python-indent: 4 -*-
import ncs
from . import utils as Utils
from . import flat_L3vpn_nano_plan
from . import flat_L3vpn_actions
from . import flat_L3vpn_cfp_config
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("cisco-flat-L3-vpn-fp RUNNING")

        # L3VPN CFP CONFIG
        self.register_service(
            "flat-L3vpn-cfp-config", flat_L3vpn_cfp_config.FlatL3vpnCfgCallBack
        )

        # L3VPN NANO PREMOD
        self.register_service(
            "flat-L3vpn-external", flat_L3vpn_nano_plan.FlatL3vpnCallBack
        )

        # L3VPN EXTERNAL PLAN NANO
        self.register_nano_service(
            "flat-L3vpn-external", "ncs:self",
            "ncs:ready", flat_L3vpn_nano_plan.FlatL3vpnExternalSelfCallback,
        )
        self.register_nano_service(
            "flat-L3vpn-external", "cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
            "ncs:init", flat_L3vpn_nano_plan.FlatL3vpnExternalEndpointCallback,
        )
        self.register_nano_service(
            "flat-L3vpn-external",
            "cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
            "cisco-flat-L3vpn-fp-nano-plan-services:config-apply",
            flat_L3vpn_nano_plan.FlatL3vpnExternalEndpointCallback,
        )
        self.register_nano_service(
            "flat-L3vpn-external", "cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
            "ncs:ready", flat_L3vpn_nano_plan.FlatL3vpnExternalEndpointCallback,
        )
        # L3VPN Validation
        self.flat_l3vpn_validation = validate_callback.ValPointRegistrar(
            self.log, "flat_L3vpn_validation", "flat-L3vpn-validation",
            flat_L3vpn_nano_plan.FlatL3vpnValidator(self.log),
        )

        # L3VPN EXTERNAL ACTIONS
        self.register_action(
            "cisco-flat-L3vpn-fp-cleanup", flat_L3vpn_actions.FlatL3vpnCleanupAction
        )
        self.register_action(
            "flat-L3vpn-self-test-actionpoint", flat_L3vpn_actions.FlatL3vpnSelfTest
        )
        self.register_action(
            "update-l3vpn-internal-cfp-configurations",
            flat_L3vpn_actions.FlatL3vpnUpdateInternalCfpConfigurations,
        )
        self.register_action(
            "cisco-flat-L3vpn-fp-error-recovery",
            flat_L3vpn_actions.FlatL3vpnRecoveryAction,
        )
        self.register_action(
            "cisco-flat-L3vpn-fp-service-error-recovery",
            flat_L3vpn_actions.FlatL3vpnServiceRecoveryAction,
        )
        self.register_action(
            "cisco-flat-L3vpn-fp-endpoint-error-recovery",
            flat_L3vpn_actions.FlatL3vpnEndpointRecoveryAction,
        )

        # L3VPN INTERNAL PLAN CHANGE ACTION
        self.register_action(
            "l3vpn-internal-plan-change-handler",
            flat_L3vpn_actions.InternalL3vpnPlanChangeHandler,
        )

        # Global Setup
        Utils.is_lsa_setup()

    def teardown(self):
        self.flat_l3vpn_validation.cleanup()
        self.log.info("cisco-flat-L3-vpn-fp FINISHED")
