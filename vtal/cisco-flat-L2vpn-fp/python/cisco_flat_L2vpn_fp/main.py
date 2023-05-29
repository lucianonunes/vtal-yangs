# -*- mode: python; python-indent: 4 -*-
import ncs
from . import utils as Utils
from . import flat_L2vpn_nano_plan as flat_L2vpn_nano_plan
from . import flat_L2vpn_actions as flat_L2vpn_actions
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("cisco-flat-L2-vpn-fp RUNNING")

        # L2VPN NANO PREMOD
        self.register_service(
            "flat-L2vpn-external", flat_L2vpn_nano_plan.FlatL2vpnCallBack
        )

        self.register_nano_service(
            "flat-L2vpn-external", "ncs:self", "ncs:ready",
            flat_L2vpn_nano_plan.FlatL2vpnExternalSelfCallback,
        )

        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:local-site",
            "ncs:init", flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:local-site",
            "cisco-flat-L2vpn-fp-nano-plan-services:config-apply",
            flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:local-site",
            "ncs:ready", flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )

        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
            "ncs:init", flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        self.register_nano_service(
            "flat-L2vpn-external",
            "cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
            "cisco-flat-L2vpn-fp-nano-plan-services:config-apply",
            flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
            "ncs:ready", flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )

        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:site",
            "ncs:init", flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        self.register_nano_service(
            "flat-L2vpn-external",
            "cisco-flat-L2vpn-fp-nano-plan-services:site",
            "cisco-flat-L2vpn-fp-nano-plan-services:config-apply",
            flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        self.register_nano_service(
            "flat-L2vpn-external", "cisco-flat-L2vpn-fp-nano-plan-services:site",
            "ncs:ready", flat_L2vpn_nano_plan.FlatL2vpnExternalSiteCallback,
        )
        # L2VPN Validation
        self.flat_L2vpn_validation = validate_callback.ValPointRegistrar(
            self.log, "flat_L2vpn_validation", "flat-L2vpn-validation",
            flat_L2vpn_nano_plan.FlatL2vpnValidator(self.log),
        )

        # L2VPM Internal Plan Change Action
        self.register_action(
            "l2vpn-internal-plan-change-handler",
            flat_L2vpn_actions.L2vpnInternalPlanChangeHandler,
        )
        # L2VPN CFP Config updates
        self.register_action(
            "update-l2vpn-internal-cfp-configurations",
            flat_L2vpn_actions.UpdateInternalCfpConfigurations,
        )

        # L2VPN EXTERNAL ACTIONS
        # L2VPN Cleaup Action
        self.register_action(
            "cisco-flat-L2vpn-fp-cleanup", flat_L2vpn_actions.FlatL2vpnCleanupAction
        )
        # L2VPN Self Test
        self.register_action(
            "flat-L2vpn-cfs-self-test-actionpoint",
            flat_L2vpn_actions.FlatL2vpnCFSSelfTest,
        )
        # L2VPN Recovery Actions
        self.register_action(
            "cisco-flat-L2vpn-fp-error-recovery",
            flat_L2vpn_actions.FlatL2vpnRecoveryAction,
        )
        self.register_action(
            "cisco-flat-L2vpn-fp-site-error-recovery",
            flat_L2vpn_actions.FlatL2vpnSiteRecoveryAction,
        )

        # Set global values
        Utils.is_lsa_setup()

    def teardown(self):
        self.flat_L2vpn_validation.cleanup()
        self.log.info("cisco-flat-L2-vpn-fp FINISHED")
