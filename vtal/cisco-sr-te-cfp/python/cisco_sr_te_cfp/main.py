# -*- mode: python; python-indent: 4 -*-
import ncs
from . import utils as Utils

# POLICY IMPORTS
from .cisco_sr_te_cfp_sr_policy import sr_policy_nano_plan as sr_policy_nano_plan
from .cisco_sr_te_cfp_sr_policy import sr_te_policy_actions as sr_te_policy_actions

# ODN IMPORTS
from .cisco_sr_te_cfp_sr_odn import sr_odn_nano_plan as sr_odn_nano_plan
from .cisco_sr_te_cfp_sr_odn import sr_te_odn_actions as sr_te_odn_actions

# Import other
from . import sr_te_actions
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("cisco-sr-te-cfp RUNNING")

        ############################################################################################

        # SR POLICY NANO PREMOD
        self.register_service(
            "sr-policies-external-servicepoint", sr_policy_nano_plan.SRPolicyCallBack
        )
        # SR POLICY EXTERNAL PLAN NANO
        self.register_nano_service(
            "sr-policies-external-servicepoint", "ncs:self", "ncs:init",
            sr_policy_nano_plan.SRPolicyExternalSelfCallback,
        )
        self.register_nano_service(
            "sr-policies-external-servicepoint", "ncs:self", "ncs:ready",
            sr_policy_nano_plan.SRPolicyExternalSelfCallback,
        )
        self.register_nano_service(
            "sr-policies-external-servicepoint",
            "cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end",
            "ncs:init", sr_policy_nano_plan.SRPolicyExternalHeadEndCallback,
        )
        self.register_nano_service(
            "sr-policies-external-servicepoint",
            "cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end",
            "cisco-sr-te-cfp-sr-policies-nano-plan-services:config-apply",
            sr_policy_nano_plan.SRPolicyExternalHeadEndCallback,
        )
        self.register_nano_service(
            "sr-policies-external-servicepoint",
            "cisco-sr-te-cfp-sr-policies-nano-plan-services:head-end",
            "ncs:ready", sr_policy_nano_plan.SRPolicyExternalHeadEndCallback,
        )
        # SR Policy Validation
        self.sr_policies_validation = validate_callback.ValPointRegistrar(
            self.log, "sr_policies_validation", "sr-policies-validation",
            sr_policy_nano_plan.SRPolicyValidator(self.log),
        )
        # SR Policy Internal Plan Change Action
        self.register_action(
            "policy-internal-plan-change-handler",
            sr_te_policy_actions.InternalPolicyPlanChangeHandler,
        )

        # SR POLICY EXTERNAL ACTIONS
        self.register_action(
            "sr-policy-global-self-test-actionpoint",
            sr_te_policy_actions.SRPolicyGlobalSelfTest,
        )

        # SR POLICY CREATE/UPDATE ERROR RECOVERY
        self.register_action(
            "cisco-sr-te-cfp-policy-error-recovery",
            sr_te_policy_actions.SrTePolicyRecoveryAction,
        )

        ############################################################################################

        # SR ODN NANO PREMOD
        self.register_service(
            "sr-odn-external-servicepoint", sr_odn_nano_plan.SROdnCallback
        )

        # SR ODN EXTERNAL PLAN NANO SELF
        self.register_nano_service(
            "sr-odn-external-servicepoint", "ncs:self", "ncs:ready",
            sr_odn_nano_plan.SROdnExternalSelfCallback,
        )
        # SR ODN EXTERNAL PLAN NANO HEAD-END
        self.register_nano_service(
            "sr-odn-external-servicepoint",
            "cisco-sr-te-cfp-sr-odn-nano-plan-services:head-end",
            "ncs:init", sr_odn_nano_plan.SROdnExternalHeadEndCallback,
        )
        self.register_nano_service(
            "sr-odn-external-servicepoint",
            "cisco-sr-te-cfp-sr-odn-nano-plan-services:head-end",
            "cisco-sr-te-cfp-sr-odn-nano-plan-services:config-apply",
            sr_odn_nano_plan.SROdnExternalHeadEndCallback,
        )
        self.register_nano_service(
            "sr-odn-external-servicepoint",
            "cisco-sr-te-cfp-sr-odn-nano-plan-services:head-end",
            "ncs:ready",
            sr_odn_nano_plan.SROdnExternalHeadEndCallback,
        )
        # SR ODN Validation
        self.sr_odn_validation = validate_callback.ValPointRegistrar(
            self.log, "sr_odn_validation", "sr-odn-validation",
            sr_odn_nano_plan.SROdnValidator(self.log),
        )
        # SR ODN Internal Plan Change Action
        self.register_action(
            "odn-internal-plan-change-handler",
            sr_te_odn_actions.InternalOdnPlanChangeHandler,
        )

        # SR ODN CREATE/UPDATE ERROR RECOVERY
        self.register_action(
            "cisco-sr-te-cfp-odn-error-recovery",
            sr_te_odn_actions.SrTeOdnRecoveryAction,
        )

        # SR ODN HEAD-END CREATE/UPDATE ERROR RECOVERY
        self.register_action(
            "cisco-sr-te-cfp-odn-head-end-error-recovery",
            sr_te_odn_actions.SrTeOdnHeadEndRecoveryAction,
        )

        ############################################################################################

        # SR TE ACTIONS
        self.register_action("cisco-sr-te-cfp-cleanup", sr_te_actions.SrTeCleanupAction)

        # SR TE RECOVERY ACTIONS
        self.register_action(
            "cisco-sr-te-cfp-error-recovery", sr_te_actions.SrTeServiceRecoveryAction
        )

        # SR TE CFP Config updates
        self.register_action(
            "update-sr-te-internal-cfp-configurations",
            sr_te_actions.UpdateInternalCfpConfigurations,
        )

        # Set global values
        Utils.is_lsa_setup()

    def teardown(self):
        self.sr_policies_validation.cleanup()
        self.sr_odn_validation.cleanup()
        self.log.info("cisco-sr-te-cfp FINISHED")
