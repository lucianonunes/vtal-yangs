# -*- mode: python; python-indent: 4 -*-
import ncs


# ODN IMPORTS
from .cisco_sr_te_cfp_sr_odn import sr_odn_nano_services as sr_odn_nano_services
from .cisco_sr_te_cfp_sr_policy import sr_policy_nano_services as sr_policy_nano_services
from .cisco_sr_te_cfp_sr_policy import sr_te_policy_actions as sr_te_policy_actions

# Import other
from . import sr_te_actions
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("cisco-sr-te-cfp-internal RUNNING")

        # SR Policy NANO PREMOD
        self.register_service("sr-policies-servicepoint", sr_policy_nano_services.SRPolicyCallback)
        # SR POLICY INTERNAL NANO SERVICE
        self.register_nano_service("sr-policies-servicepoint", "ncs:self",
                                   "cisco-sr-te-cfp-sr-policies-nano-services:config-apply",
                                   sr_policy_nano_services.SRPolicyHeadEndCallBack)
        self.register_nano_service("sr-policies-servicepoint", "ncs:self", "ncs:ready",
                                   sr_policy_nano_services.SRPolicyHeadEndCallBack)
        # SR Policy Validation
        self.sr_policies_validation = validate_callback.ValPointRegistrar(
            self.log, "sr_policies_internal_validation",
            "sr-policies-internal-validation",
            sr_policy_nano_services.SRPolicyValidator(self.log),
        )

        # SR ODN NANO PREMOD
        self.register_service("sr-odn-servicepoint", sr_odn_nano_services.SROdnCallback)
        # SR ODN INTERNAL NANO SERVICE
        self.register_nano_service("sr-odn-servicepoint", "ncs:self",
                                   "cisco-sr-te-cfp-sr-odn-nano-services:config-apply",
                                   sr_odn_nano_services.SROdnHeadEndCallBack)
        self.register_nano_service("sr-odn-servicepoint", "ncs:self", "ncs:ready",
                                   sr_odn_nano_services.SROdnHeadEndCallBack)
        # SR ODN Validation
        self.sr_odn_validation = validate_callback.ValPointRegistrar(
            self.log, "sr_odn_internal_validation", "sr-odn-internal-validation",
            sr_odn_nano_services.SROdnValidator(self.log),
        )

        # SR POLICY Internal ACTIONS
        self.register_action("sr-policy-internal-self-test-actionpoint",
                             sr_te_policy_actions.SRPolicyInternalSelfTest)

        ############################################################################################

        # SR TE ACTIONS
        self.register_action("cisco-sr-te-cfp-cleanup-internal", sr_te_actions.SrTeCleanupAction)

        # SR TE RECOVERY ACTIONS
        self.register_action("cisco-sr-te-cfp-error-recovery-internal",
                             sr_te_actions.SrTeServiceRecoveryAction)

    def teardown(self):
        self.sr_policies_validation.cleanup()
        self.sr_odn_validation.cleanup()
        self.log.info("cisco-sr-te-cfp-internal FINISHED")
