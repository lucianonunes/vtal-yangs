from ncs.application import Application
from . import utils as Utils
from . import ietf_l2vpn_nm_nano_plan as ietf_l2vpn_nm_nano_plan
from . import ietf_l2vpn_nm_actions as ietf_l2vpn_nm_actions
from . import ietf_l2vpn_nm_y1731 as y1731
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(Application):
    def setup(self):
        self.log.info('ietf-L2vpn-nm-fp Main RUNNING')

        # IETF L2NM SERVICE PREMOD
        self.register_service("ietf-l2vpn-ntw-servicepoint",
                              ietf_l2vpn_nm_nano_plan.IETFL2vpnNMCallback)

        # L2NM Global Y-1731 Servicepoint
        self.register_service("l2vpn-ntw-augmentations-y1731-servicepoint",
                              y1731.Y1731Callback)
        # IETF L2NM PLAN NANO
        self.register_nano_service("ietf-l2vpn-ntw-servicepoint",
                                   "ncs:self",
                                   "ncs:ready",
                                   ietf_l2vpn_nm_nano_plan.IETFL2vpnNMSelfCallback)
        self.register_nano_service("ietf-l2vpn-ntw-servicepoint",
                                   "ietf-l2vpn-ntw-nano-services:vpn-node",
                                   "ncs:init",
                                   ietf_l2vpn_nm_nano_plan.IETFL2vpnNMVpnNodeCallback)
        self.register_nano_service("ietf-l2vpn-ntw-servicepoint",
                                   "ietf-l2vpn-ntw-nano-services:vpn-node",
                                   "ietf-l2vpn-ntw-nano-services:config-apply",
                                   ietf_l2vpn_nm_nano_plan.IETFL2vpnNMVpnNodeCallback)
        self.register_nano_service("ietf-l2vpn-ntw-servicepoint",
                                   "ietf-l2vpn-ntw-nano-services:vpn-node",
                                   "ncs:ready",
                                   ietf_l2vpn_nm_nano_plan.IETFL2vpnNMVpnNodeCallback)
        # IETF L2VPN NM Validation
        self.ietf_l2nm_validation = validate_callback.\
            ValPointRegistrar(self.log, "ietf_l2vpn_nm_validation",
                              "ietf-l2vpn-nm-validation",
                              ietf_l2vpn_nm_nano_plan.IETFL2vpnNMValidator(self.log))
        # IETF L2NM Actions
        self.register_action("ietf-l2vpn-nm-internal-plan-change-handler",
                             ietf_l2vpn_nm_actions.L2NMInternalPlanChangeHandler)
        self.register_action("ietf-l2vpn-nm-cleanup",
                             ietf_l2vpn_nm_actions.L2NMCleanupAction)
        self.register_action("ietf-l2vpn-nm-self-test-actionpoint",
                             ietf_l2vpn_nm_actions.L2NMSelfTest)
        self.register_action("ietf-l2vpn-nm-fp-error-recovery",
                             ietf_l2vpn_nm_actions.L2NMRecoveryAction)
        self.register_action("ietf-l2vpn-nm-vpn-node-error-recovery",
                             ietf_l2vpn_nm_actions.L2NMSiteRecoveryAction)

        # Set global values
        Utils.is_lsa_setup()

    def teardown(self):
        self.ietf_l2nm_validation.cleanup()
        self.log.info('ietf-L2vpn-nm-fp Main FINISHED')
