# -*- mode: python; python-indent: 4 -*-
import ncs
from . import utils as Utils
from . import te_tunnel_p2p as te_tunnel_p2p
from . import ietf_te_actions
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info("ietf-te-fp RUNNING")

        # Service callbacks require a registration for a 'service point',
        # as specified in the corresponding data model.
        #
        self.register_service("ietf-te-tunnel-p2p", te_tunnel_p2p.P2PTeTunnelCallBack)
        # IETF TE PLAN NANO SELF
        self.register_nano_service(
            "ietf-te-tunnel-p2p", "ncs:self", "ncs:ready", te_tunnel_p2p.P2PTeSelfTunnel
        )

        self.register_nano_service(
            "ietf-te-tunnel-p2p", "ietf-te-fp-tunnel-nano-plan-services:source",
            "ncs:init", te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "ietf-te-tunnel-p2p", "ietf-te-fp-tunnel-nano-plan-services:source",
            "ietf-te-fp-tunnel-nano-plan-services:config-apply",
            te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "ietf-te-tunnel-p2p", "ietf-te-fp-tunnel-nano-plan-services:source",
            "ncs:ready", te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "ietf-te-tunnel-p2p",
            "ietf-te-fp-tunnel-nano-plan-services:destination",
            "ncs:init", te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "ietf-te-tunnel-p2p", "ietf-te-fp-tunnel-nano-plan-services:destination",
            "ietf-te-fp-tunnel-nano-plan-services:config-apply",
            te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "ietf-te-tunnel-p2p", "ietf-te-fp-tunnel-nano-plan-services:destination",
            "ncs:ready", te_tunnel_p2p.P2PTeTunnel,
        )
        # IETF TE Validation
        self.ietf_te_validation = validate_callback.ValPointRegistrar(
            self.log,
            "ietf_te_validation", "ietf-te-validation",
            te_tunnel_p2p.IETFTEValidator(self.log),
        )

        # If we registered any callback(s) above, the Application class
        # took care of creating a daemon (related to the service/action point).
        # SR ODN Internal Plan Change Action
        self.register_action(
            "ietf-te-internal-plan-change-handler",
            ietf_te_actions.InternalPlanChangeHandler,
        )

        # SR TE CFP Config updates
        self.register_action(
            "ietf-te-internal-fp-configurations",
            ietf_te_actions.UpdateInternalCfpConfigurations,
        )
        # IETF TE FP Cleanup Action
        self.register_action("ietf-te-fp-cleanup", ietf_te_actions.IETFTeCleanupAction)

        # IETF TE FP Error-Recovery Action
        self.register_action(
            "cisco-ietf-te-fp-error-recovery", ietf_te_actions.IETFTeErrorRecovery
        )

        # IETF TE FP Service level Error-Recovery Action
        self.register_action(
            "cisco-ietf-te-fp-service-error-recovery",
            ietf_te_actions.IETFTeServiceErrorRecovery,
        )

        # IETF TE FP Self-Test
        self.register_action(
            "ietf-te-self-test-actionpoint", ietf_te_actions.IETFTeSelfTest
        )

        # Set global values
        Utils.is_lsa_setup()

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.

        self.ietf_te_validation.cleanup()
        self.log.info("ietf-te-fp FINISHED")
