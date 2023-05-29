# -*- mode: python; python-indent: 4 -*-
import ncs
from . import rsvp_te_tunnel_p2p as rsvp_te_tunnel_p2p
from . import rsvp_te_actions
from cisco_tsdn_core_fp_common import validate_callback

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info("cisco-rsvp-te-fp RUNNING")

        # Service callbacks require a registration for a 'service point',
        # as specified in the corresponding data model.
        #
        self.register_service(
            "rsvp-te-servicepoint", rsvp_te_tunnel_p2p.P2PTeTunnelCallBack
        )
        self.register_nano_service(
            "rsvp-te-servicepoint", "ncs:self", "ncs:ready",
            rsvp_te_tunnel_p2p.P2PTeSelfTunnel,
        )
        self.register_nano_service(
            "rsvp-te-servicepoint", "cisco-rsvp-te-fp-nano-plan-services:head-end",
            "ncs:init", rsvp_te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "rsvp-te-servicepoint", "cisco-rsvp-te-fp-nano-plan-services:head-end",
            "cisco-rsvp-te-fp-nano-plan-services:config-apply",
            rsvp_te_tunnel_p2p.P2PTeTunnel,
        )
        self.register_nano_service(
            "rsvp-te-servicepoint", "cisco-rsvp-te-fp-nano-plan-services:head-end",
            "ncs:ready", rsvp_te_tunnel_p2p.P2PTeTunnel,
        )
        #  RSVP TE Validation
        self.rsvp_te_validation = validate_callback.ValPointRegistrar(
            self.log, "rsvp_te_validation", "rsvp-te-validation",
            rsvp_te_tunnel_p2p.RSVPTEValidator(self.log),
        )

        #  RSVP TE ACTIONS
        self.register_action(
            "cisco-rsvp-te-fp-cleanup-internal", rsvp_te_actions.RsvpTeCleanupAction
        )
        self.register_action(
            "cisco-rsvp-te-fp-error-recovery-internal", rsvp_te_actions.RsvpTeRecovery
        )
        self.register_action(
            "cisco-rsvp-te-self-test-actionpoint", rsvp_te_actions.RsvpTeSelfTest
        )

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.
        self.rsvp_te_validation.cleanup()
        self.log.info("cisco-rsvp-te-fp FINISHED")
