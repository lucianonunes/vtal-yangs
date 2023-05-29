# -*- mode: python; python-indent: 4 -*-
import ncs
from . import ron as ron
from . import ron_actions
from . import mg_lnk_db_validation
from .constants import StrConstants
from .ron_validations import RonMlValidator
from .namespaces.ciscoRonCfp_ns import ns as ronml_ns
from cisco_ron_core_fp_common import validate_callback
from cisco_ron_core_fp_common.constants import AsciiArt
from . import safecollections
from . import ron_lsa_handlers
from . import ron_kicker_callbacks


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------
class Main(ncs.application.Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info(AsciiArt.booting)

        # Service callbacks require a registration for a 'service point',
        # as specified in the corresponding data model.
        #
        self.register_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint, ron.RonMlCallBack
        )

        # Register inter-layer-link service-callback
        self.register_service(
            ronml_ns.servicepoint_cisco_ron_ml_mgdb, mg_lnk_db_validation.ManagedLinkDB
        )

        ## Self
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.ncs_self,
            StrConstants.ncs_init,
            ron.RonMlSelf,
        )
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.ncs_self,
            StrConstants.ncs_ready,
            ron.RonMlSelf,
        )

        ## Optical COntroller
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.com_optical_controller,
            StrConstants.ncs_init,
            ron.RonMlOptical,
        )
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.com_optical_controller,
            StrConstants.state_conf_apply,
            ron.RonMlOptical,
        )
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.com_optical_controller,
            StrConstants.ncs_ready,
            ron.RonMlOptical,
        )

        ## Router
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.com_router,
            StrConstants.ncs_init,
            ron.RonMlRouter,
        )
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.com_router,
            StrConstants.state_conf_apply,
            ron.RonMlRouter,
        )
        self.register_nano_service(
            ronml_ns.servicepoint_cisco_ron_ml_servicepoint,
            StrConstants.com_router,
            StrConstants.ncs_ready,
            ron.RonMlRouter,
        )

        ## RON Validation
        self.ron_validation = validate_callback.ValPointRegistrar(
            self.log,
            "ron_validation",
            ronml_ns.validate_cisco_ron_ml_validation,
            RonMlValidator(self.log),
        )

        ## RON ACTIONS
        self.register_action(
            ronml_ns.actionpoint_cisco_ron_cfp_validate_service_input,
            ron_actions.ValidateServiceInput,
        )
        self.register_action(
            ronml_ns.actionpoint_cisco_ron_cfp_cleanup, ron_actions.RonCleanupAction
        )
        self.register_action(
            ronml_ns.actionpoint_ron_ml_redeploy_kicker_callback,
            ron_actions.MLRedeployKickerCallback,
        )
        self.register_action(
            ronml_ns.actionpoint_ron_ml_process_failure, ron_actions.RonFailureHandle
        )

        self.register_action(
            ronml_ns.actionpoint_cisco_ron_cfp_error_recovery, ron_actions.ErrorRecovery
        )
        self.register_action(
            ronml_ns.actionpoint_cisco_ron_cfp_list_all_sips,
            ron_actions.ListAllSips,
        )
        self.register_action(
            ronml_ns.actionpoint_cisco_ron_cfp_validate_inter_layer_links,
            ron_actions.ValidateInterLayerLinks,
        )

        # Plan failed kicker callback to adjust plan/self/ready state
        cb_sockets = dict()  # Dict to store the notification socket
        self.notif_reg = ron_kicker_callbacks.NotifStreamRegistrar(self.log, cb_sockets)
        self.register_action(
            ronml_ns.actionpoint_ron_ml_plan_failure_report_kicker_callback,
            ron_kicker_callbacks.MLPlanFailedKickerCallback, cb_sockets
        )

        # REMOT PLAN CALLBACK HANDLER

        self.sync_set = safecollections.SynchronizedSet()
        self.register_action(
            ronml_ns.actionpoint_cisco_ron_cfp_remote_plan_change_callback,
            ron_lsa_handlers.RemotePlanChangeCallback,
            self.sync_set,
        )
        self.remote_change_handler_t = ron_lsa_handlers.RemotePlanChangeHandlerThread(
            self.log, self.sync_set
        )
        self.remote_change_handler_t.start()

        self.log.info(AsciiArt.up)

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.
        self.sync_set.abort()
        self.ron_validation.cleanup()
        self.notif_reg.cleanup()
        self.log.info(AsciiArt.goodbye)
