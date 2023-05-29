# -*- mode: python; python-indent: 4 -*-
import ncs
from . import optical as optical
from . import onc_notif_sub as subscription
from . import optical_actions
from . import optical_validations
from .namespaces.tapiConnectivity_ns import ns as tapicfp_ns
from .constants import StrConstants as str_con
from cisco_ron_core_fp_common import validate_callback
from cisco_ron_core_fp_common.constants import AsciiArt


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
            tapicfp_ns.servicepoint_tapi_connectivity_service_servicepoint,
            optical.OpticalConnectivityCallBack,
        )

        self.register_service(
            tapicfp_ns.servicepoint_cisco_optical_cfp_onc_notification,
            subscription.OncNotificationSubscriber,
        )

        # Nano callback registrations
        self.register_nano_service(
            tapicfp_ns.servicepoint_tapi_connectivity_service_servicepoint,
            str_con.ncs_self,
            str_con.ncs_init,
            optical.OpticalConnectivity,
        )
        self.register_nano_service(
            tapicfp_ns.servicepoint_tapi_connectivity_service_servicepoint,
            str_con.ncs_self,
            str_con.state_config_apply,
            optical.OpticalConnectivity,
        )
        self.register_nano_service(
            tapicfp_ns.servicepoint_tapi_connectivity_service_servicepoint,
            str_con.ncs_self,
            str_con.state_installed,
            optical.OpticalConnectivity,
        )
        self.register_nano_service(
            tapicfp_ns.servicepoint_tapi_connectivity_service_servicepoint,
            str_con.ncs_self,
            str_con.ncs_ready,
            optical.OpticalConnectivity,
        )

        ## Optical Validation
        self.optical_dymap_val = validate_callback.ValPointRegistrar(
            self.log,
            "tapi_dynamic_map_validation",
            tapicfp_ns.validate_tapi_dynamic_map_validation,
            optical_validations.TapiDynamicMappingValidator(self.log),
        )

        ## Optical ACTIONS
        self.register_action(
            tapicfp_ns.actionpoint_tapi_connectivity_service_cleanup_internal,
            optical_actions.OpticalConnectivityCleanupAction,
        )
        self.register_action(
            tapicfp_ns.actionpoint_tapi_connectivity_service_error_recovery_internal,
            optical_actions.OpticalConnectivityRecovery,
        )

        self.register_action(
            tapicfp_ns.actionpoint_cisco_optical_cs_netconf_notifications,
            optical_actions.NotificationHandler,
        )
        self.register_action(
            tapicfp_ns.actionpoint_cisco_optical_cs_redeploy_kicker_callback,
            optical_actions.RedeployKickerCallback,
        )
        self.register_action(
            tapicfp_ns.actionpoint_tapi_connectivity_service_fetch_sip,
            optical_actions.OpticalFetchSip,
        )
        self.register_action(
            tapicfp_ns.actionpoint_cisco_optical_cs_oper_data_purge,
            optical_actions.Purge,
        )
        self.register_action(
            tapicfp_ns.actionpoint_tapi_connectivity_service_netconf_subscription_action,
            optical_actions.Subscription,
        )
        self.register_action(
            tapicfp_ns.actionpoint_tapi_connectivity_service_list_sip,
            optical_actions.OpticalListSip,
        )
        self.register_action(
            tapicfp_ns.actionpoint_tapi_connectivity_service_validate_sip,
            optical_actions.OpticalValidateSip,
        )

        self.log.info(AsciiArt.up)

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.
        self.optical_dymap_val.cleanup()
        self.log.info(AsciiArt.goodbye)
