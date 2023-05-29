# -*- mode: python; python-indent: 4 -*-
import ncs
from . import zr as zr
from . import zr_validations
from . import zr_actions
from .namespaces.ciscoZrCfp_ns import ns as zrcfp_ns
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
            zrcfp_ns.servicepoint_cisco_zr_dco_servicepoint, zr.ZrDcoCallBack
        )

        self.register_nano_service(
            zrcfp_ns.servicepoint_cisco_zr_dco_servicepoint,
            str_con.ncs_self,
            str_con.state_config_apply,
            zr.ZrDco,
        )
        self.register_nano_service(
            zrcfp_ns.servicepoint_cisco_zr_dco_servicepoint,
            str_con.ncs_self,
            str_con.ncs_ready,
            zr.ZrDco,
        )
        ## ZR Validation
        self.zr_dymap_val = validate_callback.ValPointRegistrar(
            self.log,
            "dynamic_map_validation",
            zrcfp_ns.validate_zr_dco_dynamic_map_validation,
            zr_validations.ZrDynamicMappingValidator(self.log),
        )

        ## ZR ACTIONS
        self.register_action(
            zrcfp_ns.actionpoint_cisco_zr_dco_cfp_fetch_optics_data,
            zr_actions.ZrDcoFetchOpticsData,
        )
        self.register_action(
            zrcfp_ns.actionpoint_cisco_zr_dco_cfp_cleanup_internal,
            zr_actions.ZrDcoCleanupAction,
        )
        self.register_action(
            zrcfp_ns.actionpoint_cisco_zr_dco_cfp_error_recovery_internal,
            zr_actions.ZrDcoRecovery,
        )
        self.register_action(
            zrcfp_ns.actionpoint_cisco_zr_dco_cfp_zombie_clean_sync_internal,
            zr_actions.ZrDcoZombieRedeploySync,
        )

        self.log.info(AsciiArt.up)

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.
        self.zr_dymap_val.cleanup()
        self.log.info(AsciiArt.goodbye)
