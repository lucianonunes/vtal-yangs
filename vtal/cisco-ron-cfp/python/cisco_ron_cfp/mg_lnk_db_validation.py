# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs
from .ron_ml_errors import UserError, RonMLException
from cisco_ron_core_fp_common import utils as RonUtils
from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes

# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class ManagedLinkDB(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        op_type = RonUtils.get_service_operation(op)
        self.log.info(
            f"ENTRY_POINT for {RonUtils.get_kp_service_id(kp)} at pre_mod of "
            f"Managed-link-db aka inter-layer-link, operation: {op_type}"
        )
        try:
            if op == _ncs.dp.NCS_SERVICE_DELETE or op == _ncs.dp.NCS_SERVICE_UPDATE:
                text = "Update"
                if op == _ncs.dp.NCS_SERVICE_UPDATE:
                    self.log.info(AsciiArt.caterpillar)
                else:
                    self.log.info(AsciiArt.blackwidow)
                    text = "Delete"

                mg_lnk_db_oper = root.cisco_ron_cfp__ron.inter_layer_link_oper
                key = (str(kp[0][0]), str(kp[0][1]))
                if key in mg_lnk_db_oper:
                    service = mg_lnk_db_oper[key].ron_ml_service
                    raise UserError(
                        self.log,
                        StatusCodes.INTER_LAYER_LINK_DB_IN_USE,
                        f"{text} on inter-layer-link {key} is "
                        f"not allowed at this moment, it in use by ron-ml {service} service.",
                    ).set_context("inter-layer-link db edit").add_state(
                        "Opertion", text
                    ).add_state(
                        "Keypath", str(kp)
                    ).add_state(
                        "used-by", service
                    ).finish()

            else:
                self.log.info(AsciiArt.torpedo)

        except UserError:
            self.log.error(AsciiArt.roadblock)
            raise

        except Exception as exp1:
            self.log.error(AsciiArt.roadblock)
            raise RonMLException(
                self.log, StatusCodes.PRE_MOD_CALLBACK_ERROR, str(exp1)
            ).set_context(
                "pre-modification", "Failed to process pre-modification call-back"
            ).add_state(
                "service", str(kp)
            ).finish()

        self.log.info(
            f"EXIT_POINT for {RonUtils.get_kp_service_id(kp)} at pre_mod of "
            f"Managed-link-db aka inter-layer-link, operation: {op_type}"
        )

        return proplist

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info("Service create(service=", service._path, ")")
        return proplist

    @ncs.application.Service.post_modification
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            f"ENTRY_POINT for {RonUtils.get_kp_service_id(kp)} at post_mod of "
            f"Managed-link-db aka inter-layer-link, operation: {RonUtils.get_service_operation(op)}"
        )
        self.log.info(AsciiArt.hibernate)
        self.log.info(
            f"EXIT_POINT for {RonUtils.get_kp_service_id(kp)} at post_mod of "
            f"Managed-link-db aka inter-layer-link, operation: {RonUtils.get_service_operation(op)}"
        )
        return proplist
