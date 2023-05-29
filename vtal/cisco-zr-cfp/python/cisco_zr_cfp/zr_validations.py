import importlib

import _ncs
import ncs.maagic as maagic
import ncs.maapi as maapi

from cisco_ron_core_fp_common.status_codes import StatusCodes as codes
from .zr_errors import UserError
from cisco_ron_core_fp_common.constants import AsciiArt


class ZrDynamicMappingValidator:
    def __init__(self, log):
        self.log = log
        self.log.info("Initilizing zr-dco-dynamic-map-validation callback")

    def cb_validate(self, tctx, kp, newval):
        self.log.info(f"ENTRY_POINT  dynamic mapping validation callback on {kp}")
        self.log.info(AsciiArt.castle)

        try:
            th = maapi.Maapi().attach(tctx)
            mapping = maagic.get_node(th, kp)
            my_class = mapping.python_impl_class_name
            # NED ID matching
            self.log.info("Validating dynamic class {}".format(my_class))
            getattr(importlib.import_module(my_class), my_class.split(".")[-1])

        except ImportError as exp:
            self.log.error(AsciiArt.roadblock)
            raise UserError(
                self.log, codes.DYNAMIC_CLASS_NOT_FOUND, str(exp)
            ).set_context(
                "Dynamic Device Class",
                "Dynamic class {} not loaded into NSO".format(my_class),
            ).finish()
        except Exception:
            self.log.error(AsciiArt.roadblock)
            raise
        self.log.info(AsciiArt.castle)
        self.log.info(f"EXIT_POINT  dynamic mapping validation callback on {kp}")
        return _ncs.OK
