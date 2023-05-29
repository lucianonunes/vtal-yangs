# -*- mode: python; python-indent: 4 -*-
import ncs
import _ncs

from core_fp_common import common_utils
from cisco_ron_core_fp_common import utils as RonUtils
from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes
from .utils import get_device_ned_id

from .optical_errors import OpticalCfpException

# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class OncNotificationSubscriber(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            "ENTRY_POINT for {} at pre_mod of netconf-subscription, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        try:
            ## If operation is create or update, check interface Loopback on device
            if op != _ncs.dp.NCS_SERVICE_DELETE:
                ## If op is create, validate status code mapping is loaded
                if op == _ncs.dp.NCS_SERVICE_CREATE:
                    self.log.info(AsciiArt.torpedo)
                else:
                    th = ncs.maagic.get_trans(root)
                    th.diff_iterate(self.diter, 0)

                    if self.is_redeploy:
                        self.log.info(AsciiArt.caterpillar)
                    else:
                        self.log.info(AsciiArt.wave)
            else:
                self.log.info(AsciiArt.blackwidow)
        except Exception as exp1:
            self.log.error(AsciiArt.roadblock)
            raise OpticalCfpException(
                self.log, StatusCodes.OP_FP_PRE_MOD_CALLBACK_ERROR, str(exp1)
            ).set_context(
                "pre-modification", "Failed to process pre-modification call-back"
            ).add_state(
                "service", str(kp)
            ).finish()

        self.log.info(
            "EXIT_POINT for {} at pre_mod of netconf-subscription, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        return proplist

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info(f"ENTRY_POINT for notification subscription create {service.onc}")
        t_vars = ncs.template.Variables()
        template = ncs.template.Template(service)
        l_user = common_utils.get_local_user()
        ned_id = get_device_ned_id(root, service.onc)
        if not ned_id or "onf-tapi-nc-1.0" in str(ned_id):
            t_vars.add("STREAM", "NETCONF")
        else:
            t_vars.add("STREAM", "CONC_NETCONF")
        t_vars.add("USER", l_user)
        template.apply("cisco-optical-cfp-onc-netconf-subscription", t_vars)
        self.log.info(f"EXIT_POINT for notification subscription create {service.onc}")
        return proplist

    def diter(self, keypath, op, oldv, newv):
        is_key_path0_tuple = isinstance(keypath[0], tuple)
        key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])

        # Check if redeploy
        if not is_key_path0_tuple and key_path0 == "re-deploy-counter":
            self.is_redeploy = True
            return ncs.ITER_STOP
        else:
            return ncs.ITER_RECURSE

    @ncs.application.Service.post_modification
    def cb_post_modification(self, tctx, op, kp, root, proplist):
        self.log.info(
            "ENTRY_POINT for {} at post_mod of netconf-subscription, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        self.log.info(AsciiArt.hibernate)
        self.log.info(
            "EXIT_POINT for {} at post_mod of netconf-subscription, operation: {}".format(
                RonUtils.get_kp_service_id(kp), RonUtils.get_service_operation(op)
            )
        )
        return proplist
