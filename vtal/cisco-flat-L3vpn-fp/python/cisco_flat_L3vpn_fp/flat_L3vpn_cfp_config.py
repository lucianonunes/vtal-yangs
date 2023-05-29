# -*- mode: python; python-indent: 4 -*-
import ncs
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import (
    UserErrorException,
)


class FlatL3vpnCfgCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, opaque):
        # Only allow changes if there is no outstanding L3VPN service
        l3vpn_service_list = root.cisco_flat_L3vpn_fp__flat_L3vpn

        if "global-rd-enabled" in str(kp):
            if len(l3vpn_service_list) > 0:
                raise UserErrorException(
                    self.log, StatusCodes.GLOBAL_RD_EXIST
                ).set_context(
                    "Validation",
                    "Cannot change global-rd-enabled"
                    " when L3VPN services already exist.",
                ).add_state(
                    "No. of existing L3VPN services", len(l3vpn_service_list)
                ).add_state(
                    "Keypath", str(kp)
                ).finish()

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, opaque):
        return opaque
