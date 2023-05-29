import ncs


class TMTCCfgCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, opaque):
        # Only allow changes if there is no outstanding TMTC service
        tm_tc_service_list = root.cisco_tm_tc_fp__tm_tc
        if len(tm_tc_service_list) > 0:
            raise Exception("Cannot change stacked-service-enabled" \
                            + " when TMTC services already exist.")

    @ncs.application.Service.create
    def cb_create(self, tctx, root, service, opaque):
        return opaque
