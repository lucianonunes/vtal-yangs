import ncs
from . import utils as Utils
from core_fp_common import instrumentation
import logging


class Y1731Callback(ncs.application.Service):
    @ncs.application.Service.create
    @instrumentation.instrument_service(logging.INFO,
                                        Utils.l2vpn_ntw_augmentations_y1731_servicepoint)
    def cb_create(self, tctx, root, service, proplist):

        template = ncs.template.Template(service)
        template.apply("ietf-l2nm-y1731-l2vpn-y1731-template")

        return proplist
