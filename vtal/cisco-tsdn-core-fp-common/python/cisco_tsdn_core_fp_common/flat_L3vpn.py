import ncs

from cisco_tsdn_core_fp_common import utils as utils
from cisco_tsdn_core_fp_common.cnc_service import CncService
import re


is_lsa = False


class FlatL3vpn(CncService):
    def __init__(self, path, log):
        super().__init__(path)
        self.log = log

    def redeploy(self):
        # Skip re-deploy if internal plan error-info/message matched NB plan error-info/message
        # to avoid re-deploy loops on failure
        if utils.match_plan_err_msg(self.plan_path, self.service_name,
                                    self.internal_plan_path, self.service_key) and \
                utils.is_failed_plan_converged(self.plan_path, self.service_name, self.log):
            return

        utils.redeploy_if_needed(self)

    @staticmethod
    def get_service_name_from_path(path):
        return re.search("flat-L3vpn-plan{(.*?) (.*?)}", path).group(1)

    @staticmethod
    def get_service_key_from_path(path):
        re_search = re.search("flat-L3vpn-plan{(.*?) (.*?)}", path)
        return re_search.group(1), re_search.group(2)

    @staticmethod
    def get_service_kp(l3vpn_service_name):
        return f"/cisco-flat-L3vpn-fp:flat-L3vpn{{{l3vpn_service_name}}}"

    @staticmethod
    def get_service_xpath(l3vpn_service_name):
        if is_lsa:
            return f"/flat-L3vpn[name='{l3vpn_service_name}']"
        else:
            return f"/cisco-flat-L3vpn-fp:flat-L3vpn[name='{l3vpn_service_name}']"

    @staticmethod
    def get_plan_kp(l3vpn_service_name):
        return f"/cisco-flat-L3vpn-fp:flat-L3vpn-plan{{{l3vpn_service_name}}}"

    @staticmethod
    def get_plan_path():
        return "/cisco-flat-L3vpn-fp:flat-L3vpn-plan"

    @staticmethod
    def get_internal_plan_path():
        if is_lsa:
            return "/cisco-tsdn-core-fp-common:rfs-flat-L3vpn-plan"
        else:
            return (
                "/cisco-flat-L3vpn-fp-internal:flat-L3vpn-internal"
                "/cisco-flat-L3vpn-fp-internal:flat-L3vpn-plan"
            )

    @staticmethod
    def is_lsa_setup():
        with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as th:
            global is_lsa
            try:
                root = ncs.maagic.get_root(th)
                # If this path exists, it is not LSA
                non_lsa_path = root.cisco_flat_L3vpn_fp_internal__flat_L3vpn
                if non_lsa_path:
                    is_lsa = False
            except AttributeError:
                is_lsa = True
