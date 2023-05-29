from cisco_tsdn_core_fp_common import utils as utils
from cisco_tsdn_core_fp_common.cnc_service import CncService
import ncs
import re


is_lsa = False


class FlatL2vpn(CncService):
    def __init__(self, path, log):
        super().__init__(path)
        self.log = log

        if "cisco-flat-L2vpn-fp-internal-local-site" in path:
            self.internal_plan_path = self.get_internal_local_plan_path()
            self.local_site = True
        elif "cisco-flat-L2vpn-fp-internal-remote-site" in path:
            self.internal_plan_path = self.get_internal_remote_plan_path()
            self.local_site = False
        elif "cisco-flat-L2vpn-fp-internal-site" in path:
            self.internal_plan_path = self.get_internal_site_plan_path()
            self.local_site = None

    def redeploy(self):
        # Skip re-deploy if internal plan error-info/message matched NB plan error-info/message
        # to avoid re-deploy loops on failure
        if utils.match_plan_err_msg(self.plan_path, self.service_name, self.internal_plan_path,
                                    self.service_key) and \
                utils.is_failed_plan_converged(self.plan_path, self.service_name, self.log):
            return

        utils.redeploy_if_needed(self)

    @staticmethod
    def get_service_name_from_path(path):
        return re.search("flat-L2vpn-plan{(.*?) (.*?)}", path).group(1)

    @staticmethod
    def get_service_key_from_path(path):
        re_search = re.search("flat-L2vpn-plan{(.*?) (.*?)}", path)
        return re_search.group((1)), re_search.group(2)

    @staticmethod
    def get_service_kp(l2vpn_service_name):
        return f"/cisco-flat-L2vpn-fp:flat-L2vpn{{{l2vpn_service_name}}}"

    @staticmethod
    def get_service_xpath(l2vpn_service_name):
        return f"/flat-L2vpn[name='{l2vpn_service_name}']"

    @staticmethod
    def get_plan_kp(odn_service_name):
        return f"/flat-L2vpn-plan{{{odn_service_name}}}"

    @staticmethod
    def get_plan_path():
        return "/cisco-flat-L2vpn-fp:flat-L2vpn-plan"

    @staticmethod
    def get_internal_plan_path():
        return FlatL2vpn.get_internal_local_plan_path()

    @staticmethod
    def get_internal_local_plan_path():
        if is_lsa:
            return "/cisco-tsdn-core-fp-common:rfs-flat-L2vpn-local-site-plan"
        else:
            return (
                "/cisco-flat-L2vpn-fp-internal-local-site:flat-L2vpn-internal-local-site"
                "/cisco-flat-L2vpn-fp-internal-local-site:flat-L2vpn-plan"
            )

    @staticmethod
    def get_internal_remote_plan_path():
        if is_lsa:
            return "/cisco-tsdn-core-fp-common:rfs-flat-L2vpn-remote-site-plan"
        else:
            return (
                "/cisco-flat-L2vpn-fp-internal-remote-site:flat-L2vpn-internal-remote-site"
                "/cisco-flat-L2vpn-fp-internal-remote-site:flat-L2vpn-plan"
            )

    @staticmethod
    def get_internal_site_plan_path():
        if is_lsa:
            return "/cisco-tsdn-core-fp-common:rfs-flat-L2vpn-site-plan"
        else:
            return (
                "/cisco-flat-L2vpn-fp-internal-site:flat-L2vpn-internal-site"
                "/cisco-flat-L2vpn-fp-internal-site:flat-L2vpn-plan"
            )

    @staticmethod
    def is_lsa_setup():
        with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as th:
            global is_lsa
            try:
                root = ncs.maagic.get_root(th)
                # If this internal service path exists, it is not LSA
                non_lsa_path = root.\
                    cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_internal_local_site_service
                if non_lsa_path:
                    is_lsa = False
            except AttributeError:
                is_lsa = True
