from cisco_tsdn_core_fp_common import utils as utils
from cisco_tsdn_core_fp_common.cnc_service import CncService
import ncs
import re


is_lsa = False


class SrTeOdn(CncService):
    def __init__(self, path, log):
        super().__init__(path)
        self.log = log

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
        return re.search("odn-template-plan{(.*) ", path).group(1)

    @staticmethod
    def get_service_key_from_path(path):
        re_search = re.search("odn-template-plan{(.*?) (.*?)}", path)
        return re_search.group(1), re_search.group(2)

    @staticmethod
    def get_service_kp(odn_service_name):
        return ("/cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-odn:odn/"
                f"cisco-sr-te-cfp-sr-odn:odn-template{{{odn_service_name}}}")

    @staticmethod
    def get_service_xpath(odn_service_name):
        if is_lsa:
            return (f"/sr-te/cisco-sr-te-cfp-sr-odn:odn/odn-template[name='{odn_service_name}']")
        else:
            return ("/cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-odn:"
                    f"odn/odn-template[name='{odn_service_name}']")

    @staticmethod
    def get_plan_kp(odn_service_name):
        return ("/cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-odn:odn"
                f"/cisco-sr-te-cfp-sr-odn:odn-template-plan{{{odn_service_name}}}")

    @staticmethod
    def get_plan_path():
        return "/cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-odn:odn/odn-template-plan"

    @staticmethod
    def get_internal_plan_path():
        if is_lsa:
            return "/cisco-tsdn-core-fp-common:rfs-odn-template-plan"
        else:
            return (
                "/cisco-sr-te-cfp-internal:sr-te/cisco-sr-te-cfp-sr-odn-internal:odn"
                "/cisco-sr-te-cfp-sr-odn-internal:odn-template-plan"
            )

    @staticmethod
    def is_lsa_setup():
        with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as th:
            global is_lsa
            try:
                root = ncs.maagic.get_root(th)
                # If this path exists, it is not LSA
                non_lsa_path = root.cisco_sr_te_cfp_internal__sr_te
                if non_lsa_path:
                    is_lsa = False
            except AttributeError:
                is_lsa = True
