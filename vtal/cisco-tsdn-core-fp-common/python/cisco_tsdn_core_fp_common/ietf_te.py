from cisco_tsdn_core_fp_common import utils as utils
from cisco_tsdn_core_fp_common.cnc_service import CncService
import ncs
import re


is_lsa = False
internal_name_pattern = re.compile(
    "tunnel-te-plan{(.*)-(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]).){3}"
    "([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])-internal}$"
)


class IetfTe(CncService):
    def __init__(self, path, log):
        super().__init__(path)
        self.log = log

    def redeploy(self):
        # Skip re-deploy if internal plan error-info/message matched NB plan error-info/message
        # to avoid re-deploy loops on failure
        if utils.match_plan_err_msg(self.plan_path, self.service_name, self.internal_plan_path,
                                    IetfTe.get_internal_service_key_from_path(self.path)) and \
                utils.is_failed_plan_converged(self.plan_path, self.service_name, self.log):
            return

        utils.redeploy_if_needed(self)

    @staticmethod
    def get_service_name_from_path(path):
        return IetfTe.get_service_key_from_path(path)

    @staticmethod
    def get_service_key_from_path(path):
        return internal_name_pattern.search(path).group(1)

    @staticmethod
    def get_internal_service_key_from_path(path):
        return re.search("tunnel-te-plan{(.*?)}", path).group(1)

    @staticmethod
    def get_service_kp(ietf_service_name):
        return f"/te:te/tunnels/tunnel{{{ietf_service_name}}}"

    @staticmethod
    def get_service_xpath(ietf_service_name):
        return f"/te/tunnels/tunnel[name='{ietf_service_name}']"

    @staticmethod
    def get_plan_kp(ietf_service_name):
        return f"/te:te/te:tunnels/te:tunnel-plan{{{ietf_service_name}}}"

    @staticmethod
    def get_plan_path():
        return "/te:te/te:tunnels/te:tunnel-plan"

    @staticmethod
    def get_internal_plan_path():
        if is_lsa:
            return "/cisco-tsdn-core-fp-common:rfs-rsvp-plan"
        else:
            return "/cisco-rsvp-te-fp:rsvp-te/cisco-rsvp-te-fp:tunnel-te-plan"

    @staticmethod
    def is_lsa_setup():
        with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as th:
            global is_lsa
            try:
                root = ncs.maagic.get_root(th)
                # If this path exists, it is not LSA
                non_lsa_path = root.cisco_rsvp_te_fp__rsvp_te
                if non_lsa_path:
                    is_lsa = False
            except AttributeError:
                is_lsa = True
