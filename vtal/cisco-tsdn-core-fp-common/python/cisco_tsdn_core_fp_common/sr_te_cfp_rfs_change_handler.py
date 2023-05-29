# -*- mode: python; python-indent: 4 -*-
import ncs
from . import utils as Utils
from core_fp_common.common_utils import get_local_user
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
import traceback

from .sr_te_odn import SrTeOdn
from .sr_te_policy import SrTePolicy


def handle_odn_template_plan(changed_path, rfs_node, log):
    try:
        log.info(f"Update local RFS plan copy for: {changed_path}")
        service_key = SrTeOdn.get_service_key_from_path(changed_path)
        update_odn_template_plan(log, service_key, rfs_node)
        log.info(f"Successfully updated local RFS plan copy for: {changed_path}")
    except Exception as e:
        traceback.print_exc()
        raise e

    log.info(f"Local RFS plan copy updated for: {changed_path}")


def update_odn_template_plan(log, service_key, rfs_node=None):
    log.info(f"Updating ODN plan for service={service_key}")
    try:
        # Writing with maapi here instead of Cdb APIs because a partial update could be read
        # incorrectly by the service if an external redeploy happens from outside
        # Partial plan could cause issues if written with Cdbsession APIs.
        with ncs.maapi.single_write_trans(get_local_user(), "system", db=ncs.OPERATIONAL) as wr_th:
            wr_root = ncs.maagic.get_root(wr_th)
            # Read live-status data & write to rfs-odn-template-plan
            if rfs_node is None:
                rfs_live_status = LsaUtils.get_remote_nso_live_status(wr_root, service_key[1])
            else:
                rfs_live_status = wr_root.ncs__devices.device[rfs_node].live_status

            local_rfs_plan_path = wr_root.cisco_tsdn_core_fp_common__rfs_odn_template_plan

            rfs_plan_path = (rfs_live_status.cisco_sr_te_cfp_internal__sr_te
                             .cisco_sr_te_cfp_sr_odn_internal__odn.odn_template_plan)
            # Remove old rfs plan if exists
            if service_key in local_rfs_plan_path:
                del local_rfs_plan_path[service_key]

            # Copy over new plan if exists under live-status
            if service_key in rfs_plan_path:
                # Copy latest plan over to local copy
                rfs_plan = rfs_plan_path[service_key].plan
                local_rfs_plan = None
                if service_key not in local_rfs_plan_path:
                    local_plan_instance = local_rfs_plan_path.create(service_key)
                    local_rfs_plan = local_plan_instance.plan
                else:
                    local_rfs_plan = local_rfs_plan_path[service_key].plan

                internal_zombie_service = []
                internal_zombie_service.append("/cisco-sr-te-cfp-internal:sr-te/"
                                               "cisco-sr-te-cfp-sr-odn-internal:"
                                               f"odn/odn-template[name='{service_key[0]}']"
                                               f"[head-end='{service_key[1]}']")
                internal_zombie_service.append("/sr-te/"
                                               "cisco-sr-te-cfp-sr-odn-internal:"
                                               f"odn/odn-template[name='{service_key[0]}']"
                                               f"[head-end='{service_key[1]}']")

                Utils.copy_rfs_plan_to_local(log, local_rfs_plan, rfs_plan,
                                             internal_zombie_service, rfs_live_status)

            wr_th.apply()
    except Exception as e:
        traceback.print_exc()
        raise e


def handle_policy_template_plan(changed_path, rfs_node, log):
    try:
        log.info(f"Update local RFS plan copy for: {changed_path}")

        policy_key = SrTePolicy.get_service_key_from_path(changed_path)
        policy_service_name = policy_key[0]
        head_end = policy_key[1]

        update_policy_template_plan(log, policy_service_name, head_end, rfs_node)
        log.info(f"Successfully updated local RFS plan copy for: {changed_path}")
    except Exception as e:
        traceback.print_exc()
        raise e


def update_policy_template_plan(log, policy_service_name, head_end, rfs_node=None):
    log.info(f"Updating Policy plan for service={policy_service_name} head_end={head_end}")
    try:
        # Writing with maapi here instead of Cdb APIs because a partial update could be read
        # incorrectly by the service if an external redeploy happens from outside
        # Partial plan could cause issues if written with Cdbsession APIs.
        with ncs.maapi.single_write_trans(get_local_user(), "system", db=ncs.OPERATIONAL) as wr_th:
            wr_root = ncs.maagic.get_root(wr_th)
            # Read live-status data & write to rfs-policy-plan
            if rfs_node is None:
                rfs_live_status = LsaUtils.get_remote_nso_live_status(wr_root, head_end)
            else:
                rfs_live_status = wr_root.ncs__devices.device[rfs_node].live_status

            local_rfs_plan_path = wr_root.cisco_tsdn_core_fp_common__rfs_policy_plan

            rfs_plan_path = (rfs_live_status.cisco_sr_te_cfp_internal__sr_te
                             .cisco_sr_te_cfp_sr_policies_internal__policies.policy_plan)
            # Remove old rfs plan if exists
            if (policy_service_name, head_end) in local_rfs_plan_path:
                del local_rfs_plan_path[(policy_service_name, head_end)]

            # Copy over new plan if exists under live-status
            if (policy_service_name, head_end) in rfs_plan_path:
                # Copy latest plan over to local copy
                rfs_plan = rfs_plan_path[(policy_service_name, head_end)].plan
                local_rfs_plan = None
                if (policy_service_name, head_end) not in local_rfs_plan_path:
                    local_plan_instance = local_rfs_plan_path.create(policy_service_name, head_end)
                    local_rfs_plan = local_plan_instance.plan
                else:
                    local_rfs_plan = local_rfs_plan_path[(policy_service_name, head_end)].plan

                internal_zombie_service = []
                internal_zombie_service.append("/cisco-sr-te-cfp-internal:sr-te/"
                                               "cisco-sr-te-cfp-sr-policies-internal:"
                                               f"policies/policy[name='{policy_service_name}']"
                                               f"[head-end='{head_end}']")
                internal_zombie_service.append("/sr-te/cisco-sr-te-cfp-sr-policies-internal:"
                                               f"policies/policy[name='{policy_service_name}']"
                                               f"[head-end='{head_end}']")

                Utils.copy_rfs_plan_to_local(log, local_rfs_plan, rfs_plan,
                                             internal_zombie_service, rfs_live_status)

            wr_th.apply()
    except Exception as e:
        traceback.print_exc()
        raise e
