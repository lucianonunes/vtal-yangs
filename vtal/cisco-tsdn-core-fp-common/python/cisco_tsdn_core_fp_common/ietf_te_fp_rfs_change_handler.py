# -*- mode: python; python-indent: 4 -*-
import ncs
from . import utils as Utils
from core_fp_common.common_utils import get_local_user
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
import re
import traceback


def handle_ietf_plan(changed_path, rfs_node, log):
    try:
        log.info(f"Update local RFS plan copy for: {changed_path}")

        internal_service_name = re.search("tunnel-te-plan{(.*?)}", changed_path).group(1)

        update_ietf_plan(log, internal_service_name, rfs_node)
        log.info(f"Successfully updated local RFS plan copy for: {changed_path}")
    except Exception as e:
        traceback.print_exc()
        raise e


def update_ietf_plan(log, internal_service_name, rfs_node=None):
    log.info(f"Updating IETF-TE plan for service={internal_service_name}")
    try:
        # Writing with maapi here instead of Cdb APIs because a partial update could be read
        # incorrectly by the service if an external redeploy happens from outside
        # Partial plan could cause issues if written with Cdbsession APIs.
        with ncs.maapi.single_write_trans(get_local_user(), "system", db=ncs.OPERATIONAL) as wr_th:
            wr_root = ncs.maagic.get_root(wr_th)

            # Read live-status data & write to rfs-ietf-plan
            if rfs_node is None:
                # Internal service name format: <ietf_service_name>-<head_end>-internal
                # Retrieve head-end name and IETF (NB) service name from the internal service name
                # based on the above format
                device = None
                head_end = internal_service_name.split("-")[-2]
                ietf_service_name = internal_service_name[0: internal_service_name.
                                                          index("-" + head_end)]
                plan_path = wr_root.te__te.tunnels.tunnel_plan[ietf_service_name].plan
                if ("ietf-te-fp-tunnel-nano-plan-services:source", head_end) in plan_path.component:
                    device = plan_path.\
                        component["ietf-te-fp-tunnel-nano-plan-services:source", head_end].private.\
                        property_list.property["DEVICE"].value
                else:
                    device = plan_path.\
                        component["ietf-te-fp-tunnel-nano-plan-services:destination", head_end].\
                        private.property_list.property["DEVICE"].value
                rfs_live_status = LsaUtils.get_remote_nso_live_status(wr_root, device)
            else:
                rfs_live_status = wr_root.ncs__devices.device[rfs_node].live_status

            local_rfs_plan_path = wr_root.cisco_tsdn_core_fp_common__rfs_rsvp_plan

            rfs_plan_path = rfs_live_status.cisco_rsvp_te_fp__rsvp_te.tunnel_te_plan

            # Remove old rfs plan if exists
            if internal_service_name in local_rfs_plan_path:
                del local_rfs_plan_path[internal_service_name]

            # Copy over new plan if exists under live-status
            if internal_service_name in rfs_plan_path:
                # Copy latest plan over to local copy
                rfs_plan = rfs_plan_path[internal_service_name].plan
                local_rfs_plan = None
                if internal_service_name not in local_rfs_plan_path:
                    local_plan_instance = local_rfs_plan_path.create(internal_service_name)
                    local_rfs_plan = local_plan_instance.plan
                else:
                    local_rfs_plan = local_rfs_plan_path[internal_service_name].plan

                internal_zombie_services = []
                # Non-LSA
                internal_zombie_services.append("/cisco-rsvp-te-fp:rsvp-te/"
                                                f"tunnel-te[name='{internal_service_name}']")

                # LSA
                internal_zombie_services.append("/rsvp-te/"
                                                f"tunnel-te[name='{internal_service_name}']")
                Utils.copy_rfs_plan_to_local(log, local_rfs_plan, rfs_plan,
                                             internal_zombie_services, rfs_live_status)

            wr_th.apply()
    except Exception as e:
        traceback.print_exc()
        raise e
