import ncs
from . import utils as Utils
from core_fp_common.common_utils import get_local_user
import traceback
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from .flat_L2vpn import FlatL2vpn


def update_flat_L2VPN_plan(log, service_name, pe_name, rfs_node=None):
    with ncs.maapi.single_read_trans(get_local_user(), "system", db=ncs.OPERATIONAL) as th:
        root = ncs.maagic.get_root(th)

        local_rfs_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L2vpn_local_site_plan
        remote_rfs_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L2vpn_remote_site_plan
        if (service_name, pe_name) in local_rfs_plan_path:
            is_local_site = True
        elif (service_name, pe_name) in remote_rfs_plan_path:
            is_local_site = False
        else:
            is_local_site = None

    update_flat_L2VPN_site_plan(log, service_name, pe_name, is_local_site, rfs_node)


def update_flat_L2VPN_site_plan(log, service_name, pe_name, local_site, rfs_node):
    log.info(f"Updating Flat L2VPN plan for service={service_name} pe={pe_name} site={local_site}")
    try:
        with ncs.maapi.single_write_trans(get_local_user(), "system", db=ncs.OPERATIONAL) as wr_th:
            wr_root = ncs.maagic.get_root(wr_th)
            # Read live-status data & write to rfs-flat-l2vpn plan copy
            if rfs_node is None:
                rfs_live_status = LsaUtils.get_remote_nso_live_status(wr_root, pe_name)
            else:
                rfs_live_status = wr_root.ncs__devices.device[rfs_node].live_status

            if local_site:
                local_rfs_plan_path = wr_root.\
                    cisco_tsdn_core_fp_common__rfs_flat_L2vpn_local_site_plan
                rfs_plan_path = rfs_live_status.\
                    cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_internal_local_site.\
                    cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_plan
            elif local_site is False:
                local_rfs_plan_path = wr_root.\
                    cisco_tsdn_core_fp_common__rfs_flat_L2vpn_remote_site_plan
                rfs_plan_path = rfs_live_status.\
                    cisco_flat_L2vpn_fp_internal_remote_site__flat_L2vpn_internal_remote_site.\
                    cisco_flat_L2vpn_fp_internal_remote_site__flat_L2vpn_plan
            else:
                local_rfs_plan_path = wr_root.\
                    cisco_tsdn_core_fp_common__rfs_flat_L2vpn_site_plan
                rfs_plan_path = rfs_live_status.\
                    cisco_flat_L2vpn_fp_internal_site__flat_L2vpn_internal_site.\
                    cisco_flat_L2vpn_fp_internal_site__flat_L2vpn_plan

            # Remove old rfs plan if exists
            if (service_name, pe_name) in local_rfs_plan_path:
                del local_rfs_plan_path[(service_name, pe_name)]

            # Copy over new plan if exists under live-status
            if (service_name, pe_name) in rfs_plan_path:
                # Copy latest plan over to local copy
                rfs_plan = rfs_plan_path[(service_name, pe_name)]

                local_plan_instance = local_rfs_plan_path.create(service_name, pe_name)
                local_rfs_plan = local_plan_instance.plan

                internal_zombie_service = []
                if local_site:
                    internal_zombie_service.\
                        append("/cisco-flat-L2vpn-fp-internal-local-site:"
                               f"/flat-L2vpn-internal-local-site-service[name='{service_name}']"
                               f"[pe='{pe_name}']")

                    internal_zombie_service.append("/flat-L2vpn-internal-local-site-service"
                                                   f"[name='{service_name}'][pe='{pe_name}']")
                elif local_site is False:
                    internal_zombie_service.append("/cisco-flat-L2vpn-fp-internal-remote-site:"
                                                   "/flat-L2vpn-internal-remote-site-service"
                                                   f"[name='{service_name}'][pe='{pe_name}']")

                    internal_zombie_service.append("/flat-L2vpn-internal-remote-site-service"
                                                   f"[name='{service_name}'][pe='{pe_name}']")
                else:
                    local_plan_instance.pe = rfs_plan.pe
                    internal_zombie_service.append(
                        "/cisco-flat-L2vpn-fp-internal-site:"
                        "/flat-L2vpn-internal-site-service"
                        f"[name='{service_name}'][site-name='{pe_name}']"
                    )

                    internal_zombie_service.append(
                        "/flat-L2vpn-internal-site-service"
                        f"[name='{service_name}'][site-name='{pe_name}']"
                    )

                Utils.copy_rfs_plan_to_local(log, local_rfs_plan, rfs_plan.plan,
                                             internal_zombie_service, rfs_live_status)

            wr_th.apply()
    except Exception as e:
        traceback.print_exc()
        raise e


def handle_flat_L2VPN_plan(changed_path, rfs_node, log):
    try:
        if "cisco-flat-L2vpn-fp-internal-local-site" in changed_path:
            local_site = True
        elif "cisco-flat-L2vpn-fp-internal-remote-site" in changed_path:
            local_site = False
        else:
            local_site = None

        service_key = FlatL2vpn.get_service_key_from_path(changed_path)
        service_name = service_key[0]
        pe_name = service_key[1]

        update_flat_L2VPN_site_plan(log, service_name, pe_name, local_site, rfs_node)
        log.info(f"Successfully updated local RFS plan copy for: {changed_path}")

    except Exception as e:
        log.error(f"Exception handle_flat_L2VPN_plan: {e}")
        traceback.print_exc()
        raise e
