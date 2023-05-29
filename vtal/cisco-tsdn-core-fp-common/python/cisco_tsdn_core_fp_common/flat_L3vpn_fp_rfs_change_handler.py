from .flat_L3vpn import FlatL3vpn
from .utils import copy_rfs_plan_to_local
from ncs import maapi, maagic, OPERATIONAL
from traceback import print_exc
from core_fp_common.common_utils import get_local_user
from lsa_utils_pkg.dmap.dm_utils import get_remote_nso_live_status


def handle_L3vpn_plan(changed_path, rfs_node, log):
    # Plan Update
    try:
        log.info(f"Update local RFS plan copy for: {changed_path}")
        # Grab service name and endpoint
        # Example: (Extract service_name='L3' and endpoint='cli-0'), changed_path =
        # '/cisco-flat-L3vpn-fp-internal:flat-L3vpn-internal/flat-L3vpn-plan{L3 cli-0}'
        service_key = FlatL3vpn.get_service_key_from_path(changed_path)
        service_name = service_key[0]
        endpoint = service_key[1]
        # Update plan
        update_L3vpn_plan(log, service_name, endpoint, rfs_node)
        log.info(f"Successfully updated local RFS plan copy for: {changed_path}")
    except Exception as e:
        print_exc()
        raise e


def update_L3vpn_plan(log, service_name, endpoint, rfs_node=None):
    log.info(f"Updating Flat L3VPN plan for service={service_name} endpoint={endpoint}")
    try:
        # Writing with maapi here instead of Cdb APIs because a partial update could be read
        # incorrectly by the service if an external redeploy happens from outside
        # Partial plan could cause issues if written with Cdbsession APIs.
        with maapi.single_write_trans(get_local_user(), "system", db=OPERATIONAL) as th:
            root = maagic.get_root(th)

            # Read live-status data & write to rfs-flat-L3vpn-plan
            if rfs_node is None:
                rfs_live_status = get_remote_nso_live_status(root, endpoint)
                plan_path = root.cisco_flat_L3vpn_fp__flat_L3vpn_plan[service_name].plan
                if ("cisco-flat-L3vpn-fp-nano-plan-services:endpoint", endpoint) \
                        in plan_path.component:
                    # Get device (access_pe)
                    device = (
                        plan_path
                        .component["cisco-flat-L3vpn-fp-nano-plan-services:endpoint", endpoint]
                        .private.property_list.property["ACCESS_PE"]
                        .value
                    )
                    rfs_live_status = get_remote_nso_live_status(root, device)
            else:
                rfs_live_status = root.ncs__devices.device[rfs_node].live_status

            local_rfs_plan_path = root.cisco_tsdn_core_fp_common__rfs_flat_L3vpn_plan
            rfs_plan_path = rfs_live_status.\
                cisco_flat_L3vpn_fp_internal__flat_L3vpn_internal.flat_L3vpn_plan
            # Remove old rfs plan if exists
            service_key = (service_name, endpoint)
            if service_key in local_rfs_plan_path:
                del local_rfs_plan_path[service_key]
            # Copy over new plan if exists under live-status
            if service_key in rfs_plan_path:
                # Copy latest plan over to local copy
                rfs_plan = rfs_plan_path[service_key].plan
                if service_key not in local_rfs_plan_path:
                    local_plan_instance = local_rfs_plan_path.create(*service_key)
                    local_rfs_plan = local_plan_instance.plan
                else:
                    local_rfs_plan = local_rfs_plan_path[service_key].plan
                # Define zombie paths
                internal_zombie_services = []
                # non-LSA
                internal_zombie_services.append("/cisco-flat-L3vpn-fp-internal:flat-L3vpn"
                                                "[name='{0}'][endpoint-name='{1}']".
                                                format(*service_key))
                # LSA
                internal_zombie_services.append("/flat-L3vpn[name='{0}']"
                                                "[endpoint-name='{1}']".
                                                format(*service_key))
                copy_rfs_plan_to_local(log, local_rfs_plan, rfs_plan,
                                       internal_zombie_services, rfs_live_status)

            th.apply()
    except Exception as e:
        print_exc()
        raise e
