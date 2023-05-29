# -*- mode: python; python-indent: 4 -*-
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from .IosXR import IosXR
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common import utils as TsdnUtils
import ncs

local_site_servicepoint = "flat-L2vpn-internal-local-site"
remote_site_servicepoint = "flat-L2vpn-internal-remote-site"
site_servicepoint = "flat-L2vpn-internal-site"
local_site_validation_callpoint = "flat-L2vpn-internal-local-site-validation"
remote_site_validation_callpoint = "flat-L2vpn-internal-remote-site-validation"
site_validation_callpoint = "flat-L2vpn-internal-site-validation"
l2vpn_rr_parent_route_policy_servicepoint = "l2vpn-rr-parent-route-policy"


def get_device_impl_default_class(self, root, service, device, dynamic_map_list):
    device_ned_id = TsdnUtils.get_device_ned_id(self, root, device)

    self.log.info(device_ned_id)
    iosxr_default_ned_id = root.\
        cisco_flat_L2vpn_fp_internal_common__cfp_configurations.iosxr_default_ned_id
    if iosxr_default_ned_id == device_ned_id:
        router = IosXR(self.log, root, service)
    elif device_ned_id is not None:
        # Dynamic Loading
        router = TsdnUtils.get_device_impl_class(self, root, service, device, device_ned_id,
                                                 dynamic_map_list,
                                                 StatusCodes.DYNAMIC_CLASS_NOT_FOUND,
                                                 UserErrorException)
        if router is None:
            raise UserErrorException(
                self.log, StatusCodes.NED_NOT_SUPPORTED
            ).set_context(
                f"Router NED not supported: {device_ned_id}",
                "Missing dynamic device mapping",
            ).add_state(
                "Device", device
            ).add_state(
                "Service", service.name
            ).add_state(
                "Device NED ID", device_ned_id
            ).finish()
    else:
        raise UserErrorException(self.log, StatusCodes.NED_NOT_SUPPORTED).set_context(
            f"Router NED not supported: {device_ned_id}",
            "Missing dynamic device mapping",
        ).add_state("Device", device).add_state("Service", service.name).add_state(
            "Device NED ID", device_ned_id
        ).finish()

    return router


def create_parent_route_policy_service(self, name, root, device, device_impl_class):
    self.log.info(f"Entering create_parent_route_policy_service for {device}")
    # Assumes CREATE or MOD op
    # Check if L2VPN NB attach-point -> parent-rr-route-policy points to valid RP on device
    if not device_impl_class.validate_parent_policy_exists(root, device, name):
        raise UserErrorException(
            self.log, StatusCodes.RP_NO_PARENT_POLICY).set_context(
            "Incomplete Reference", "L2VPN attach-point -> parent-rr-route-policy "
            "does not point to valid RP",
        ).add_state("parent-rr-route-policy", name).finish()

    # If parent policy is present on device, check if there exists parent route policy service
    parent_rp_service_list = root.cisco_flat_L2vpn_fp_internal_common__l2vpn_rr_parent_route_policy

    if (name, device) not in parent_rp_service_list:
        # If not, create an entry, read original RR value from device, and persist
        parent_rp_service = parent_rp_service_list.create(name, device)
        # Get original RR RP value from device
        parent_rp_service.original_rr_route_policy = device_impl_class.\
            get_original_policy(root, device, name)
    else:
        # If does, check if local-rp list has any entries.
        parent_rp_service = parent_rp_service_list[(name, device)]
        if len(parent_rp_service.local_route_policy) < 1:
            # If no entries, then we need to update original rr from device
            parent_rp_service.original_rr_route_policy = device_impl_class.\
                get_original_policy(root, device, name)
    self.log.info(f"Exiting create_parent_route_policy_service for {device}")


def config_parent_route_policy_service(self, site, prefix=''):
    self.log.info(f"Entering update_parent_route_policy_service_local_rp for {site.pe}")
    # Apply template to copy over local route policy
    l2vpn_vars = ncs.template.Variables()
    l2vpn_vars.add("PE", site.pe)
    l2vpn_vars.add("PREFIX", prefix)
    l2vpn_template = ncs.template.Template(site.sr_te.odn)
    l2vpn_template.apply("cisco-flat-L2vpn-fp-rr-parent-route-policy", l2vpn_vars)
    self.log.info(f"Exiting update_parent_route_policy_service_local_rp for {site.pe}")


def diff_iterate(th) -> DiffIterateWrapper:
    def diter(self, keypath, op, oldv, newv):
        # Extract key_path0
        is_key_path0_tuple = isinstance(keypath[0], tuple)
        key_path0 = keypath[0] if is_key_path0_tuple else str(keypath[0])
        # Check if redeploy
        if is_key_path0_tuple is False and key_path0 == "re-deploy-counter":
            self.is_redeploy = True
            return ncs.ITER_STOP
        else:
            return ncs.ITER_RECURSE

    diff_iter = DiffIterateWrapper(diter, is_redeploy=False)
    th.diff_iterate(diff_iter, 0)

    return diff_iter
