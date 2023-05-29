# -*- mode: python; python-indent: 4 -*-
from .IosXR import IosXR
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common import utils as TsdnUtils

l3vpn_internal_servicepoint = "flat-L3vpn"
l3vpn_internal_validation_callpoint = "flat-L3vpn-internal-validation"


def get_device_impl_default_class(self, root, service, device, dynamic_map_list):
    device_ned_id = TsdnUtils.get_device_ned_id(self, root, device)

    self.log.info(device_ned_id)
    iosxr_default_ned_id = root.\
        cisco_flat_L3vpn_fp_internal__cfp_configurations.iosxr_default_ned_id
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


def can_update_on_device(self, root, service, endpoint, attribute):
    device = endpoint.access_pe
    router = get_device_impl_default_class(
        self, root, service, device,
        root.cisco_flat_L3vpn_fp_internal__cfp_configurations.dynamic_device_mapping,
    )
    # RD value updates
    if attribute == "route-distinguisher":
        global_rd_enabled = root.cisco_flat_L3vpn_fp_internal__cfp_configurations.global_rd_enabled
        bgp_as_no = ""
        if endpoint.as_no is not None:
            bgp_as_no = endpoint.as_no
        elif endpoint.as_no_from_device.exists():
            bgp_as_no = router.get_bgp_as_from_device(root, device)

        vrf_name = endpoint.vrf.vrf_definition
        new_rd = endpoint.vrf.route_distinguisher

        if bgp_as_no != "":
            # Is VRF address-family active on device?
            try:
                vrf_active = router.is_vrf_address_family_active(root, device,
                                                                 bgp_as_no, vrf_name)
            except AttributeError as e:
                raise UserErrorException(
                    self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                ).set_context(
                    "Dynamic Method Error",
                    "is_vrf_address_family_active method is missing from "
                    "multi-vendor python class",
                ).add_state(
                    "Endpoint", device
                ).finish()
            if vrf_active:
                try:
                    global_router_rd = router.get_vrf_rd(root, device, vrf_name)
                except AttributeError as e:
                    raise UserErrorException(
                        self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Method Error",
                        "get_vrf_rd method is missing from "
                        "multi-vendor python class",
                    ).add_state(
                        "Endpoint", device
                    ).finish()
                try:
                    local_router_rd = router.get_bgp_vrf_rd(root, device, bgp_as_no, vrf_name)

                except AttributeError as e:
                    raise UserErrorException(
                        self.log, StatusCodes.DYNAMIC_METHOD_NOT_FOUND, str(e)
                    ).set_context(
                        "Dynamic Method Error",
                        "get_bgp_vrf_rd method is missing from "
                        "multi-vendor python class",
                    ).add_state(
                        "Endpoint", device
                    ).finish()

                # if vrf is active, and local rd is set and we have enabled global rd,
                # updates to global RD are allowed.
                if global_rd_enabled and local_router_rd is not None:
                    return True
                # if vrf is active, and local rd is not set and we have enabled global rd,
                # updates to global RD are NOT allowed.
                elif global_rd_enabled and local_router_rd is None:
                    if new_rd != global_router_rd:
                        return False
                # if vrf is active, local RD updates are NOT allowed.
                else:
                    if new_rd != local_router_rd:
                        return False

    # Worst case, return True and let device throw exception
    return True


def get_bgp_as_from_device(self, root, service, endpoint):
    device = endpoint.access_pe
    router = get_device_impl_default_class(
        self, root, service, device,
        root.cisco_flat_L3vpn_fp_internal__cfp_configurations.dynamic_device_mapping,
    )
    return router.get_bgp_as_from_device(root, device)


# Check if external service resides on same NSO instance as internal
def external_service_model_exists(root):
    try:
        # If this path exists, it is not LSA
        return root.cisco_flat_L3vpn_fp__flat_L3vpn is not None
    except Exception:
        return False
