import ncs

is_lsa = False
ietf_l2vpn_servicepoint = "ietf-l2vpn-ntw-servicepoint"
ietf_l2vpn_validation_callpoint = "ietf-l2vpn-nm-validation"
l2vpn_ntw_augmentations_y1731_servicepoint = "l2vpn-ntw-augmentations-y1731-servicepoint"


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
        except Exception:
            is_lsa = True


def get_evpn_pbb_comp_name(vpn_node_id, vpn_network_access_id):
    return f"{vpn_node_id}_{vpn_network_access_id}"
