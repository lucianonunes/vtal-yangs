import ncs
import traceback
import time
from cisco_tsdn_core_fp_common.utils import is_netsim_device


class UMXR:
    def get_interface_loopback(self, root, service):
        self.log.info(f"Getting Loopback for service {service} "
                      f"from IOSXR device {service.head_end}")

        device_address = service.name.split("-")[-2]
        device = root.devices.device[service.head_end]

        for (act_loopback) in (
            device.
            config.Cisco_IOS_XR_um_interface_cfg__interfaces.interface
        ):
            if "Loopback" in act_loopback.interface_name:
                loopback = act_loopback.interface_name.replace("Loopback", "")
                address = act_loopback.\
                    ipv4.Cisco_IOS_XR_um_if_ip_address_cfg__addresses.address.address
                if address == device_address:
                    return loopback
        return None

    def conf_rsvp_te_tunnel_p2p(self, service, loopback):
        self.log.info(f"Configuring RSVP TE Tunnel on IOSXR NC device {service.head_end}")

        #  Applying config
        vars = ncs.template.Variables()
        vars.add("LOOPBACK", loopback)
        template = ncs.template.Template(service)
        template.apply("cisco-rsvp-te-fp-um-template", vars)

    def ietf_te_self_test(self, uinfo, root, rsvp_te_service):
        device = rsvp_te_service.head_end
        self.log.info(f"Running IETF-TE self test on IOSXR NETCONF device {device}")
        #  If netsim return success
        if is_netsim_device(self, root, device):
            return ("success", None)
        else:
            # Execute self-test
            return self.run_self_test(root, rsvp_te_service.id, device, rsvp_te_service)

    def run_self_test(self, root, identifier, device, service):
        max_retries = 2
        for retry in range(max_retries):
            try:
                admin_output = (root.devices.device[device].live_status.
                                Cisco_IOS_XR_mpls_te_oper__mpls_te.p2p_p2mp_tunnel
                                .tunnel_heads.tunnel_head["tunnel-te" + str(identifier)]
                                .admin_state)
                self.log.info(f"admin state: {admin_output}")
                oper_output = (root.devices.device[device].live_status.
                               Cisco_IOS_XR_mpls_te_oper__mpls_te.p2p_p2mp_tunnel.tunnel_heads.
                               tunnel_head["tunnel-te" + str(identifier)].operational_state)
                self.log.info(f"oper state : {oper_output}")

                if "admin-up" == admin_output and ("oper-up" == oper_output or
                                                   "operational-up" == oper_output):
                    return ("success", None)
                elif "admin-up" != admin_output and ("oper-up" != oper_output or
                                                     "operational-up" != oper_output):
                    return ("failed", "Both Admin & Operational states are down")
                elif "admin-up" != admin_output:
                    return ("failed", "Admin state is down")
                elif "oper-up" != oper_output or "operational-up" != oper_output:
                    return ("failed", "Operational state is down")
                else:
                    return ("failed", "No rsvp-te status found on device")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (max_retries - 1):
                    self.log.info("Connection reset on device, "
                                  f"reached max-retries for: {service.name}")
                    return (
                        "failed", "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request ietf-te te tunnels tunnel <TUNNEL>"
                        + " action self-test'")
                elif "Connection reset in new state" in str(e):
                    self.log.info("Connection reset on device, "
                                  f"will retry self test for: {service.name}")
                else:
                    traceback.print_exc()
                    raise e
            time.sleep(30)
            self.log.info(f"Retrying self test for: {service.name}")

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
