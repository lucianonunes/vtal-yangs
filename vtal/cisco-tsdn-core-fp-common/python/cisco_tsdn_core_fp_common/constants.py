failed_device = "/cisco-tsdn-core-fp-common:commit-queue-recovery-data/failed-device"
impacted_service = "/impacted-service-path"
failed_cq_id = "/failed-commit-queue-id"
current_poller = (
    "/cisco-tsdn-core-fp-common:commit-queue-recovery-data/current-device-poller"
)
poller_recovery_result = "/poller-recovery-result"

# TODO: how to differentiate between internal and external services
# for the case where CQ is enabled between CFS and RFS.
odn_service_alarm_id = "cisco-sr-te-cfp-sr-odn-internal:odn"
policy_service_alarm_id = "cisco-sr-te-cfp-sr-policies-internal:policies"
l3vpn_service_alarm_id = "flat-L3vpn"
l2vpn_remote_service_alarm_id = "flat-L2vpn-internal-remote-site-service"
l2vpn_local_service_alarm_id = "flat-L2vpn-internal-local-site-service"
tunnel_service_alarm_id = "tunnel-te"
pce_service_alarm_id = "sr-pce"
setting_service_alarm_id = "sr-setting"
transient_error_message = [
    "connection refused", "timed out", "host is unreachable",
    "no route to host", "host unreachable", "transport timeout",
    "host key mismatch", "read timeout"
]

cq_alarm_services = [
    odn_service_alarm_id,
    policy_service_alarm_id,
    l3vpn_service_alarm_id,
    l2vpn_remote_service_alarm_id,
    l2vpn_local_service_alarm_id,
    tunnel_service_alarm_id,
    pce_service_alarm_id,
    setting_service_alarm_id
]

L3NM_PLAN_PATH = "/l3vpn-ntw/vpn-services/vpn-service-plan"
L3NM_SERVICE_PATH = "/l3vpn-ntw/vpn-services/vpn-service"

CS_SR_TE_PLAN_PATH = "/cs-sr-te-plan"
CS_SR_TE_SERVICE_PATH = "/cs-sr-te-policy"

ZOMBIE_PATH = "/ncs:zombies/ncs:service"

LOCAL_SITE = "local-site"
REMOTE_SITE = "remote-site"
SITE = "site"

L2VPN_LOCAL_SITE_COMP_TYPE = "cisco-flat-L2vpn-fp-nano-plan-services:local-site"
L2VPN_REMOTE_SITE_COMP_TYPE = "cisco-flat-L2vpn-fp-nano-plan-services:remote-site"
L2VPN_SITE_COMP_TYPE = "cisco-flat-L2vpn-fp-nano-plan-services:site"

L2VPN_LSA_LOCAL_SITE_PLAN_PATH = "/cisco-tsdn-core-fp-common:rfs-flat-L2vpn-local-site-plan"
L2VPN_LSA_REMOTE_SITE_PLAN_PATH = "/cisco-tsdn-core-fp-common:rfs-flat-L2vpn-remote-site-plan"
L2VPN_LSA_SITE_PLAN_PATH = "/cisco-tsdn-core-fp-common:rfs-flat-L2vpn-site-plan"

TEN_GIGABIT_ETHERNET = "TenGigabitEthernet"
TEN_GIG_E = "TenGigE"

FORTY_GIGABIT_ETHERNET = "FortyGigabitEthernet"
FORTY_GIG_E = "FortyGigE"


def join(values):
    return "".join(values)
