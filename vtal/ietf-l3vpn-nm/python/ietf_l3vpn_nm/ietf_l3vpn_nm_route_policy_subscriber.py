# -*- mode: python; python-indent: 4 -*-
import ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from cisco_tsdn_core_fp_common.utils import is_ha_slave


class L3vpnRoutePolicySubscriber(ncs.cdb.Subscriber):
    """
    This subscriber subscribes to /l3vpn-route-policy and accordingly updates
    /l3vpn-ntw/vpn-profiles/valid-provider-identifiers/routing-profile-identifier
    when create or delete happens
    """

    def init(self):
        self.sub_path = "/cisco-flat-L3vpn-fp:l3vpn-route-policy"
        self.register(self.sub_path)
        self.log.debug("L3vpnRoutePolicySubscriber: subscribed")

    def pre_iterate(self):
        return {"created": set(), "deleted": set()}

    def should_iterate(self):
        if is_ha_slave():
            self.log.debug("L3vpn-RoutePolicy-Subscriber: HA role is slave, skipping iteration")
            return False
        else:
            return True

    def iterate(self, kp, op, oldv, newv, state):
        self.log.debug(f"L3vpnRoutePolicySubscriber: iterator: kp = {str(kp)}")
        self.log.debug(f"L3vpnRoutePolicySubscriber: iterator: op = {str(op)}")
        policy_name = str(kp[0][0])
        if op == ncs.MOP_CREATED:
            self.log.debug("L3vpnRoutePolicySubscriber: is MOP_CREATED")
            self._state_update(state, "created", policy_name)
        elif op == ncs.MOP_DELETED:
            self.log.debug("L3vpnRoutePolicySubscriber: is MOP_DELETED")
            self._state_update(state, "deleted", policy_name)

        return ncs.ITER_CONTINUE

    def _state_update(self, state, kind_of_operation, policy_name):
        state[kind_of_operation].add(policy_name)
        return state

    def should_post_iterate(self, state):
        return len(state["created"]) > 0 or len(state["deleted"]) > 0

    def post_iterate(self, state):
        state_delete = state["deleted"] or "{}"
        state_create = state["created"] or "{}"
        self.log.info(f"Handling l3vpn-route-policy deletion: {state_delete}, "
                      f"creation: {state_create}")
        with maapi.single_write_trans("", "system") as th:
            l3nm_profile_path = \
                "/l3vpn-ntw/vpn-profiles/valid-provider-identifiers/routing-profile-identifier"
            l3nm_profile_node = maagic.get_node(th, l3nm_profile_path)
            for policy_name in state["deleted"]:
                try:
                    del l3nm_profile_node[policy_name]
                except Exception as e:
                    self.log.error(f"Could not delete routing-profile-identifier {policy_name}: "
                                   f"{type(e)} {e}")

            for policy_name in state["created"]:
                try:
                    l3nm_profile_node.create(policy_name)
                except Exception as e:
                    self.log.error(f"Could not create routing-profile-identifier {policy_name}: "
                                   f"{type(e)} {e}")
            th.apply()
        self.log.info("l3vpn-route-policy changes handled")
