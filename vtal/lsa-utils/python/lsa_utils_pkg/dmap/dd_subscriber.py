# -*- mode: python; python-indent: 4 -*-
from typing import Dict, List, Tuple, Union
from ncs import ITER_WANT_ANCESTOR_DELETE, ITER_CONTINUE, ITER_RECURSE
from ncs import MOP_VALUE_SET, MOP_DELETED, OPERATIONAL
from ncs import maagic, maapi
from ncs.cdb import Subscriber
from .dm import process_dd_change


class DeviceTreeSubscriber(Subscriber):
    """
    Subscriber to monitor the '/devices/device' tree and
    updates local dispatch-map or send notifications  addition/deletion of device.
    """

    def init(self):
        # TODO: Improve the subscriber, this is triggered for every device change.
        self.register("/ncs:devices/ncs:device/ncs:name", iter_flags=ITER_WANT_ANCESTOR_DELETE)
        self.register("/ncs:devices/ncs:device/ncs:device-type")

    def pre_iterate(self):
        return {"added": [], "deleted": []}

    def should_iterate(self):
        if is_ha_standby():
            self.log.debug("DeviceTreeSubscriber: HA role is standby, skipping iteration")
            return False
        return True

    def iterate(self, kp, op, oldv, newv, state):
        # kp: /ncs:devices/device{xyz} Op: MOP_DELETED
        if str(kp[1]) == "device" and str(kp[2]) == "devices":
            key = kp[0][0]
            if op == MOP_DELETED:
                state["deleted"].append(str(key))
                return ITER_CONTINUE
        # kp: /ncs:devices/device{xyz}/device-type/cli/ned-id Op: MOP_VALUE_SET
        elif op == MOP_VALUE_SET and str(kp[0]) == "ned-id":
            key = kp[3][0]
            ned_id = str(newv) + ":" + str(newv)
            state["added"].append(tuple((str(key), ned_id)))
            return ITER_CONTINUE
        return ITER_RECURSE

    def should_post_iterate(self, state: Dict[str, List[Union[str, Tuple[str, str]]]]) -> bool:
        '''Identifies if post_iterate should be handled
        '''
        self.log.info(f"Updated devices: {state}")
        return not (state["added"] == [] and state["deleted"] == [])

    def post_iterate(self, state: Dict[str, List[str]]):
        '''Handle post subscription activity
        '''
        process_dd_change(self.log, state["added"], state["deleted"])


def is_ha_standby() -> bool:
    '''identifies if the NSO is a HA standby node
    '''
    with maapi.single_read_trans("", "system", db=OPERATIONAL) as trans:
        if trans.exists("/tfnm:ncs-state/tfnm:ha"):
            mode = str(maagic.get_node(trans, "/tfnm:ncs-state/tfnm:ha/tfnm:mode"))
            return mode != "master"
        return False
