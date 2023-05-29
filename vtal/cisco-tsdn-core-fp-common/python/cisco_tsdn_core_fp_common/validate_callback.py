from ncs.dp import TransactionCallback
from ncs.dp import StateManager
from ncs.dp import Daemon


class TransValCb(TransactionCallback):
    def cb_stop(self, tctx):
        return TransactionCallback.cb_finish(self, tctx)


class ValidateSM(StateManager):
    def __init__(self, log, vcp_name, vcb_obj):
        self.vcp_name = vcp_name
        self.vcb_obj = vcb_obj
        StateManager.__init__(self, log)

    def setup(self, state, previous_state):
        self.register_trans_validate_cb(state, TransValCb(state))
        self.register_valpoint_cb(state, self.vcp_name, self.vcb_obj)


class ValPointRegistrar(object):
    """
    As per RT #37128, this extra daemon initialization is not required in NSO-5.1.
    """

    def __init__(self, log, dmn_name, vcp, vcb):
        sm = ValidateSM(log, vcp, vcb)
        self.val_daemon = Daemon(dmn_name, state_mgr=sm)
        self.val_daemon.register_trans_cb(TransValCb)
        self.val_daemon.start()

    def cleanup(self):
        self.val_daemon.finish()
