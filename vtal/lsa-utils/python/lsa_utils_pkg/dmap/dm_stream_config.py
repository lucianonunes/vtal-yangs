from ncs.dp import take_worker_socket, return_worker_socket
from ncs.dp import StateManager
from ncs.dp import Daemon

# A placeholder for dispatch-map-update notification stream context
notifCtx = None


class DispatchMapNotifStreamStateManager(StateManager):
    def __init__(self, log):
        self.log = log
        StateManager.__init__(self, log)

    def setup(self, state, previous_state):
        global notifCtx
        notifCtx = self.register_notification_stream(
            state,
            None,
            take_worker_socket(state, "lsa-utils-dmap-events-name", "lsa-utils-dmap-events-key"),
            "dispatch-map-update",
        )

    def teardown(self, state, finished):
        return_worker_socket(state, "lsa-utils-dmap-events-key")


class DispatchMapNotifStreamDaemon(object):
    def __init__(self, log):
        sm = DispatchMapNotifStreamStateManager(log)
        self.notif_stream_daemon = Daemon("lsa-utils-dispatch-map-events-notif", state_mgr=sm)
        self.notif_stream_daemon.start()

    def cleanup(self):
        self.notif_stream_daemon.finish()
