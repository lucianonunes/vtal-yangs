# -*- mode: python; python-indent: 4 -*-
from ncs.application import Application
from .dmap.dm_stream_config import DispatchMapNotifStreamDaemon
from .dmap.dd_subscriber import DeviceTreeSubscriber
from .namespaces.lsaUtils_ns import ns as lsa_ns
from .dmap.dm_actions import DispatchMapUpdateNotifCallback, SyncDispatchMap

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.

        # --- :: START :: Dispatch Map :: -----
        self.notif_reg = DispatchMapNotifStreamDaemon(self.log)
        self.dd_sub = DeviceTreeSubscriber(self)
        self.dd_sub.start()

        self.register_action(
            lsa_ns.actionpoint_internal_dispatch_map_callback, DispatchMapUpdateNotifCallback
        )
        self.register_action(lsa_ns.actionpoint_lsa_utils_sync_dispatch_map, SyncDispatchMap)

    def teardown(self):
        self.dd_sub.stop()
        self.notif_reg.cleanup()
        self.log.info("LSA-Utils Main FINISHED")
