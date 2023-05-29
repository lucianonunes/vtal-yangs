import ncs

from . import tm_tc_actions
from . import tm_tc_nano_plan
from . import tm_tc_nano_services
from . import tm_tc_subscriber
from . import tm_tc_alarm_subscriber
from . import tm_tc_fp_config


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------
class Main(ncs.application.Application):
    def setup(self):
        self.log.info('cisco-tm-tc-fp RUNNING')

        # TM-TC SUBSCRIBERS
        self.tm_tc_sub = tm_tc_subscriber.TMTCSubscriber(self)
        self.tm_tc_sub.start()

        # TM-TC FP CONFIG
        self.register_service("tm-tc-fp-config", tm_tc_fp_config.TMTCCfgCallBack)

        # TM-TC NANO PREMOD
        self.register_service("tm-tc-external-servicepoint",
                              tm_tc_nano_plan.TMTCCallBack)

        # TM-TC EXTERNAL PLAN NANO
        self.register_nano_service("tm-tc-external-servicepoint",
                                   "ncs:self",
                                   "ncs:init",
                                   tm_tc_nano_plan.TMTCExternalSelfCallback)
        self.register_nano_service("tm-tc-external-servicepoint",
                                   "ncs:self",
                                   "ncs:ready",
                                   tm_tc_nano_plan.TMTCExternalSelfCallback)
        self.register_nano_service("tm-tc-external-servicepoint",
                                   "cisco-tm-tc-fp-nano-plan-services:node",
                                   "ncs:init",
                                   tm_tc_nano_plan.TMTCExternalNodeCallback)
        self.register_nano_service("tm-tc-external-servicepoint",
                                   "cisco-tm-tc-fp-nano-plan-services:node",
                                   "cisco-tm-tc-fp-nano-plan-services:config-apply",
                                   tm_tc_nano_plan.TMTCExternalNodeCallback)
        self.register_nano_service("tm-tc-external-servicepoint",
                                   "cisco-tm-tc-fp-nano-plan-services:node",
                                   "ncs:ready",
                                   tm_tc_nano_plan.TMTCExternalNodeCallback)

        # TM-TC EXTERNAL ACTIONS
        self.register_action("tm-tc-node-redeploy-actionpoint", tm_tc_actions.TMTCNodeRedeploy)
        self.register_action("tm-tc-global-redeploy-actionpoint", tm_tc_actions.TMTCRedeploy)
        self.register_action("tm-tc-get-modifications-actionpoint",
                             tm_tc_actions.TMTCGetModifications)
        self.register_action("cisco-tm-tc-fp-cleanup", tm_tc_actions.TMTCCleanupAction)

        self.register_action("cisco-tm-tc-fp-status", tm_tc_actions.TMTCStatusAction)

        # TM-TC INTERNAL NANO SERVICE
        self.register_nano_service("tm-tc-internal-servicepoint",
                                   "ncs:self",
                                   "cisco-tm-tc-fp-nano-services:config-apply",
                                   tm_tc_nano_services.TMTCSelfCallback)

        # TM-TC INTERNAL ACTIONS
        self.register_action("tm-tc-no-op-actionpoint", tm_tc_actions.NoOpAction)
        self.register_action("tm-tc-purge-actionpoint", tm_tc_actions.TMTCPurgeOper)

        # ALARM SUBSCRIBERS
        self.alarm_sub = tm_tc_alarm_subscriber.TMTCAlarmSubscriber(self)
        self.alarm_sub.start()

    def teardown(self):
        self.tm_tc_sub.stop()
        self.alarm_sub.stop()
        self.log.info('cisco-tm-tc-fp FINISHED')
