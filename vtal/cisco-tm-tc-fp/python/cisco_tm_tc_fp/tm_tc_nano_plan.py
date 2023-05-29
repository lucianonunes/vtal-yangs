from . import tm_tc_internal_handler

from . import utils

import _ncs
import ncs
import ncs.maagic as maagic


class TMTCCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        localhost = "127.0.0.1"  # NOSONAR

        # If operation is create or update, check interfaces on device and validate
        if op == _ncs.dp.NCS_SERVICE_CREATE or op == _ncs.dp.NCS_SERVICE_UPDATE:
            service = maagic.get_node(maagic.get_trans(root), kp)
            self.log.info("Pre-mod for service: {}, operation: {}".format(service.name, op))

# commented the code for external interface check for traffic collector, as this not as per the device behaviour

            """
            tm_tc_validation_enabled = root.cisco_tm_tc_fp__cfp_configurations. \
                tm_tc_validation_enabled

            # Validate traffic collector external interface
            if tm_tc_validation_enabled:
                for node in service.node:
                    if root.devices.device[node.name].address == localhost:
                        continue
                    else:
                        router = utils.get_device_impl_default_class(self, root, service, node.name,
                                                        root.cisco_tm_tc_fp__cfp_configurations. \
                                                        dynamic_device_mapping)
                        if node.traffic_collector:
                            tc_ext_interfaces = node.traffic_collector.external_interface
                            for tc_ext_interface in tc_ext_interfaces:
                                if tc_ext_interface.if_type == "Loopback":
                                    continue
                                tc_interface = str(
                                    tc_ext_interface.if_type) + tc_ext_interface.if_id
                                if not router.check_if_interface_exists(root, node, \
                                                                      str(tc_ext_interface.if_type),
                                                                      tc_ext_interface.if_id):
                                    raise Exception("Provided external interface {} \
                                                        not present on node {}.".
                                                    format(tc_interface, node.name))
            """

class TMTCExternalSelfCallback(ncs.application.NanoService):
    '''
    Nano Service callback handler for tm-tc-plan
    '''

    @ncs.application.NanoService.create
    def cb_nano_create(
            self, tctx, root, service, plan, component, state, opaque, comp_var):

        if state == "ncs:init":
            # Check for stacked service
            if root.cisco_tm_tc_fp__cfp_configurations.stacked_service_enabled.exists():
                # Create node tm-tc services
                tm_tc_service = root.cisco_tm_tc_fp__tm_tc[service.name]
                for node in tm_tc_service.node:
                    self.log.info("Inside Nano Plan Stacked Service Create for: {} {}"
                                  .format(service.name, node.name))
                    tm_tc_internal_handler. \
                        create_internal_service(tm_tc_service, service.name, node.name)
        elif state == "ncs:ready":
            self.log.info("Nano plan state for service {} is {}. Updating the plan status accordingly".format(service.name, state))
            self._create_ready(plan, component, state)
        return opaque

    def _create_ready(self, plan, component, state):
        for plan_component in plan.component:
            if not (plan_component.name == "self" and \
                    plan_component.type == "ncs:self"):
                for plan_state in plan_component.state:
                    if plan_state.status == "failed" or \
                            plan_state.post_action_status == "failed":
                        self.log.debug("Component '{}' state '{}' failed, "
                                       "setting self ready state to "
                                       "failed".format(component, state))
                        plan.component[component].state[state].status = "failed"


class TMTCExternalNodeCallback(ncs.application.NanoService):
    '''
    Nano Service callback handler for tm-tc-plan
    '''

    @ncs.application.NanoService.create
    def cb_nano_create(
            self, tctx, root, service, plan, component, state, opaque, comp_var):

        if state == 'ncs:init':
            self._create_init(service, comp_var)
        elif state == "cisco-tm-tc-fp-nano-plan-services:config-apply":
            self.log.info("Nano plan state for node level service {} is {}. Applying the config".format(service.name, state))
            self._create_config_apply(root, service, plan, component, state, comp_var)
        elif state == "ncs:ready":
            self.log.info("Nano plan state for node level service {} is {}. Updating the plan status accordingly".format(service.name, state))
            self._create_ready(root, service, plan, component, state, comp_var)

        return opaque

    @ncs.application.NanoService.delete
    def cb_nano_delete(
            self, tctx, root, service, plan, component, state, opaque, comp_var):
        new_opaque = None
        self.log.info("Inside Nano Plan Node Delete for: {} {}".format(service.name, state))

        if state == "cisco-tm-tc-fp-nano-plan-services:config-apply":
            new_opaque = self._delete_config_apply(
                tctx, root, service, plan, component, state, opaque, comp_var)

        if new_opaque is None:
            return opaque
        return new_opaque

    def _create_init(self, service, comp_vars):

        comp_vars = dict(comp_vars)

        # Create internal-oper-data kicker
        vars = ncs.template.Variables()
        template = ncs.template.Template(service)
        vars.add("NODE", comp_vars["NODE"])
        vars.add("NAME", comp_vars["NAME"])
        self.log.info('Applying kicker template for tm-tc-internal-oper-data changes: ', \
                      service.name)
        template.apply('cisco-tm-tc-fp-internal-oper-data-kicker', vars)

    def _create_config_apply(
            self, root, service, plan, component, state, comp_vars):
        self.log.debug("Start config apply")

        comp_vars = dict(comp_vars)
        state_node = plan.component[component].state[state]
        internal_oper_result = root.cisco_tm_tc_fp_internal__tm_tc_internal. \
            tm_tc_oper_data

        if (service.name, comp_vars["NODE"]) in internal_oper_result:
            internal_oper_result_instance = internal_oper_result[service.name, comp_vars["NODE"]]
            if internal_oper_result_instance.status == 'failed' \
                    and internal_oper_result_instance.status_type == 'service-commit':
                state_node.status = "failed"
                if internal_oper_result_instance.status_message is not None:
                    plan.error_info.create()
                    plan.error_info.message = internal_oper_result_instance.status_message
            else:
                state_node.status = "reached"
        self.log.debug("Start config end")
    
    def _create_ready(self,root, service, plan, component, state, comp_var):
        self.log.debug("Start create ready for external node call back")

        comp_vars = dict(comp_var)
        internal_plan_result = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_plan

        status_node = plan.component[component].state[state]

        node_name = comp_vars["NODE"]
        service_name = service.name

        if (service_name, node_name) in internal_plan_result:
            result = internal_plan_result[service.name, comp_vars["NODE"]]
            self.log.info("Checking if the internal plan has any commit queue pending")
            if result.plan.commit_queue:
                self.log.info("Internal plan {}, node {} has pending commit queue".format(service_name, node_name))
                if len(result.plan.commit_queue.queue_item) > 0:
                    status_node.status = "not-reached"

    def _delete_config_apply(
            self, tctx, root, service, plan, component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        state_node = plan.component[component].state[state]

        # Check if internal plan has been removed, if so then return
        internal_plan = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_plan
        internal_oper_result = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_oper_data

        if (service.name, comp_vars["NODE"]) not in internal_plan:
            self.log.info("No internal plan: {} {}".format(service.name, comp_vars["NODE"]))
            if (service.name, comp_vars["NODE"]) in internal_oper_result:
                del internal_oper_result[service.name, comp_vars["NODE"]]
            return opaque
        elif (service.name, comp_vars["NODE"]) in internal_oper_result:
            self.log.info("Failed internal-oper-data: {} {}".format(service.name,
                                                                    comp_vars["NODE"]))
            internal_oper_result_instance = internal_oper_result[service.name, comp_vars["NODE"]]
            if internal_oper_result_instance.status == 'failed' \
                    and internal_oper_result_instance.status_type == 'service-commit':
                state_node.status = "failed"
                if internal_oper_result_instance.status_message is not None:
                    plan.error_info.create()
                    plan.error_info.message = internal_oper_result_instance.status_message
        return opaque
