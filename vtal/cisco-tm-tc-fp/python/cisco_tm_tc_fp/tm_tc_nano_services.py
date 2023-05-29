from . import utils
import ncs


class TMTCSelfCallback(ncs.application.NanoService):
    """
    Internal Nano Service callback handler for tm-tc service node
    """

    @ncs.application.NanoService.create
    def cb_nano_create(
            self, tctx, root, service, plan, component, state, opaque, comp_var):

        if state == "cisco-tm-tc-fp-nano-services:config-apply":
            self.log.info("Inside Nano Create, state is {} for service {}. Applying the temmplate".format(state,service.name))
            self._create_config_apply(root, service)

        return opaque

    def _create_config_apply(self, root, service):
        # Apply template to each node
        for node in service.node:
            # One node per internal service
            router = utils.get_device_impl_default_class(self, root, service, node.name,
                                                         root.cisco_tm_tc_fp__cfp_configurations. \
                                                         dynamic_device_mapping)
            router.conf_tm_tc(node)

            # Apply global level custom-template
            if service.custom_template:
                utils.apply_custom_template(self, root, service, node.name)

            # Apply local level custom-template
            if node.custom_template:
                utils.apply_custom_template(self, root, node, node.name)
