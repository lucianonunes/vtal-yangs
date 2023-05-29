# -*- mode: python; python-indent: 4 -*-
import ncs

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info('custom-template RUNNING')

        self.register_action("ct-change-netconf-notification-handler",
                             CtNetconfNotificationHandler)

    def teardown(self):
        self.log.info('custom-template FINISHED')


class CtNetconfNotificationHandler(ncs.dp.Action):
    """
    Action handler for RFS ct change notification on CFS node
    """
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Handle CT notif change for: {input.path}")
        with ncs.maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            ct_info = ncs.maagic.get_root(th).custom_template_info
            event = ncs.maagic.get_node(th, input.path)
            rfs_node = ncs.maagic.cd(event, "../../../../../ncs:name")
            for ct_template in event.custom_template_info:
                template_name = ct_template.template_name
                if (template_name not in ct_info) or (template_name in ct_info and
                                                      (not(ct_info[template_name].rfs_node) or
                                                       ct_info[template_name].rfs_node == rfs_node)
                                                      ):
                    ct_vars = ncs.template.Variables()
                    ct_vars.add('RFS_NODE', rfs_node)
                    operation = ct_template.operation
                    if operation == "created":
                        template = ncs.template.Template(ct_template)
                        template.apply('cisco-custom-template-info', ct_vars)
                    if operation == "deleted":
                        del ct_info[ct_template.template_name]
                    if operation == "modified":
                        del ct_info[ct_template.template_name]
                        template = ncs.template.Template(ct_template)
                        template.apply('cisco-custom-template-info', ct_vars)
                else:
                    ct_info_rfs_node = ct_info[ct_template.template_name].rfs_node
                    self.log.error(f'Template {ct_template.template_name} already '
                                   f'exists for rfs-node {ct_info_rfs_node} '
                                   f'so cannot be updated for rfs-node {rfs_node}')
            th.apply()
            self.log.info(f"Done Handling CT notif change for: {input.path}")
