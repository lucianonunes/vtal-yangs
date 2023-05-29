# -*- mode: python; python-indent: 4 -*-
import ncs
from ncs.application import Service

# ------------------------------------------
# Generate ISIS System NET ID from Loopback
# -------------------------------------------
def generate_net_id_from_loopback0(Loopback):
    ip_octates = Loopback.split('.')
    loopback_len = len(ip_octates)

    sys_id_1 = ''
    if (loopback_len == 4):
        for oc in ip_octates:
            if len(oc) == 1:
                sys_id_1 = sys_id_1 + '00' + oc
            elif len(oc) == 2:
                sys_id_1 = sys_id_1 + '0' + oc
            else:
                sys_id_1 = sys_id_1 + oc

    len_toc = len(sys_id_1)
    # print(len_toc)
    len_ned_oc = int(len_toc / 4)
    SYSTEM_ID = ''
    for i in range(len_toc):
        SYSTEM_ID = SYSTEM_ID + sys_id_1[i]
        if (i == 3 or i == 7):
            SYSTEM_ID = SYSTEM_ID + '.'


    return SYSTEM_ID


# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------
class ServiceCallbacks(Service):

    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @Service.create
    def cb_create(self, tctx, root, service, proplist):
        self.log.info('Service create(service=', service._path, ')')

        str_loopback=str(service.loopback_ipv4_address)
        isis_net_id = generate_net_id_from_loopback0(str_loopback)

        int_abs_prefix = int(service.abs_prefix_sid)
        strict_prefix_sid = int_abs_prefix+1000
        vars = ncs.template.Variables()
        vars.add('ISIS_SYS_NET', isis_net_id)
        vars.add('STRICT_PREFIX_SID', strict_prefix_sid)
        #vars.add('PHYSICAL_INTERFACE_TYPE', service.PHY_INTF_TYPE)

        template = ncs.template.Template(service)
        template.apply('sr-mpls-with-isis-net-id-template', vars)
    # The pre_modification() and post_modification() callbacks are optional,
    # and are invoked outside FASTMAP. pre_modification() is invoked before
    # create, update, or delete of the service, as indicated by the enum
    # ncs_service_operation op parameter. Conversely
    # post_modification() is invoked after create, update, or delete
    # of the service. These functions can be useful e.g. for
    # allocations that should be stored and existing also when the
    # service instance is removed.

    # @Service.pre_lock_create
    # def cb_pre_lock_create(self, tctx, root, service, proplist):
    #     self.log.info('Service plcreate(service=', service._path, ')')

    # @Service.pre_modification
    # def cb_pre_modification(self, tctx, op, kp, root, proplist):
    #     self.log.info('Service premod(service=', kp, ')')

    # @Service.post_modification
    # def cb_post_modification(self, tctx, op, kp, root, proplist):
    #     self.log.info('Service postmod(service=', kp, ')')


# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------
class Main(ncs.application.Application):
    def setup(self):
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info('Main RUNNING')

        # Service callbacks require a registration for a 'service point',
        # as specified in the corresponding data model.
        #
        self.register_service('sr-mpls-with-isis-net-id-servicepoint', ServiceCallbacks)

        # If we registered any callback(s) above, the Application class
        # took care of creating a daemon (related to the service/action point).

        # When this setup method is finished, all registrations are
        # considered done and the application is 'started'.

    def teardown(self):
        # When the application is finished (which would happen if NCS went
        # down, packages were reloaded or some error occurred) this teardown
        # method will be called.

        self.log.info('Main FINISHED')
