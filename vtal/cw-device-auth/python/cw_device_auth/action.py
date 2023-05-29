# -*- mode: python; python-indent: 4 -*-
"""
Crosswork Change Automation device override credentials

Authors: Nitish Vashishtha (nvashish@cisco.com), Sanshit Sharma (sansshar@cisco.com)
Email: support@cisco.com
Since: Nov 2020
https://www.cisco.com/public/sw-license-agreement.html
Copyright (c) 2020 by Cisco Systems, Inc.
All Rights Reserved
"""

# pylint: disable=import-error
import _ncs
import ncs
from ncs import maagic, maapi
from ncs.dp import Action
# pylint: enable=import-error


class App(ncs.application.Application):
    """Component thread that will started by NCS.
    """
    def setup(self):
        """When this application is started (which would happen when this
        package is loaded or relaoded in NCS) this setup method will be called.
        """
        # The application class sets up logging for us. It is accessible
        # through 'self.log' and is a ncs.log.Log instance.
        self.log.info('RUNNING Crosswork device auth actions app')

        # When using actions, this is how we register them:
        #
        self.register_action('cw-creds-set-actionpoint', SetCredsAction)
        self.register_action('cw-creds-get-actionpoint', GetCredsAction)
        self.register_action('cw-creds-del-actionpoint', DelCredsAction)

        # If we registered any callback(s) above, the Application class
        # took care of creating a daemon (related to the service/action point).

        # When this setup method is finished, all registrations are
        # considered done and the application is 'started'.

    def teardown(self):
        """When this application is finished (which would happen if NCS went
        down, packages were reloaded or some error occurred) this teardown
        method will be called.
        """
        self.log.info('FINISHED Crosswork device auth actions app')

# ---------------------------------------------------------------------------- #
# ACTIONS                                                                      #
# ---------------------------------------------------------------------------- #

_CRED_STORE_PATH = '/cw-device-auth:cw-creds-store'
_TXN_USER = 'admin'
_TXN_CONTEXT = 'cw-device-auth'

# pylint: disable=redefined-builtin

class SetCredsAction(Action):
    """
    """
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        """
        """
        self.log.info('started action: [{}/{}], user: {}'.format(
            kp, name, uinfo.username))
        self.log.info('device(s): {}]'.format(input.devices.as_list()))

        #TODO: how can this fail? handle it
        try:
            # write to oper db TODO: change user?
            with maapi.single_write_trans(_TXN_USER,
                                          _TXN_CONTEXT,
                                          db=ncs.OPERATIONAL) as txn:
                store = maagic.get_node(txn, _CRED_STORE_PATH)
                for device in input.devices:
                    # the creation is idempotent
                    cred = store.credentials.create(device)
                    cred.uname = input.uname
                    cred.passwd = input.passwd
                    txn.apply()
        except Exception as err:
            output.message = str(err)
            self.log.error('action failed: [{}/{}], err: [{}]'.format(
                kp, name, repr(err)))
            output.status = 1
            return

        output.status = 0
        output.message = "ok"
        self.log.info('action completed: [{}/{}]'.format(kp, name))


class DelCredsAction(Action):
    """
    """
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        """
        """
        self.log.info('started action: [{}/{}], user: {}'.format(
            kp, name, uinfo.username))
        self.log.info('device(s): {}]'.format(input.devices.as_list()))

        #TODO: how can this fail? handle it
        try:
            # write to oper db TODO: change user?
            with maapi.single_write_trans(_TXN_USER,
                                          _TXN_CONTEXT,
                                          db=ncs.OPERATIONAL) as txn:
                store = maagic.get_node(txn, _CRED_STORE_PATH)
                for device in input.devices:
                    # the deletion is idempotent
                    if store.credentials.exists(device):
                        del store.credentials[device]
                txn.apply()
        except Exception as err:
            output.message = str(err)
            self.log.error('action failed: [{}/{}], err: [{}]'.format(
                kp, name, repr(err)))
            output.status = 1
            return

        output.status = 0
        output.message = "ok"
        self.log.info('action completed: [{}/{}]'.format(kp, name))


class GetCredsAction(Action):
    """
    """
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        """
        """
        self.log.info('started action: [{}/{}], user: {}'.format(
            kp, name, uinfo.username))
        self.log.info(
            'input: [local-user: {}, authgroup: {}, device: {}]'.format(
                input.local_user, input.authgroup, input.device))

        # read from oper db
        #TODO: how can this fail? handle it
        try:
            with ncs.maapi.Maapi() as m_conn:
                # TODO change user?
                with ncs.maapi.Session(m_conn, _TXN_USER, _TXN_CONTEXT):
                    with m_conn.start_read_trans(db=ncs.OPERATIONAL) as txn:
                        try:
                            # TODO: how to get path where creds are set dynamically?
                            store = ncs.maagic.get_node(txn, _CRED_STORE_PATH)
                            cred = store.credentials[input.device]
                            output.remote_user = cred.uname
                            m_conn.install_crypto_keys()
                            key = cred.passwd
                            output.remote_password = _ncs.decrypt(key)
                        except KeyError:
                            self.log.info(
                                'failed to find credentials for device: {}'.format(
                                    input.device))
        except Exception as err:
            self.log.error('action failed: [{}/{}], err: [{}]'.format(
                kp, name, repr(err)))
            output.remote_user = ''
            output.remote_password = ''
            return

        self.log.info('action completed: [{}/{}]'.format(kp, name))

