import ncs
import _ncs
import ncs.cdb
import ncs.maapi as maapi
import ncs.maagic as maagic
from .tm_tc_internal_handler import TMTCInternalHandler
from .tm_tc_redeploy_handler import TMTCRedeployHandler
from . import utils


class TMTCSubscriber(ncs.cdb.Subscriber):
    """
    tm-tc Subscriber subscribes to changes in '/cisco-tm-tc-fp:tm-tc' and writes to internal
    tm-tc service model
    """

    def init(self):
        self.register('/cisco-tm-tc-fp:tm-tc')

    def pre_iterate(self):
        return {"added": [],
                "deleted": [],
                "modified": [],
                "deleted-node": [],
                "added-node": [],
                "updated-node": [],
                "created-node": [],
                "no-change-redeploy": True}

    def should_iterate(self):
        # For HA check if current host is master so we can write
        if utils.is_ha_slave():
            self.log.debug("TMTC: HA role is slave, skipping iteration")
            return False
        return True

    def iterate(self, kp, op, oldv, newv, state):
        # Indicate if redeploy or reactive-redeploy
        if (len(kp) > 2 and str(kp[1]) != "private") or (len(kp) == 2 and op != ncs.MOP_MODIFIED):
            state["no-change-redeploy"] = False
        if str(kp[1]) == 'tm-tc':
            key = kp[0][0]
            if op == ncs.MOP_CREATED:
                state["added"].append(key)
            elif op == ncs.MOP_DELETED:
                state["deleted"].append(key)
        # Node is deleted
        elif str(kp[1]) == "node" and op == ncs.MOP_DELETED:
            # Collect all deleted nodes
            node = kp[0][0]
            tm_tc_name = kp[2][0]
            state["deleted-node"].append((tm_tc_name, node))
            return ncs.ITER_CONTINUE
        # Node is added/modified
        elif str(kp[1]) == "node" and (op == ncs.MOP_CREATED or op == ncs.MOP_MODIFIED):
            # Collect all added nodes
            node = kp[0][0]
            tm_tc_name = kp[2][0]
            state["added-node"].append((tm_tc_name, node))
            if op == ncs.MOP_CREATED:
                state["created-node"].append((tm_tc_name, node))
            elif op == ncs.MOP_MODIFIED:
                state["updated-node"].append((tm_tc_name, node))
            return ncs.ITER_CONTINUE
        # Other service config modifications
        else:
            key = kp[len(kp) - 2][0]
            if (key not in state["added"]) and ("private" != str(kp[1])):
                state["added"].append(key)
                state["modified"].append(key)
                return ncs.ITER_CONTINUE
        # Continue recursing until all node updates and config updates are found
        return ncs.ITER_RECURSE

    def should_post_iterate(self, state):
        self.log.info("Updated data: {}".format(state))
        return not (state["no-change-redeploy"] \
                    or (state["added"] == [] and state["deleted"] == [] \
                        and state["added-node"] == [] and state["deleted-node"] == []))

    def post_iterate(self, state):
        if utils.is_stacked_service():
            self._stacked_service_based(state)
        else:
            self._subscriber_based(state)

    def _subscriber_based(self, state):

        context = "system"
        added = state["added"]
        removed = state["deleted"]
        removed_nodes = state["deleted-node"]
        added_nodes = state["added-node"]

        with maapi.single_read_trans("", context) as th:
            root = ncs.maagic.get_root(th)
            username = root.cisco_tm_tc_fp__cfp_configurations.local_user

            for key in added:
                tm_tc_kp = "/cisco-tm-tc-fp:tm-tc{{{}}}".format(key)
                self.log.info("TMTCSubscriber, added/updated: {}".format(tm_tc_kp))
                tm_tc_service = maagic.get_node(th, tm_tc_kp)
                for node in tm_tc_service.node:
                    tm_tc_handler = TMTCInternalHandler(username, key, node.name, "create",
                                                        self.log)
                    tm_tc_handler.start()

            # Newly added nodes or updated nodes
            for node in added_nodes:
                # Only create node if not already handled
                # This is a case where there is no config update,
                # but just a node was added into service or an existing node is modified
                if node[0] not in added:
                    tm_tc_handler = TMTCInternalHandler(username, node[0], node[1], "create",
                                                        self.log)
                    tm_tc_handler.start()

            # Remove nodes
            for node in removed_nodes:
                tm_tc_handler = TMTCInternalHandler(username, node[0], node[1], "delete", self.log)
                tm_tc_handler.start()

            for key in removed:
                nodes = self._get_all_internal_tm_tc_services(key)

                for node in nodes:
                    tm_tc_handler = TMTCInternalHandler(username, key, node, "delete", self.log)
                    tm_tc_handler.start()

    def _stacked_service_based(self, state):
        context = "system"
        created_nodes = state["created-node"]
        updated_nodes = state["updated-node"]
        removed_nodes = state["deleted-node"]
        modified = state["modified"]
        removed = state["deleted"]

        with maapi.single_read_trans("", context) as th:
            root = ncs.maagic.get_root(th)
            username = root.cisco_tm_tc_fp__cfp_configurations.local_user

            for key in modified:
                tm_tc_kp = ("/cisco-tm-tc-fp:tm-tc{{{}}}".format(key))

                tm_tc_service = maagic.get_node(th, tm_tc_kp)
                for node in tm_tc_service.node:
                    if (key, node.name) not in updated_nodes:
                        state["updated-node"].append((key, node.name))

            # Update internal oper-data of created nodes
            tm_tc_handler = TMTCRedeployHandler(username, created_nodes, "created", self.log)
            tm_tc_handler.start()
            # Update internal oper-data of updated nodes
            tm_tc_handler = TMTCRedeployHandler(username, updated_nodes, "updated", self.log)
            tm_tc_handler.start()

            # Delete internal oper-data of deleted nodes
            tm_tc_handler = TMTCRedeployHandler(username, removed_nodes, "deleted", self.log)
            tm_tc_handler.start()

            # Whole service is delete
            for key in removed:
                nodes = self._get_all_internal_tm_tc_services_by_oper_data(key)
                tm_tc_handler = TMTCRedeployHandler(username, nodes, "deleted", self.log)
                tm_tc_handler.start()

    def _get_all_internal_tm_tc_services(self, service_name):
        nodes = []
        with ncs.maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as th:
            qh = _ncs.maapi.query_start(th.maapi.msock, th.th,
                                        "/cisco-tm-tc-fp-internal:tm-tc"
                                        + "[name='" + str(service_name) + "']",
                                        '/', 0, 1,
                                        _ncs.QUERY_STRING, ["node-name"], [])

            res = _ncs.maapi.query_result(th.maapi.msock, qh)

            for r in res:
                nodes.append(r[0])

            _ncs.maapi.query_stop(th.maapi.msock, qh)
        return nodes

    def _get_all_internal_tm_tc_services_by_oper_data(self, service_name):
        nodes = []
        with ncs.maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as th:
            qh = _ncs.maapi.query_start(th.maapi.msock, th.th,
                                        "/cisco-tm-tc-fp-internal:tm-tc-internal"
                                        + "/cisco-tm-tc-fp-internal:tm-tc-oper-data"
                                        + "[name='" + str(service_name) + "']",
                                        '/', 0, 1,
                                        _ncs.QUERY_STRING, ["node-name"], [])

            res = _ncs.maapi.query_result(th.maapi.msock, qh)

            for r in res:
                nodes.append((service_name, r[0]))

            _ncs.maapi.query_stop(th.maapi.msock, qh)
        return nodes
