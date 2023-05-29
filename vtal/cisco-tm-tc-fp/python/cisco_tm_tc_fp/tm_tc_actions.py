import ncs
import _ncs
import ncs.maapi as maapi
import ncs.maagic as maagic
from ncs.dp import Action
import time
import traceback


class TMTCGetModifications(ncs.dp.Action):
    """
    Action handler for redeploying internal tm-tc services
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        try:
            with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
                root = maagic.get_root(th)
                action = ncs.maagic.get_node(th, kp)
                node = input.device
                service = ncs.maagic.cd(action, "..")
                service_key = (service.name, node)

                # If node exists and node's service exists in internal model, generate output
                if service_key in root.cisco_tm_tc_fp_internal__tm_tc:
                    output.message = self.create_output_message(root, input, service_key)
                # If node is not defined, then try to generate output based on internal tm-tc list
                elif not node:
                    output.message = ""
                    for service_node in service.node:
                        service_key = (service.name, service_node.name)
                        if service_key in root.cisco_tm_tc_fp_internal__tm_tc:
                            output.message += self.create_output_message(root, input, service_key)

                if not output.message or output.message == "":
                    output.message = "ERROR: No Internal Service"

        except Exception as e:
            self.log.error("ERROR: {}".format(traceback.print_exc()))
            output.message = "ERROR: {}".format(traceback.print_exc())

    def create_output_message(self, root, input, service_key):
        action_path = root.cisco_tm_tc_fp_internal__tm_tc[service_key].get_modifications
        input_nso = action_path.get_input()
        if input.outformat == "xml":
            input_nso.outformat = input.outformat
            action_output = action_path(input_nso)
            return action_output["result-xml"]["local-node"].data
        else:
            input_nso.deep.create()
            action_output = action_path(input_nso)
            return action_output.cli["local-node"].data


class TMTCRedeploy(ncs.dp.Action):
    """
    Action handler for redeploying internal tm-tc services
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        # Simply set redeploy flag to trigger redeploy action
        try:
            with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
                self.log.info("Redeploying tm-tc service: {}".format(kp))
                action = ncs.maagic.get_node(th, kp)
                service = ncs.maagic.cd(action, "..")
                # Set redeploy flag as a timestamp to guarantee uniqueness for future redeploys
                service.redeploy_flag = int(round(time.time() * 1000))
                th.apply()
                self.log.info("Redeployed service: {}".format(kp))
                output.status = "SUCCESS"
                output.message = "Service Redeployed"
        except Exception as e:
            self.log.error("Redeploy service failed: {}".format(traceback.print_exc()))
            output.status = "FAILED"
            output.message = "ERROR: {}".format(e)


class TMTCNodeRedeploy(ncs.dp.Action):
    """
    Action handler for redeploying internal tm-tc service per node
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        # Simply set redeploy flag to trigger redeploy action
        try:
            with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
                self.log.info("Redeploying node service: {}".format(kp))
                action = ncs.maagic.get_node(th, kp)
                node_service = ncs.maagic.cd(action, "..")
                # Set redeploy flag as a timestamp to guarantee uniqueness for future redeploys
                node_service.redeploy_flag = int(round(time.time() * 1000))
                th.apply()
                self.log.info("Redeployed node service: {}".format(kp))
                output.status = "SUCCESS"
                output.message = "Service Redeployed"
        except Exception as e:
            self.log.error("Redeploy node service failed: {}".format(traceback.print_exc()))
            output.status = "FAILED"
            output.message = "ERROR: {}".format(e)


class NoOpAction(ncs.dp.Action):
    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info("NO OP ACTION")


class TMTCCleanupAction(ncs.dp.Action):
    """
    Action handler for internal clean up
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        cleanup_log = []
        service_list = []
        status = True

        if input.match is True:
            service_list = self._get_matching_services(input.service, uinfo)
        else:
            service_list.append(input.service)
        for service in service_list:
            try:
                with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
                    self.log.info("Cleanup Action for name={} service={} device={}\
                                no-networking={}".format(name, service, input.device,
                                                        input.no_networking))

                    root = maagic.get_root(th)
                    cleanup_log.append("\nCleaning up TMTC service: {}".format(service))
                    self._cleanup_tm_tc_service(uinfo, service, input.device,
                                                input.no_networking, cleanup_log, root, th)
                    self.log.info("Cleanup Successful for {}".format(service))
                    cleanup_log.append("\n Cleanup Successful for {}".format(service))
                    output.detail = "".join(cleanup_log)

            except Exception as ex:
                self.log.error("Cleanup Action for {} failed: {}".format(input.service,
                                                                        traceback.print_exc()))
                status = False
                output.detail = "ERROR: {} {}".format(cleanup_log, ex)

        output.success = status

    def _get_matching_services(self, name, uinfo):
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            root = maagic.get_root(th)
            services = []
            for service_element in root.cisco_tm_tc_fp__tm_tc_plan:
                if name in service_element.name:
                    services.append(service_element.name)
            return services

    def _cleanup_tm_tc_service(self, uinfo, service, device, no_networking, cleanup_log,
                               root, th):
        force_back_track = []
        internal_force_back_track = []
        service_oper_paths = []
        internal_zombie_paths = []
        internal_service_paths = []

        tm_tc_plan_path = "/cisco-tm-tc-fp:tm-tc-plan{{{}}}".format(service)
        zombie_service_path = "/cisco-tm-tc-fp:tm-tc[name='{}']".format(service)
        service_path = "/cisco-tm-tc-fp:tm-tc{{{}}}".format(service)

        if th.exists(service_path) and \
                len(ncs.maagic.get_node(th, service_path + "/node")) <= 1:
            device = None
        # Collect service data to be cleaned up
        nodes = []
        if th.exists(tm_tc_plan_path) and device is None:
            tm_tc_components = root.cisco_tm_tc_fp__tm_tc_plan[service].plan.component
            for component in tm_tc_components:
                if component.type == "cisco-tm-tc-fp-nano-plan-services:node":
                    service_oper_paths.append("/cisco-tm-tc-fp-internal:"
                                              + "tm-tc-internal/tm-tc-oper-data"
                                              + "{{{} {}}}".format(service, component.name))
                    nodes.append(component.name)
                force_back_track.append(component.force_back_track)
            self.log.info("Nodes for {} tm-tc service: {}".format(service, nodes))
        elif device is not None:
            nodes.append(device)
            service_oper_paths.append("/cisco-tm-tc-fp-internal:tm-tc-internal/"
                                      + "tm-tc-oper-data"
                                      + "{{{} {}}}".format(service, device))
            if th.exists(tm_tc_plan_path):
                node_component = root.cisco_tm_tc_fp__tm_tc_plan[service].plan.component
                if ("cisco-tm-tc-fp-nano-plan-services:node", device) in \
                        node_component:
                    force_back_track.append(node_component \
                                                ["cisco-tm-tc-fp-nano-plan-services:node",
                                                 device].force_back_track)
        for node in nodes:
            internal_tm_tc_plan = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_plan
            if (service, node) in internal_tm_tc_plan:
                internal_tm_tc_components = internal_tm_tc_plan[service, node].plan.component
                for component in internal_tm_tc_components:
                    # Only self component in internal service
                    internal_force_back_track.append(component.force_back_track)
            internal_zombie_paths.append("/cisco-tm-tc-fp-internal:tm-tc[name='%s'][node-name='%s']" \
                                         %(service, node))
            internal_service_paths.append("/cisco-tm-tc-fp-internal:tm-tc{{{} {}}}" \
                                          .format(service, node))
        self._cleanup_service_data(uinfo, service, device, no_networking, cleanup_log, root, th,
                                   force_back_track, internal_force_back_track,
                                   service_oper_paths, internal_zombie_paths,
                                   internal_service_paths,
                                   zombie_service_path, service_path, "node")

    def _cleanup_service_data(self, uinfo, service, device, is_no_networking, cleanup_log, root, th,
                              force_back_track_comp, internal_force_back_track_comp,
                              service_oper_paths, internal_zombie_paths, internal_service_paths,
                              zombie_service_path, service_path, device_list_name_in_service):
        # force-backtrack all lower head-end services
        self._invoke_backtrack_actions(service, internal_force_back_track_comp,
                                       is_no_networking)

        # force-backtrack all upper service components
        self._invoke_backtrack_actions(service, force_back_track_comp, is_no_networking)

        cleanup_log.append("\n Removed all plan components")

        # remove zombies for lower services
        for internal_zombie_path in internal_zombie_paths:
            self.log.info("internal plan zombies: {}".format(internal_zombie_path))
            self._remove_zombie(internal_zombie_path, cleanup_log, uinfo)

        # remove zombies for upper service
        if device is None:
            self._remove_zombie(zombie_service_path, cleanup_log, uinfo)

        # delete upper service if exists
        if device is None:
            self._delete_service(service_path, cleanup_log, uinfo)
        else:
            # Remove device from upper service if exists
            self._delete_service(service_path + "/" + device_list_name_in_service
                                 + "{" + device + "}", cleanup_log, uinfo)

        # delete lower services if exists
        for internal_service_path in internal_service_paths:
            self._delete_service(internal_service_path, cleanup_log, uinfo)

        # remove oper data for all nodes
        self._remove_service_oper_data(service_oper_paths, cleanup_log, uinfo)

        # remove all side-effects for lower service
        for internal_zombie_path in internal_zombie_paths:
            side_effects = self._get_all_side_effect_for_service(internal_zombie_path)
            self.log.info("side_effects for service: {}".format(side_effects))
            self._remove_service_side_effects(side_effects, cleanup_log, uinfo)

        # remove all side-effects for upper service
        side_effects = self._get_all_side_effect_for_service(zombie_service_path)
        self.log.info("side_effects for service: {}".format(side_effects))
        self._remove_service_side_effects(side_effects, cleanup_log, uinfo)

        kickers = self._get_kicker_with_service(zombie_service_path)
        self.log.info("kickers for external service: {}".format(kickers))
        # Remove all lingering kickers for outer service
        if device is None:
            self._remove_kickers(kickers, cleanup_log, uinfo)
        else:
            # Remove kickers on this service pertaining to given head-end device
            self._remove_kickers_with_device(kickers, device, cleanup_log, uinfo)

        # Remove lingering kickers for inner services
        for internal_zombie_path in internal_zombie_paths:
            kickers = self._get_kicker_with_service(internal_zombie_path)
            self.log.info("kickers for internal service: {}".format(kickers))
            self._remove_kickers(kickers, cleanup_log, uinfo)

        # If the service is still present, we should re-deploy the service to reconcile
        # as the out of band deletion of data can leave the northbound plan in weird state.
        if device is not None and th.exists(service_path):
            external_service = ncs.maagic.get_node(th, service_path)
            external_service.re_deploy()

    def _run_action(self, service_name, fbt_action, fbt_input):
        """Executes an action and throws an exception if the result was not successful"""
        output = fbt_action(fbt_input)
        self.log.info("Cleanup result for {} {}: {}".format(fbt_action, service_name,
                                                             output.result))
        if not output.result:
            raise Exception("Failed cleaning instance {}: {}".format(service_name, output.info))

    def _invoke_backtrack_actions(self, service_name, back_track_actions, is_no_networking):
        """invokes the force-back-track actions"""
        for fbt_action in back_track_actions:
            fbt_input = fbt_action.get_input()
            if is_no_networking:
                fbt_input.no_networking.create()
            self._run_action(service_name, fbt_action, fbt_input)

    def _remove_zombie(self, zombie_service_path, cleanup_log, uinfo):
        """Checks for any remaining zombie services and removes them if there"""
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            kp = "/ncs:zombies/ncs:service{{\"{}\"}}".format(zombie_service_path)
            self.log.info("Checking for zombie {}".format(kp))
            if th.exists(kp):
                self.log.info("removing zombie service: {}".format(kp))
                cleanup_log.append("\n Removing zombie service: {}".format(kp))
                th.delete(kp)
                th.apply()
                cleanup_log.append("\n Removed zombie service")

    def _delete_service(self, service_path, cleanup_log, uinfo):
        """Deletes the northbound service """
        self.log.info("Deleting service {}".format(service_path))
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            if th.exists(service_path):
                cleanup_log.append("\n Removing service {}".format(service_path))
                th.delete(service_path)
                th.apply()
                cleanup_log.append("\n Removed service {}".format(service_path))

    def _delete_schedulers(self, schedulers, cleanup_log, uinfo):
        """Deletes schedulers created by service """
        self.log.debug("deleting schedulers {}".format(schedulers))
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            for scheduler in schedulers:
                scheduler_path = "/scheduler:scheduler/task{{{}}}".format(scheduler)
                if th.exists(scheduler_path):
                    cleanup_log.append("\n Removing schedulers {}".format(scheduler_path))
                    th.delete(scheduler_path)
            th.apply()
            cleanup_log.append("\n Removed schedulers")

    def _remove_service_oper_data(self, service_oper_paths, cleanup_log, uinfo):
        """Remove all service oper paths"""
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            for service_oper_path in service_oper_paths:
                if th.exists(service_oper_path):
                    self.log.info("removing service oper: {}".format(service_oper_path))
                    cleanup_log.append("\n Removing service oper: {}".format(service_oper_path))
                    th.delete(service_oper_path)
            th.apply()
            cleanup_log.append("\n Removed service oper")

    def _remove_service_side_effects(self, side_effects, cleanup_log, uinfo):
        """Remove all service side-effect queue"""
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            for side_effect_path in side_effects:
                kp = "/ncs:side-effect-queue/side-effect{{{}}}".format(side_effect_path)
                if th.exists(kp):
                    self.log.info("removing side-effect queue: {}".format(kp))
                    cleanup_log.append("\n Removing side-effect queue: {}".format(kp))
                    th.delete(kp)
            th.apply()
            cleanup_log.append("\n Removed side-effects")

    def _remove_kickers(self, kickers, cleanup_log, uinfo):
        """Remove all service kickers"""
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            for kicker_id in kickers:
                kp = "/kickers/data-kicker{{\"{}\"}}".format(kicker_id)
                if th.exists(kp):
                    self.log.info("removing kickers: {}".format(kp))
                    cleanup_log.append("\n Removing kicker: {}".format(kp))
                    th.delete(kp)
            th.apply()
            cleanup_log.append("\n Removed kickers")

    def _remove_kickers_with_device(self, kickers, device, cleanup_log, uinfo):
        """Remove all service kickers with given device"""
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            for kicker_id in kickers:
                if device in kicker_id:
                    kp = "/kickers/data-kicker{{\"{}\"}}".format(kicker_id)
                    if th.exists(kp):
                        self.log.info("removing kickers: {}".format(kp))
                        cleanup_log.append("\n Removing kicker: {}".format(kp))
                        th.delete(kp)
            th.apply()
            cleanup_log.append("\n Removed kickers")

    def _get_all_side_effect_for_service(self, service_path):
        side_effects = []
        with ncs.maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as trans:
            qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                        "/ncs:side-effect-queue/side-effect[service=\""
                                        + service_path + "\"]", '/', 0, 1,
                                        _ncs.QUERY_STRING, ["id"], [])

            res = _ncs.maapi.query_result(trans.maapi.msock, qh)

            for r in res:
                side_effects.append(r[0])

            _ncs.maapi.query_stop(trans.maapi.msock, qh)
        return side_effects

    def _get_kicker_with_service(self, service_path):
        kickers = []
        with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
            qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                        "/kickers/data-kicker/variable[value=\"" +
                                        service_path + "\"]", '/', 0, 1,
                                        _ncs.QUERY_STRING, ["../id"], [])

            res = _ncs.maapi.query_result(trans.maapi.msock, qh)

            for r in res:
                kickers.append(r[0])

            _ncs.maapi.query_stop(trans.maapi.msock, qh)
        return kickers


class TMTCPurgeOper(ncs.dp.Action):
    """
    Action handler for internal service oper data purge
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        self.log.info("Running internal tm-tc oper purge for: {}".format(kp))
        with ncs.maapi.single_write_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            th.safe_delete(str(kp))
            th.apply()
            self.log.info("Deleted: {}".format(kp))


class TMTCStatusAction(ncs.dp.Action):
    """
    Action handler to retrieve tm-tc status
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        service = input.service
        device = input.device
        status = True

        #defaults
        output.success = False
        output.detail = ""
        output.internal_plan_error = ""
        output.ready = "not-reached"
        output.queue_items = ""

        try:
            self._check_if_empty("service", service)
            self._check_if_empty("device", device)

            with ncs.maapi.single_write_trans("", "system", db=ncs.OPERATIONAL) as th:
                root = ncs.maagic.get_root(th)

                new_path = root.cisco_tm_tc_fp_internal__tm_tc_internal.tm_tc_plan[service, device]
                internal_plan = new_path.plan
                if internal_plan.failed:
                    self.log.info("Internal plan for {} {} is failed".format(service, device))
                    output.internal_plan_error = internal_plan.error_info.message
                    output.success = True
                    output.ready = "failed"
                    return
                is_cq_exists = False
                if internal_plan.commit_queue:
                    if len(internal_plan.commit_queue.queue_item) > 0:
                        is_cq_exists = True
                
                if is_cq_exists:
                    id = "[ "
                    for item in internal_plan.commit_queue.queue_item:
                        id += str(item.id) + " "
                    output.queue_items = id + "]"
                    output.success = True
                    output.ready = "not-reached"
                    return
        
        except Exception as ex:
            self.log.error("Get status Action for service {} & device {} failed: {}".format(service, device,
                                                                        traceback.print_exc()))
            status = False
            output.detail = "ERROR: {}".format(ex)
        
        output.success = status
        output.ready = "reached"

    def _check_if_empty(self, name, content):
        if len(content) == 0:
            raise Exception(name+ " can not be empty")
            
