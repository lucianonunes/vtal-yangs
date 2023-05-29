import ncs
import _ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
from traceback import print_exc

from cisco_tsdn_core_fp_common.flat_L3vpn import FlatL3vpn
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L3vpn_cfp_base_exception import (
    CustomActionException
)
from core_fp_common.common_utils import get_local_user
from core_fp_common.cleanup_utils import remove_zombie, invoke_backtrack_actions, delete_service
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from core_fp_common import cleanup_utils as CleanupUtils
from cisco_tsdn_core_fp_common.utils import (
    set_service_cleanup_flag, delete_service_cleanup_flag,
    check_service_cleanup_flag, get_action_timeout, get_all_rfs_nodes
)


class FlatL3vpnSelfTest(Action):
    """
    Action handler for self-test
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running self test: {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system") as th:
            root = maagic.get_root(th)

            # Get service node from action path
            action = maagic.get_node(th, kp)
            service = maagic.cd(action, "..")

            final_status = None
            final_message = None
            endpoint_list = []
            try:
                if len(service.endpoint) == 1:
                    final_status = "success"
                    final_message = "Only one service endpoint, nothing to test."
                else:
                    # Build endpoint list for testing
                    for endpoint in service.endpoint:
                        # Get device ip
                        pe_ip_addr = None
                        if endpoint.pe_ip_addr:
                            pe_ip_addr = Utils.getIpAddress(endpoint.pe_ip_addr)
                        pe_ipv6_addr = endpoint.pe_ipv6_addr
                        # Get route targets
                        route_targets = self.get_route_targets(endpoint)
                        # Get endpoint config status
                        ep_config_status = str(
                            root.cisco_flat_L3vpn_fp__flat_L3vpn_plan[service.name]
                            .plan.component["cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
                                            endpoint.endpoint_name].state["ncs:ready"].status)
                        # Add entry to list
                        endpoint_list.append(
                            (
                                endpoint.access_pe,  # [0]
                                pe_ip_addr,  # [1]
                                pe_ipv6_addr,  # [2]
                                route_targets,  # [3]
                                ep_config_status,  # [4]
                                endpoint.endpoint_name,  # [5]
                            )
                        )

                    self.log.info(f"L3vpn endpoints list {endpoint_list}")
                    # Generate test combinations
                    self_test_list = self.self_test_combinations(endpoint_list)
                    self.log.info(f"L3vpn self-test endpoints matrix {self_test_list}")

                    status_list = []

                    # Run tests
                    for test in self_test_list:
                        # Define vars for readability
                        # src
                        src_ipv4, src_ipv6 = test[0][1], test[0][2]
                        src_device = test[0][0]
                        src_config_status = test[0][4]
                        endpoint_name = test[0][5]
                        # dst
                        dst_ipv4, dst_ipv6 = test[1][1], test[1][2]
                        dst_device = test[1][0]
                        dst_config_status = test[1][4]
                        # Make sure src and dst endpoints are configured
                        self.log.info(f"L3vpn test check for: {test}")
                        if src_config_status == "failed":
                            status_list.append((src_device, dst_device, "failed",
                                                src_device + " not configured."))
                        elif dst_config_status == "failed":
                            status_list.append((src_device, dst_device,
                                                "failed", dst_device + " not configured."))
                        else:
                            # Both endpoints should have ipv4 or ipv6
                            # IPV4
                            if src_ipv4 is not None and dst_ipv4 is not None:
                                result = self.run_internal_self_test(root, service.name,
                                                                     endpoint_name, src_device,
                                                                     src_ipv4, dst_ipv4)
                                status_list.append((src_device, dst_device, *result))
                            # IPV6
                            elif src_ipv6 is not None and dst_ipv6 is not None:
                                result = self.run_internal_self_test(root, service.name,
                                                                     endpoint_name, src_device,
                                                                     src_ipv6, dst_ipv6)
                                status_list.append((src_device, dst_device, *result))
                            # IP PROTOCOL VERSION DOES NOT MATCH FOR THESE ENDPOINTS
                            else:
                                status_list.append((src_device, dst_device, "failed",
                                                    "IP PROTOCOL VERSION does not \
                                                    match for these endpoints"))

                    self.log.info(f"All L3vpn test results: {status_list}")

                    failed_results = [result for result in status_list if result[2] == "failed"]
                    self.log.info(f"Failed L3vpn test results: {failed_results}")

                    if len(failed_results) == 0:
                        final_status = "success"
                        final_message = str(status_list)
                    else:
                        final_status = "failed"
                        final_message = str(failed_results)

                    if final_status != "success" and final_status != "failed":
                        raise CustomActionException(
                            self.log, StatusCodes.SELF_TEST_STATUS_ERROR
                        ).set_context(
                            "Self Test Error", "Unsupported status returned"
                        ).add_state(
                            "Status", final_status
                        ).add_state(
                            "Service Name", service.name
                        ).add_state(
                            "Keypath", str(kp)
                        ).finish()

            except CustomActionException:
                raise
            except Exception as e:
                print_exc()
                exp = CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e)).\
                    set_context("Self Test Error", "Self Test Failed").\
                    finish()

                final_status = "failed"
                final_message = "Error: " + str(exp)

            output.status = final_status
            output.message = final_message

    def run_internal_self_test(self, root, service_name,
                               endpoint_name, device, src_ip, dst_ip):
        # First get corresponding internal self-test action
        if Utils.is_lsa:
            # Get RFS Node to device mapping
            rfs_node = LsaUtils.get_remote_nso(device)
            internal_action = (
                root.ncs__devices.device[rfs_node]
                .config.cisco_flat_L3vpn_fp_internal__flat_L3vpn[service_name, endpoint_name]
                .action.self_test
            )
        else:
            # Invoke self test action locally
            internal_action = root.cisco_flat_L3vpn_fp_internal__flat_L3vpn[
                service_name, endpoint_name].action.self_test
        # Build input
        input = internal_action.get_input()
        input.src_ip = src_ip
        input.dst_ip = dst_ip
        # Run test and return output
        output = internal_action(input)
        return (output.status, output.message)

    def self_test_combinations(self, endpoints):
        self_test_list = []
        if len(endpoints) % 2 == 1:
            endpoints = endpoints + [None]
        endpoints_size = len(endpoints)
        map = list(range(endpoints_size))
        mid = endpoints_size // 2
        for i in range(endpoints_size - 1):
            epl1 = map[:mid]
            epl2 = map[mid:]
            epl2.reverse()
            for j in range(mid):
                ep1 = endpoints[epl1[j]]
                ep2 = endpoints[epl2[j]]

                if ep1 is None or ep2 is None:
                    continue

                # Define vars for readability
                # Get ip
                ep1_ipv4, ep1_ipv6 = ep1[1], ep1[2]
                ep2_ipv4, ep2_ipv6 = ep2[1], ep2[2]
                # Get route targets
                ep1_rt = ep1[3]
                ep2_rt = ep2[3]

                # Both endpoints have ipv4
                if ep2_ipv4 is not None and ep1_ipv4 is not None:
                    # ipv4 import of ep2 should be in ipv4 export of ep1
                    if len([e for e in ep2_rt[0] if e in ep1_rt[1]]) > 0:
                        self_test_list.append((ep2, ep1))
                    # ipv4 import of ep2 should be in ipv4 export of ep1
                    if len([e for e in ep1_rt[0] if e in ep2_rt[1]]) > 0:
                        self_test_list.append((ep1, ep2))
                # Both endpoints have ipv6
                if ep2_ipv6 is not None and ep1_ipv6 is not None:
                    # ipv6 import of ep2 should be in ipv6 export of ep1
                    if len([e for e in ep2_rt[2] if e in ep1_rt[3]]) > 0:
                        self_test_list.append((ep2, ep1))
                    # ipv6 import of ep2 should be in ipv6 export of ep1
                    if len([e for e in ep1_rt[2] if e in ep2_rt[3]]) > 0:
                        self_test_list.append((ep1, ep2))

            map = map[mid:-1] + map[:mid] + map[-1:]
        return self_test_list

    def get_route_targets(self, endpoint):
        ipv4_import_targets = []
        ipv4_export_targets = []
        ipv6_import_targets = []
        ipv6_export_targets = []

        for af in endpoint.vrf.address_family:
            if af.address_family == "ipv4":
                for target in af.vpn_target:
                    rt_type = target.rt_type
                    rt_value = target.rt_value
                    if rt_type == "both":
                        ipv4_import_targets.append(rt_value)
                        ipv4_export_targets.append(rt_value)
                    elif rt_type == "import":
                        ipv4_import_targets.append(rt_value)
                    elif rt_type == "export":
                        ipv4_export_targets.append(rt_value)
            if af.address_family == "ipv6":
                for target in af.vpn_target:
                    rt_type = target.rt_type
                    rt_value = target.rt_value
                    if rt_type == "both":
                        ipv6_import_targets.append(rt_value)
                        ipv6_export_targets.append(rt_value)
                    elif rt_type == "import":
                        ipv6_import_targets.append(rt_value)
                    elif rt_type == "export":
                        ipv6_export_targets.append(rt_value)

        return (ipv4_import_targets, ipv4_export_targets,
                ipv6_import_targets, ipv6_export_targets)


class FlatL3vpnUpdateInternalCfpConfigurations(ncs.dp.Action):
    """
    Action handler for updating internal cfp config (for both non-LSA and LSA deployments)
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        # TODO : Add check to ensure that only RFS nodes with TSDN will be configured
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        username = get_local_user()
        with maapi.single_write_trans(username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            template = ncs.template.Template(root.cisco_flat_L3vpn_fp__cfp_configurations)
            # Check for LSA or non-LSA deployment
            if Utils.is_lsa:
                # Populate template vars with all RFS nodes
                template_vars = ncs.template.Variables()
                for rfs_node in get_all_rfs_nodes(self):
                    template_vars.add("RFS_NODE", rfs_node)
                    template.apply("cisco-flat-L3vpn-copy-cfp-configurations", template_vars)
            else:
                template.apply("cisco-flat-L3vpn-copy-cfp-configurations")
            th.apply()
            self.log.info("Updated internal L3VPN CFP configurations")


class InternalL3vpnPlanChangeHandler(ncs.dp.Action):
    """
    Action handler for L3vpn internal plan change
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Internal plan kicker changed for: {input.kicker_id} "
                      f"{input.path} {input.tid}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        l3vpn_wrapper = FlatL3vpn(input.path, self.log)

        # If cleanup is in progress, do not take any action
        if check_service_cleanup_flag(self.log, l3vpn_wrapper.service_kp, uinfo.username):
            return

        try:
            l3vpn_wrapper.redeploy()
        except Exception:
            print_exc()

        self.log.info(f"Internal plan change handled for: {input.path}")


class FlatL3vpnCleanupAction(ncs.dp.Action):
    """
    Action handler for flat l3vpn services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        cleanup_log = []
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        try:
            with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
                self.log.info(f"Cleanup Action for service name={name} service={input.service} "
                              f"endpoint={input.endpoint} no-networking={input.no_networking}")
                root = maagic.get_root(th)
                cleanup_log.append(f"Cleaning up L3vpn service: {input.service}")
                is_no_networking = input.no_networking
                service = input.service
                service_kp = f"/cisco-flat-L3vpn-fp:flat-L3vpn{{{service}}}"
                endpoint = input.endpoint
                self._cleanup_l3vpn_service(uinfo, service, endpoint,
                                            is_no_networking, cleanup_log, root, th)

                # return the log
                self.log.info("Cleanup Successful")
                cleanup_log.append("\n Cleanup Successful \n")
                output.success = True
                output.detail = "".join(cleanup_log)
        except Exception as ex:
            print_exc()
            exp = (
                CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(ex))
                .set_context("Cleanup Action", "Service cleanup failed")
                .finish()
            )
            self.log.error(f"Cleanup Failed : {exp}")
            cleanup_log.append("\n Cleanup Failed \n")
            cleanup_log.append(str(exp))
            output.success = False
            output.detail = "".join(cleanup_log)
        finally:
            delete_service_cleanup_flag(self, service_kp, uinfo.username)

    def _cleanup_l3vpn_service(self, uinfo, service, endpoint,
                               is_no_networking, cleanup_log, root, th):
        # We need to call is_lsa_setup() here to set is_lsa global varible as this is being
        # used in get_service_xpath which is a static method refers is_lsa
        # to return right zombie path.
        FlatL3vpn.is_lsa_setup()
        force_back_track_comp = []
        remove_plan_paths = []

        l3vpn_plan_path = FlatL3vpn.get_plan_kp(service)

        zombie_service_path = FlatL3vpn.get_service_xpath(service)

        service_path = FlatL3vpn.get_service_kp(service)
        if th.exists(service_path):
            try:
                endpoint_node = ncs.maagic.get_node(th, service_path + "/endpoint")
            except KeyError:
                endpoint_node = None
            # Check if there is only one device in service ,same as the one requested,
            # cleanup should be on the entire service.
            if endpoint_node and len(endpoint_node) <= 1:
                # If the requested device is not in current service path,
                # cleanup only the requested device
                if (len(endpoint_node) == 1 and endpoint is not None
                        and endpoint not in endpoint_node):
                    self.log.info(f"Clean up endpoint not present in service: {endpoint}")
                else:
                    endpoint = None

        # Collect service data to be cleaned up
        device_names = {}
        endpoints = []
        # If device input is not defined and plan exists, service wide cleanup
        if th.exists(l3vpn_plan_path) and (endpoint is None):
            l3vpn_components = root.cisco_flat_L3vpn_fp__flat_L3vpn_plan[
                service
            ].plan.component
            for component in l3vpn_components:
                if component.type == "cisco-flat-L3vpn-fp-nano-plan-services:endpoint":
                    endpoints.append(component.name)
                    device_name = component.private.property_list.property["ACCESS_PE"].value
                    # Collect devices from plan
                    device_names[component.name] = device_name
                    if Utils.is_lsa:
                        remove_plan_paths.append(
                            f"/cisco-tsdn-core-fp-common:rfs-flat-L3vpn-plan{{{service} "
                            f"{component.name}}}"
                        )
                force_back_track_comp.append((component.force_back_track, zombie_service_path))
            # Collect plan path
            remove_plan_paths.append(l3vpn_plan_path)
        # If endpoint is given as cleanup input
        elif endpoint is not None:
            if th.exists(l3vpn_plan_path):
                endpoint_component = root.\
                    cisco_flat_L3vpn_fp__flat_L3vpn_plan[service].plan.component
                if ("cisco-flat-L3vpn-fp-nano-plan-services:endpoint", endpoint) \
                        in endpoint_component:
                    endpoint_comp = endpoint_component["cisco-flat-L3vpn-fp-nano-plan-services"
                                                       ":endpoint", endpoint]
                    force_back_track_comp.append(
                        (endpoint_comp.force_back_track, zombie_service_path))

                    device_name = endpoint_comp.\
                        private.property_list.property["ACCESS_PE"].value
                    # Collect device name from plan
                    device_names[endpoint] = device_name
                    endpoints.append(endpoint)
                    remove_plan_paths.append(endpoint_comp._path)
                    if Utils.is_lsa:
                        remove_plan_paths.append("/cisco-tsdn-core-fp-common:"
                                                 f"rfs-flat-L3vpn-plan{{{service} {endpoint}}}")

        self._cleanup_service_data(uinfo, service, endpoint, is_no_networking, cleanup_log, root,
                                   th, force_back_track_comp, zombie_service_path, service_path,
                                   "endpoint", device_names, endpoints, remove_plan_paths)

    def _cleanup_service_data(self, uinfo, service, endpoint, is_no_networking, cleanup_log, root,
                              th, force_back_track_comp, zombie_service_path, service_path,
                              device_list_name_in_service, internal_devices,
                              endpoints, remove_plan_paths):

        self.log.info("L3VPN service cleanup data: "
                      f"external_plan_force_back_track_comp:{force_back_track_comp}, "
                      f"service_path:{service_path}, zombie_service_path:{zombie_service_path} ")

        commit_params = ncs.maapi.CommitParams()
        if is_no_networking:
            commit_params.no_networking()
            if Utils.is_lsa:
                commit_params.no_lsa()
        # If endpoint, delete endpoint if exists
        if endpoint:
            service_endpoint_kp = f"{service_path}/{device_list_name_in_service}{{{endpoint}}}"
            delete_service(self, service_endpoint_kp, cleanup_log, uinfo, commit_params)
        # Otherwise, delete service if exists
        else:
            set_service_cleanup_flag(self, service_path, uinfo.username)
            delete_service(self, service_path, cleanup_log, uinfo, commit_params)

        # Invoke internal cleanup action
        if len(internal_devices) > 0:
            # Check if internal service is local or on RFS node
            if Utils.is_lsa:
                # Get RFS Node to device mapping
                rfs_node_dev_dict = LsaUtils.get_device_remote_nso(
                    list(set(internal_devices.values())))
                self.log.info(f"rfs_node_dev_dict: {rfs_node_dev_dict}")
                # Invoke internal cleanup action on all RFS nodes with corresponding devices
                for rfs_node, rfs_node_devices in rfs_node_dev_dict.items():
                    endpoint_list = Utils.get_keys_by_values(internal_devices, rfs_node_devices)
                    internal_action = root.ncs__devices.device[rfs_node].config.\
                        cisco_flat_L3vpn_fp_internal__flat_L3vpn_actions.cleanup

                    self._call_internal_cleanup_action(internal_action, service, endpoint_list,
                                                       is_no_networking, cleanup_log)
            else:
                # Invoke internal cleanup action locally
                internal_action = (root.cisco_flat_L3vpn_fp_internal__flat_L3vpn_actions.cleanup)
                self._call_internal_cleanup_action(internal_action, service, endpoints,
                                                   is_no_networking, cleanup_log)

        # In LSA, CFS cleanup messages will be sent to RFS, when no-networking is true
        # To avoid messages being sent to RFS, no-lsa should also be true
        if Utils.is_lsa:
            is_no_networking = True
            no_lsa = True
        else:
            no_lsa = False

        # force-backtrack all upper service components
        invoke_backtrack_actions(self, service, force_back_track_comp,
                                 is_no_networking, no_lsa)

        cleanup_log.append("\n Removed all plan components")

        # remove zombies for upper service
        if endpoint is None:
            remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        self.log.info(f"remove_plan_paths list {remove_plan_paths}")
        # Remove leftover plan paths
        for plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, plan_path, cleanup_log, uinfo)

        # This is to cover the case where cleanup is requested without service delete.
        # In this case we will see race conditions where zombie is recreated
        # after we pass the zombie deletion check during removal of config-apply callbacks
        if endpoint is None:
            remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        # Due to zombie removal race conditions
        # plan elements are partially recreated causing cleanup leftovers.
        # This does a final check on plan path removal.
        for plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, plan_path, cleanup_log, uinfo)

        # If the service is still present, we should re-deploy the service to reconcile
        # as the out of band deletion of data can leave the northbound plan in weird state.
        if endpoint is not None and th.exists(service_path):
            try:
                external_service = ncs.maagic.get_node(th, service_path)
                external_service.reactive_re_deploy()
            except KeyError:
                pass

    def _call_internal_cleanup_action(self, internal_action, service,
                                      endpoints, is_no_networking, cleanup_log):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        internal_action_input.endpoints = endpoints
        internal_action_input.no_networking = is_no_networking
        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        # If success proceed with northbound service cleanup
        if internal_action_output.success:
            cleanup_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            cleanup_log.append(internal_action_output.detail)
            raise Exception("Internal Service Cleanup Failed")


class FlatL3vpnRecoveryAction(ncs.dp.Action):
    """
    Action handler for L3VPN services recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"Recovery Action for service={input.service} endpoint={input.endpoint}")
            root = maagic.get_root(th)
            recovery_log = []
            recovery_log.append(f"Recovering L3VPN service: {input.service}")
            service = input.service
            endpoint = input.endpoint
            sync_direction = input.sync_direction

            try:
                self._recover_l3vpn_service(service, endpoint, root,
                                            th, recovery_log, sync_direction)
            except Exception as e:
                exp = (
                    CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(e))
                    .set_context("Recovery Action", "Recovery failed for L3VPN")
                    .add_state("L3VPN Service Name", service)
                    .add_state("Endpoint", endpoint)
                    .finish()
                )
                self.log.error(f"Recovery Failed : {exp}")
                recovery_log.append("\nRecovery Failed\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            else:
                self.log.info("Recovery Complete")
                recovery_log.append("\nRecovery Complete\n")
                output.success = True
                output.detail = "".join(recovery_log)

    def _recover_l3vpn_service(self, service, endpoint, root,
                               th, recovery_log, sync_direction):

        l3vpn_plan_path = FlatL3vpn.get_plan_kp(service)

        endpoint_comp_type = "cisco-flat-L3vpn-fp-nano-plan-services:endpoint"

        if not th.exists(l3vpn_plan_path):
            raise Exception(f"No such service found for recovery: {service}")

        l3vpn_plan = maagic.get_node(th, l3vpn_plan_path).plan
        # To Find failed endpoints
        query_path = ("cisco-flat-L3vpn-fp:flat-L3vpn-plan[name='"
                      + service + "']/plan/component" + "[type='"
                      + endpoint_comp_type + "']/" + "state[status='failed']")

        self.recover_service(root, th, service, endpoint, l3vpn_plan,
                             endpoint_comp_type, query_path,
                             recovery_log, sync_direction)

    def recover_service(self, root, th, service, endpoint, service_plan,
                        endpoint_comp_type, query_path, recovery_log, sync_direction):
        if endpoint is not None:
            # Check if such a failed endpoint exists in plan:
            if not (endpoint_comp_type, endpoint) in service_plan.component:
                raise Exception(f"No such failed endpoint: {endpoint} "
                                f"found for recovery in service: {service}")

            endpoint_comp = service_plan.component[(endpoint_comp_type, endpoint)]
            if (endpoint_comp.state["ncs:ready"].status != "failed"
                    and endpoint_comp.state["ncs:init"].status != "failed"):
                raise Exception(f"Endpoint: {endpoint} is not in failed "
                                f"state in service: {service}")
            # Check if internal service is local or on RFS node
            if Utils.is_lsa:
                failed_device = (
                    root.cisco_flat_L3vpn_fp__flat_L3vpn_plan[service]
                    .plan.component["cisco-flat-L3vpn-fp-nano-plan-services:endpoint",
                                    endpoint].private.property_list.property["ACCESS_PE"]
                    .value)
                rfs_node = LsaUtils.get_remote_nso(failed_device)
                internal_action = root.\
                    ncs__devices.device[rfs_node].config.\
                    cisco_flat_L3vpn_fp_internal__flat_L3vpn_actions.error_recovery

                self._call_internal_recovery_action(internal_action, service, [endpoint],
                                                    recovery_log, sync_direction)
            else:
                # Invoke internal recovery action locally
                internal_action = root.\
                    cisco_flat_L3vpn_fp_internal__flat_L3vpn_actions.error_recovery

                self._call_internal_recovery_action(internal_action, service, [endpoint],
                                                    recovery_log, sync_direction)
        else:
            # Entire service recovery
            failed_endpoints = self.get_failed_endpoints(th, query_path)

            self.log.info(f"Failed endpoints to recover: {failed_endpoints} on service: {service}")

            if len(failed_endpoints) == 0:
                raise Exception(f"No failed endpoints to recover in service: {service}")

            if Utils.is_lsa:
                # Get all corresponding devices for failed_endpoints
                failed_devices = {}
                for endpoint in failed_endpoints:
                    failed_devices[endpoint] = (
                        root.cisco_flat_L3vpn_fp__flat_L3vpn_plan[service]
                        .plan.component[
                            "cisco-flat-L3vpn-fp-nano-plan-services:endpoint", endpoint
                        ].private.property_list.property["ACCESS_PE"]
                        .value
                    )
                # Get RFS Node to device mapping
                rfs_node_dev_dict = LsaUtils.get_device_remote_nso(
                    list(set(failed_devices.values())))
                # Invoke internal recovery action on all RFS nodes with corresponding devices
                for rfs_node, rfs_node_devices in rfs_node_dev_dict.items():
                    endpoint_list = Utils.get_keys_by_values(failed_devices, rfs_node_devices)
                    internal_action = root.\
                        ncs__devices.device[rfs_node].config.\
                        cisco_flat_L3vpn_fp_internal__flat_L3vpn_actions.error_recovery

                    self._call_internal_recovery_action(internal_action, service,
                                                        endpoint_list, recovery_log, sync_direction)
            else:
                # Invoke internal recovery action locally
                internal_action = root.\
                    cisco_flat_L3vpn_fp_internal__flat_L3vpn_actions.error_recovery

                self._call_internal_recovery_action(internal_action, service,
                                                    failed_endpoints, recovery_log, sync_direction)

    def _call_internal_recovery_action(self, internal_action, service, endpoints,
                                       recovery_log, sync_direction):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        internal_action_input.endpoints = endpoints
        internal_action_input.sync_direction = sync_direction
        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        if internal_action_output.success:
            recovery_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            recovery_log.append(internal_action_output.detail)
            raise Exception("Service Recovery Failed")

    def get_failed_endpoints(self, trans, query_path):
        failed_endpoints = []
        qh = _ncs.maapi.query_start(trans.maapi.msock,
                                    trans.th, query_path, "/", 0, 1,
                                    _ncs.QUERY_STRING, ["../name"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)
        for r in res:
            failed_endpoints.append(r[0])
        _ncs.maapi.query_stop(trans.maapi.msock, qh)

        return failed_endpoints


class FlatL3vpnServiceRecoveryAction(ncs.dp.Action):
    """
    Action handler for L3VPN service recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"L3VPN Service Recovery Action for : {kp}")
            # Call general recovery action
            root = maagic.get_root(th)
            service = maagic.get_node(th, kp)

            recovery_action = root.cisco_flat_L3vpn_fp__flat_L3vpn_actions.error_recovery
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service.name
            recovery_action_input.sync_direction = input.sync_direction
            recovery_action_output = recovery_action(recovery_action_input)

            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail


class FlatL3vpnEndpointRecoveryAction(ncs.dp.Action):
    """
    Action handler for L3VPN service recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            self.log.info(f"L3VPN Endpoint Recovery Action for : {kp}")
            # Call general recovery action
            root = maagic.get_root(th)
            endpoint = maagic.get_node(th, kp)
            service = ncs.maagic.cd(endpoint, "../")

            recovery_action = root.cisco_flat_L3vpn_fp__flat_L3vpn_actions.error_recovery
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service.name
            recovery_action_input.endpoint = endpoint.endpoint_name
            recovery_action_input.sync_direction = input.sync_direction
            recovery_action_output = recovery_action(recovery_action_input)

            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail
