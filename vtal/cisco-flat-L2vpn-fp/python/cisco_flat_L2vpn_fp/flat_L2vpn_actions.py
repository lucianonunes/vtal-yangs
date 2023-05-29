import ncs
import _ncs
from ncs.dp import Action
import ncs.maapi as maapi
import ncs.maagic as maagic
import traceback
from . import utils as Utils
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.flat_L2vpn_cfp_base_exception import (
    CustomActionException
)
from cisco_tsdn_core_fp_common.utils import (
    set_service_cleanup_flag, delete_service_cleanup_flag,
    check_service_cleanup_flag, get_action_timeout, get_all_rfs_nodes
)
from cisco_tsdn_core_fp_common.flat_L2vpn import FlatL2vpn
from cisco_tsdn_core_fp_common import constants as const
from core_fp_common import cleanup_utils as CleanupUtils
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from core_fp_common.common_utils import get_local_user


class L2vpnInternalPlanChangeHandler(ncs.dp.Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info("L2vpnInternalPlanChangeHandler Internal "
                      f"plan kicker changed for: {input.kicker_id} {input.path} {input.tid}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))

        l2vpn_wrapper = FlatL2vpn(input.path, self.log)
        pe_name = l2vpn_wrapper.service_key[1]

        # If cleanup is in progress, do not take any action
        if check_service_cleanup_flag(self.log, l2vpn_wrapper.service_kp, uinfo.username):
            return

        self.log.info(f"L2vpnInternalPlanChangeHandler() name: {l2vpn_wrapper.service_name} ,"
                      f"pe_name: {pe_name} , local_site: {l2vpn_wrapper.local_site}")

        try:
            l2vpn_wrapper.redeploy()
        except Exception as e:
            self.log.error(f"Exception L2vpnInternalPlanChangeHandler: {e}")
            traceback.print_exc()

        self.log.info(f"Internal plan change handled for {input.path}")


class UpdateInternalCfpConfigurations(ncs.dp.Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"UpdateInternalCfpConfigurations() on CFS : {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_write_trans(get_local_user(), "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)
            template = ncs.template.Template(root.cisco_flat_L2vpn_fp__cfp_configurations)
            if Utils.is_lsa:
                # Get all RFS nodes
                create_vars = ncs.template.Variables()
                for rfs_node in get_all_rfs_nodes(self):
                    # TODO: How to avoid applying this on RFS node not belonging to TSDN?
                    create_vars.add("RFS_NODE", rfs_node)
                    template.apply("cisco-flat-L2vpn-fp-copy-cfp-configurations", create_vars)
            else:
                template.apply("cisco-flat-L2vpn-fp-copy-cfp-configurations")
            th.apply()
            self.log.info("Updated internal L2vpn CFP configurations")


class FlatL2vpnCFSSelfTest(ncs.dp.Action):
    """
    Action handler for self-test
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running Flat L2vpn Self Test on CFS : {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with ncs.maapi.single_read_trans(uinfo.username, "system") as th:
            root = ncs.maagic.get_root(th)
            # Get service node from action path
            action = ncs.maagic.get_node(th, kp)
            service = ncs.maagic.cd(action, "..")

            try:
                if service.service_type == "p2p":
                    local_site_pe = service.flat_L2vpn_p2p.local_site.pe
                    remote_site_pe = service.flat_L2vpn_p2p.remote_site.pe
                else:
                    local_site_pe = service.flat_L2vpn_evpn_vpws.local_site.pe
                    remote_site_pe = service.flat_L2vpn_evpn_vpws.remote_site.pe
                self.log.info(f"Flat L2vpn Self Test local_site_pe: {local_site_pe} ,"
                              f"remote_site_pe: {remote_site_pe}")
                if Utils.is_lsa:
                    # Get RFS Node to device mapping
                    action_prefix_local = root.ncs__devices.\
                        device[LsaUtils.get_remote_nso(local_site_pe)].config
                    action_prefix_remote = root.ncs__devices.\
                        device[LsaUtils.get_remote_nso(remote_site_pe)].config
                else:
                    # Invoke internal self-test action locally
                    action_prefix_local = root
                    action_prefix_remote = root

                internal_action_local = self.get_local_action(action_prefix_local,
                                                              service.name, local_site_pe)
                internal_action_remote = self.get_remote_action(action_prefix_remote,
                                                                service.name, remote_site_pe)

                (local_status, local_message) = self.\
                    _call_internal_selftest_action(internal_action_local)

                self.log.info(f"Local Site L2vpn test results: {local_status} {local_message}")

                (remote_status, remote_message) = self\
                    ._call_internal_selftest_action(internal_action_remote)

                self.log.info(f"Remote Site L2vpn test results: {remote_status} {remote_message}")

                final_status = "success"
                final_message = ("local-site: " + local_message +
                                 ". remote-site: " + remote_message)

                self.log.info(f"final_message: {final_message}")

                if local_status == "failed" or remote_status == "failed":
                    final_status = "failed"

                if final_status != "success" and final_status != "failed":
                    raise CustomActionException(
                        self.log, StatusCodes.SELF_TEST_STATUS_ERROR
                    ).set_context("Self Test", "Unsupported status returned").add_state(
                        "Status", final_status
                    ).add_state(
                        "Service Name", service.name
                    ).finish()

            except CustomActionException:
                raise
            except Exception as e:
                self.log.error(f"Exception FlatL2vpnCFSSelfTest: {e}")
                traceback.print_exc()
                exp = (CustomActionException(self.log, StatusCodes.SELF_TEST_ERROR, str(e))
                       .set_context("Self Test", "Self Test Failed")
                       .finish())
                final_status = "failed"
                final_message = "Error: " + str(exp)
            output.status = final_status
            output.message = final_message

    def _call_internal_selftest_action(self, internal_action):
        internal_action_input = internal_action.get_input()
        internal_action_output = internal_action(internal_action_input)

        return (internal_action_output.status, internal_action_output.message)

    def get_local_action(self, action_prefix_local, name, local_site_pe):
        return (action_prefix_local.
                cisco_flat_L2vpn_fp_internal_local_site__flat_L2vpn_internal_local_site_service[
                    name, local_site_pe].action.self_test)

    def get_remote_action(self, action_prefix_remote, name, remote_site_pe):
        return (action_prefix_remote.
                cisco_flat_L2vpn_fp_internal_remote_site__flat_L2vpn_internal_remote_site_service[
                    name, remote_site_pe].action.self_test)


class FlatL2vpnCleanupAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services cleanup
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        cleanup_log = []
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        try:
            with maapi.single_read_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
                service = input.service
                site_name = input.site
                is_no_networking = input.no_networking

                self.log.info(f"Cleanup Action for L2vpn service name={name}  "
                              f"service={service} "
                              f"site={site_name} "
                              f"no-networking={is_no_networking}")

                root = maagic.get_root(th)
                cleanup_log.append(f"\nCleaning up L2vpn service: {service} \n")
                service_kp = f"/cisco-flat-L2vpn-fp:flat-L2vpn{{{service}}}"

                self._cleanup_l2vpn_service(uinfo, service, site_name, is_no_networking,
                                            cleanup_log, root, th, output,)

                # return the log
                self.log.info("Cleanup Successful")
                cleanup_log.append("\n Cleanup Successful")
                output.success = True

        except Exception as ex:
            self.log.error(f"Exception in FlatL2vpnCleanupAction() : {ex}")
            traceback.print_exc()
            exp = (CustomActionException(self.log, StatusCodes.CLEANUP_ERROR, str(ex))
                   .set_context("Cleanup Action", "Service cleanup failed")
                   .finish())
            cleanup_log.append(f"\nERROR: {exp}")
            output.success = False
        finally:
            output.detail = "".join(cleanup_log)
            delete_service_cleanup_flag(self, service_kp, uinfo.username)

    def _cleanup_l2vpn_service(self, uinfo, service, site_name, is_no_networking,
                               cleanup_log, root, th, output):
        l2vpn_plan_path = f"/cisco-flat-L2vpn-fp:flat-L2vpn-plan{{{service}}}"
        zombie_service_path = f"/flat-L2vpn[name='{service}']"
        service_path = f"/cisco-flat-L2vpn-fp:flat-L2vpn{{{service}}}"

        force_back_track_comp = []
        local_sites = []
        remote_sites = []
        sites = []
        status_code_paths = []
        remove_plan_paths = []

        if th.exists(l2vpn_plan_path):
            commit_params = ncs.maapi.CommitParams()
            if is_no_networking:
                commit_params.no_networking()
                if Utils.is_lsa:
                    commit_params.no_lsa()

            local_comp_type = const.L2VPN_LOCAL_SITE_COMP_TYPE
            remote_comp_type = const.L2VPN_REMOTE_SITE_COMP_TYPE
            site_comp_type = const.L2VPN_SITE_COMP_TYPE

            local_comp_key = (local_comp_type, site_name)
            remote_comp_key = (remote_comp_type, site_name)
            site_comp_key = (site_comp_type, site_name)

            if Utils.is_lsa:
                local_lsa_plan_path = const.L2VPN_LSA_LOCAL_SITE_PLAN_PATH
                remote_lsa_plan_path = const.L2VPN_LSA_REMOTE_SITE_PLAN_PATH
                site_lsa_plan_path = const.L2VPN_LSA_SITE_PLAN_PATH
            else:
                local_lsa_plan_path = None
                remote_lsa_plan_path = None
                site_lsa_plan_path = None

            l2vpn_plan = maagic.get_node(th, l2vpn_plan_path)

            if site_name is not None:
                if l2vpn_plan.plan.component.exists(local_comp_key):
                    self._validate_local_site_cleanup(th, service_path, service,
                                                      site_name, cleanup_log)

                    local_lsa_plan_path = f"{local_lsa_plan_path}{{{service} {site_name}}}"

                    self._register_site_cleanup(l2vpn_plan,
                                                local_comp_key,
                                                local_sites,
                                                force_back_track_comp,
                                                zombie_service_path,
                                                remove_plan_paths,
                                                status_code_paths,
                                                local_lsa_plan_path)

                elif l2vpn_plan.plan.component.exists(remote_comp_key):
                    self._delete_remote_site(th, service_path, site_name,
                                             cleanup_log, uinfo, commit_params)

                    remote_lsa_plan_path = f"{remote_lsa_plan_path}{{{service} {site_name}}}"

                    self._register_site_cleanup(l2vpn_plan,
                                                remote_comp_key,
                                                remote_sites,
                                                force_back_track_comp,
                                                zombie_service_path,
                                                remove_plan_paths,
                                                status_code_paths,
                                                remote_lsa_plan_path)

                elif l2vpn_plan.plan.component.exists(site_comp_key):
                    self._delete_site(th, service_path, site_name,
                                      cleanup_log, uinfo, commit_params)

                    site_lsa_plan_path = f"{site_lsa_plan_path}{{{service} {site_name}}}"

                    self._register_site_cleanup(l2vpn_plan,
                                                site_comp_key,
                                                sites,
                                                force_back_track_comp,
                                                zombie_service_path,
                                                remove_plan_paths,
                                                status_code_paths,
                                                site_lsa_plan_path)

            else:
                # First add service xpath to cisco-tsdn-core-fp-common cleanup-in-progress-for list
                set_service_cleanup_flag(self, service_path, uinfo.username)

                # delete upper service if exists
                CleanupUtils.delete_service(self, service_path, cleanup_log, uinfo, commit_params)

                for component in l2vpn_plan.plan.component:
                    if (component.type == local_comp_type):
                        lsa_plan_path = f"{local_lsa_plan_path}{{{service} {component.name}}}"
                        self._register_site_cleanup_helper(local_sites, component,
                                                           remove_plan_paths, lsa_plan_path)
                    elif (component.type == remote_comp_type):
                        lsa_plan_path = f"{remote_lsa_plan_path}{{{service} {component.name}}}"
                        self._register_site_cleanup_helper(remote_sites, component,
                                                           remove_plan_paths, lsa_plan_path)
                    elif component.type == site_comp_type:
                        lsa_plan_path = f"{site_lsa_plan_path}{{{service} {component.name}}}"
                        self._register_site_cleanup_helper(sites, component,
                                                           remove_plan_paths, lsa_plan_path)

                    force_back_track_comp.append((component.force_back_track, zombie_service_path))
                remove_plan_paths.append(l2vpn_plan_path)

        self._cleanup_service_data(uinfo,
                                   service,
                                   site_name,
                                   local_sites,
                                   remote_sites,
                                   sites,
                                   is_no_networking,
                                   cleanup_log,
                                   root,
                                   th,
                                   force_back_track_comp,
                                   zombie_service_path,
                                   service_path,
                                   remove_plan_paths,
                                   status_code_paths,
                                   output)

    def _cleanup_service_data(self, uinfo, service, site_name, local_sites, remote_sites, sites,
                              is_no_networking, cleanup_log, root, th, force_back_track_comp,
                              zombie_service_path, service_path, remove_plan_paths,
                              status_code_paths, output):
        # call internal cleanup
        if len(remote_sites) > 0:
            self._call_internal_cleanup(root, service, "remote-site",
                                        remote_sites, is_no_networking, cleanup_log, output)
            self.log.debug("After L2VPN internal cleanup on remote-site ")

        if len(local_sites) > 0:
            self._call_internal_cleanup(root, service, "local-site",
                                        local_sites, is_no_networking, cleanup_log, output)
            self.log.debug("After L2VPN internal cleanup on local-site ")

        if len(sites) > 0:
            self._call_internal_cleanup(root, service, "site",
                                        sites, is_no_networking, cleanup_log, output)
            self.log.debug("After L2VPN internal cleanup on site ")

        self.log.info("L2VPN service cleanup data: "
                      f"external_plan_force_back_track_comp:{force_back_track_comp}, "
                      f"zombie_service_path:{zombie_service_path} ")

        # In LSA, CFS cleanup messages will be sent to RFS, when no-networking is true
        # To avoid messages being sent to RFS, no-lsa should also be true
        if Utils.is_lsa:
            is_no_networking = True
            no_lsa = True
        else:
            no_lsa = False

        # force-backtrack all upper service components
        CleanupUtils.invoke_backtrack_actions(self, service, force_back_track_comp,
                                              is_no_networking, no_lsa)

        cleanup_log.append("\n Removed all external plan components")

        # remove zombies for upper service
        if site_name is None:
            CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        # remove leftover plan path & zombies for upper service again
        # This is to cover the case where cleanup is requested without service delete.
        # In this case we will see race conditions where zombie is recreated
        # after we pass the zombie deletion check during removal of config-apply callbacks
        # Remove leftover plan paths
        for plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, plan_path, cleanup_log, uinfo)
        if site_name is None:
            CleanupUtils.remove_zombie(self, zombie_service_path, cleanup_log, uinfo)

        # Due to zombie removal race conditions
        # plan elements are partially recreated causing cleanup leftovers.
        # This does a final check on plan path removal.
        for plan_path in remove_plan_paths:
            CleanupUtils.remove_plan_paths(self, plan_path, cleanup_log, uinfo)

        CleanupUtils.cleanup_status_codes(self, status_code_paths, cleanup_log)

        # If the service is still present, we should re-deploy the service to reconcile
        # as the out of band deletion of data can leave the northbound plan in weird state.
        if site_name is not None and th.exists(service_path):
            try:
                external_service = ncs.maagic.get_node(th, service_path)
                external_service.re_deploy()
            except KeyError:
                pass

    def _call_internal_cleanup(self, root, service, site_type,
                               sites, is_no_networking, cleanup_log, output):
        try:
            self.log.debug(f"_call_internal_cleanup() of {site_type} {sites} ")
            for [site_name, pe_name] in sites:
                # Check if internal service is local or on RFS node
                if Utils.is_lsa:
                    rfs_node = LsaUtils.get_remote_nso(pe_name)
                    internal_action = root.ncs__devices.device[rfs_node].config.\
                        cisco_flat_L2vpn_fp_internal_common__flat_L2vpn_internal_actions.cleanup
                else:
                    internal_action = root.\
                        cisco_flat_L2vpn_fp_internal_common__flat_L2vpn_internal_actions.cleanup

                self._call_internal_cleanup_action(internal_action, service, site_type,
                                                   site_name, is_no_networking, cleanup_log)

        except Exception as ex:
            self.log.error(f"Exception in _call_internal_cleanup() : {ex}")
            traceback.print_exc()
            output.success = False
            # cleanup_log will have exception msg (added by internal service)
            output.detail = "".join(cleanup_log)

    def _call_internal_cleanup_action(self, internal_action, service,
                                      site_type, pe_name, is_no_networking, cleanup_log):
        internal_action_input = internal_action.get_input()
        internal_action_input.service = service
        internal_action_input.no_networking = is_no_networking

        if site_type == "local-site":
            internal_action_input.local_site_only = pe_name
        elif site_type == "remote-site":
            internal_action_input.remote_site_only = pe_name
        elif site_type == "site":
            internal_action_input.site_only = pe_name

        internal_action_output = internal_action(internal_action_input)

        # Check if internal action output failed/success
        # If success proceed with northbound service cleanup
        if internal_action_output.success:
            cleanup_log.append(internal_action_output.detail)
        else:
            # If failed, return with error.
            cleanup_log.append(internal_action_output.detail)
            raise Exception(f"Internal Service Cleanup Failed for {site_type} {service}")

    def _validate_local_site_cleanup(self, th, service_path, service_name, site_name, cleanup_log):
        local_site = self._get_local_site(th, service_path)

        if local_site is None:
            err_msg = (f"Service Cleanup Failed for local-site of {service_name}. "
                       "L2VPN service has been deleted. Trigger cleanup action without "
                       "'site' option.")
            self._raise_cleanup_err(err_msg)

        if local_site.pe == site_name:
            err_msg = (f"Service Cleanup Failed for local-site of {service_name}."
                       " Local-site deletion must be triggered first before using cleanup action.")
            self._raise_cleanup_err(err_msg)

    def _get_local_site(self, th, service_path):
        if th.exists(service_path + "/flat-L2vpn-p2p/local-site"):
            return maagic.get_node(th, service_path + "/flat-L2vpn-p2p/local-site")
        elif th.exists(service_path + "/flat-L2vpn-evpn-vpws"):
            return maagic.get_node(th, service_path + "/flat-L2vpn-evpn-vpws/local-site")
        else:
            return None

    def _delete_remote_site(self, th, service_path, site_name, cleanup_log, uinfo, commit_params):
        def _delete(self, th, remote_path, site_name,
                    cleanup_log, uinfo, commit_params):
            if th.exists(remote_path):
                try:
                    remote_site = ncs.maagic.get_node(th, remote_path)
                    if remote_site.pe == site_name:
                        CleanupUtils.delete_service(self, remote_path,
                                                    cleanup_log, uinfo, commit_params)
                except KeyError:
                    pass

        # Remove remote-site from upper service if exists with given device
        _delete(self, th, f"{service_path}/flat-L2vpn-p2p/remote-site",
                site_name, cleanup_log, uinfo, commit_params)

        # Remove remote-site from upper service if exists with given device
        _delete(self, th, f"{service_path}/flat-L2vpn-evpn-vpws/remote-site",
                site_name, cleanup_log, uinfo, commit_params)

    def _delete_site(self, th, service_path, site_name, cleanup_log, uinfo, commit_params):
        path = f"{service_path}/flat-L2vpn-evpn-multipoint/site{{{site_name}}}"

        if th.exists(path):
            try:
                CleanupUtils.delete_service(self, path, cleanup_log, uinfo, commit_params)
            except KeyError:
                pass

    def _register_site_cleanup(self, l2vpn_plan, comp_key, cleanup_sites,
                               force_back_track_comp, zombie_service_path,
                               remove_plan_paths, status_code_paths, lsa_plan_path):
        site_comp = l2vpn_plan.plan.component[comp_key]

        force_back_track_comp.append((site_comp.force_back_track, zombie_service_path))
        remove_plan_paths.append(site_comp._path)

        self._register_site_cleanup_helper(cleanup_sites,
                                           site_comp,
                                           remove_plan_paths,
                                           lsa_plan_path)

        if l2vpn_plan.plan.status_code_detail.exists(comp_key):
            status_code_paths.append(
                l2vpn_plan.plan.status_code_detail[comp_key]._path)

    def _register_site_cleanup_helper(self, cleanup_sites, site_comp, remove_plan_paths,
                                      lsa_plan_path):
        if site_comp.type == const.L2VPN_SITE_COMP_TYPE:
            cleanup_sites.append([site_comp.name, site_comp.pe])
        else:
            cleanup_sites.append([site_comp.name, site_comp.name])
        if Utils.is_lsa:
            remove_plan_paths.append(lsa_plan_path)

    def _raise_cleanup_err(self, err_msg):
        self.log.error(err_msg)
        raise Exception(err_msg)


class FlatL2vpnSiteRecoveryAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services site error recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running Flat L2vpn Site Recovery action on CFS : {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            root = maagic.get_root(th)

            # EX: kp = /cisco-flat-L2vpn-fp:flat-L2vpn{L2NM-EVPN-MP-ELAN-internal} \
            #            /flat-L2vpn-evpn-multipoint/site{PIOSXR-0_a1}/action
            service = str(kp[-2][0])

            # Get site service node from action path
            action = ncs.maagic.get_node(th, kp)
            site_service = ncs.maagic.cd(action, "..")
            pe_name = site_service.pe
            self.log.info(f"Flat L2vpn Site Recovery for {service} {pe_name}")

            recovery_action = (root.cisco_flat_L2vpn_fp__flat_L2vpn_actions.error_recovery)
            recovery_action_input = recovery_action.get_input()
            recovery_action_input.service = service
            recovery_action_input.sync_direction = input.sync_direction

            site_type = str(kp[-4])
            if site_type == "local-site":
                recovery_action_input.local_site_only = pe_name
            elif site_type == "remote-site":
                recovery_action_input.remote_site_only = pe_name
            elif site_type == "site":
                recovery_action_input.site_only = site_service.site_name
            else:
                raise Exception(f"Invalid site-type = {site_type}")

            recovery_action_output = recovery_action(recovery_action_input)

            output.success = recovery_action_output.success
            output.detail = recovery_action_output.detail


class FlatL2vpnRecoveryAction(ncs.dp.Action):
    """
    Action handler for flat l2vpn services site error recovery
    """

    @Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Running Flat L2vpn Recovery action on CFS : {kp}")
        _ncs.dp.action_set_timeout(uinfo, get_action_timeout(self, uinfo.username))
        with ncs.maapi.single_read_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
            root = ncs.maagic.get_root(th)
            recovery_log = []
            recovery_log.append(f"Recovering L2vpn service: {input.service} "
                                f"with sync-direction: {input.sync_direction}")
            service = input.service

            remote_site_only = input.remote_site_only
            local_site_only = input.local_site_only
            sites_only = {input.site_only: None}

            sync_direction = input.sync_direction

            try:
                l2vpn_plan_path = f"/cisco-flat-L2vpn-fp:flat-L2vpn-plan{{{service}}}"
                if not th.exists(l2vpn_plan_path):
                    err_msg = f"No such service found for recovery: {service}"
                    recovery_log.append(err_msg)
                    raise Exception(err_msg)

                l2vpn_plan = maagic.get_node(th, l2vpn_plan_path)

                local_site_pe = None
                remote_site_pe = None

                return_status = [None, None, None]
                recovery_flag = [None, None, None]

                if local_site_only is not None:
                    local_site_pe = local_site_only
                elif remote_site_only is not None:
                    remote_site_pe = remote_site_only
                elif input.site_only is not None:
                    site_name = input.site_only
                    comp_key = ("cisco-flat-L2vpn-fp-nano-plan-services:site", site_name)

                    if l2vpn_plan.plan.component.exists(comp_key):
                        sites_only[site_name] = l2vpn_plan.plan.component[comp_key].pe
                    else:
                        err_msg = f"Site, {site_name}, does not exist in {l2vpn_plan._path}"
                        raise Exception(err_msg)
                else:
                    sites_only = {}

                    # Lookup site from plan as in case of delete-failures service wont existing
                    l2vpn_components = root.cisco_flat_L2vpn_fp__flat_L2vpn_plan[
                        service
                    ].plan.component

                    for component in l2vpn_components:
                        if component.type == "cisco-flat-L2vpn-fp-nano-plan-services:site":
                            sites_only[component.name] = component.pe
                        else:
                            if (component.type ==
                                    "cisco-flat-L2vpn-fp-nano-plan-services:local-site"):
                                local_site_pe = component.name
                            if (component.type ==
                                    "cisco-flat-L2vpn-fp-nano-plan-services:remote-site"):
                                remote_site_pe = component.name

                if local_site_pe is not None:
                    try:
                        return_status[0] = self._L2vpn_site_recovery(
                            root, th, service, local_site_pe,
                            l2vpn_plan_path, "cisco-flat-L2vpn-fp-nano-plan-services:local-site",
                            sync_direction, recovery_log)
                    except Exception as ex:
                        self.log.error(f"Exception in FlatL2vpnRecoveryAction: {ex}")
                        traceback.print_exc()
                        recovery_flag[0] = False
                        recovery_log.append((str(ex)))

                if remote_site_pe is not None:
                    try:
                        return_status[1] = self._L2vpn_site_recovery(
                            root, th, service, remote_site_pe,
                            l2vpn_plan_path, "cisco-flat-L2vpn-fp-nano-plan-services:remote-site",
                            sync_direction, recovery_log)
                    except Exception as ex:
                        self.log.error(f"Exception in FlatL2vpnRecoveryAction: {ex}")
                        traceback.print_exc()
                        recovery_flag[1] = False
                        recovery_log.append((str(ex)))

                if sites_only:
                    try:
                        return_status[2] = self._L2vpn_site_recovery(
                            root, th, service, sites_only,
                            l2vpn_plan_path, "cisco-flat-L2vpn-fp-nano-plan-services:site",
                            sync_direction, recovery_log)
                    except Exception as ex:
                        self.log.error(f"Exception in FlatL2vpnRecoveryAction: {ex}")
                        traceback.print_exc()
                        recovery_flag[2] = False
                        recovery_log.append((str(ex)))

                if (return_status[0] is not None and return_status[1] is not None) \
                        or return_status[2] is not None:
                    if "is not in failed state" in str(return_status[0]) \
                            and "is not in failed state" in str(return_status[1]):
                        raise Exception("".join(return_status[:2]))
                    elif "is not in failed state" in str(return_status[2]):
                        raise Exception("".join(return_status[2]))
                else:
                    recovery_log.extend(list(filter(lambda x: x is not None, return_status)))

            except Exception as ex:
                self.log.error(f"Exception in FlatL2vpnRecoveryAction: {ex}")
                traceback.print_exc()
                exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR, str(ex)).
                       set_context("Recovery Action", f"Recovery failed for {service}").finish())

                recovery_log.append("\nRecovery Failed\n\n")
                output.success = False
                recovery_log.append(str(exp))
                output.detail = "".join(recovery_log)
            else:
                if (recovery_flag[0] is not False and recovery_flag[1] is not False) \
                        or recovery_flag[2] is not False:
                    self.log.info("Recovery Complete")
                    recovery_log.append("\nRecovery Complete\n")
                    output.success = True
                    output.detail = "".join(recovery_log)
                else:
                    exp = (CustomActionException(self.log, StatusCodes.RECOVERY_ERROR,
                                                 "\nService Recovery Failed").
                           set_context("Recovery Action",
                                       f"Recovery failed for {service}").finish())
                    self.log.info("Recovery Failed")
                    recovery_log.append("\nRecovery Failed\n")
                    output.success = False
                    recovery_log.append(str(exp))
                    output.detail = "".join(recovery_log)

    def _L2vpn_site_recovery(self, root, th, service, pe_name, l2vpn_plan_path,
                             component_type, sync_direction, recovery_log):
        self.log.info(f"L2vpn_site_recovery service:{service} ,"
                      f"sites: {pe_name} , component_type: {component_type}")

        l2vpn_plan = maagic.get_node(th, l2vpn_plan_path).plan

        sites = {}
        if type(pe_name) == str:
            sites[pe_name] = pe_name
        else:
            sites = pe_name

        for site_name in sites:
            if not (component_type, site_name) in l2vpn_plan.component:
                err_msg = \
                    (f"No such failed site: {site_name} found for recovery in service: {service}")
                self.log.error(err_msg)
                raise Exception(err_msg)

            if component_type != "cisco-flat-L2vpn-fp-nano-plan-services:site":
                site_comp = l2vpn_plan.component[(component_type, site_name)]

                if (site_comp.state["ncs:ready"].status != "failed"
                        and site_comp.state["ncs:init"].status != "failed"):
                    component = component_type[component_type.find(":") + 1: len(component_type)]
                    err_msg = \
                        f"Exception {component} {site_name} " \
                        + f"is not in failed state in service {service}"
                    self.log.error(err_msg)
                    return "\n" + err_msg

            if Utils.is_lsa:
                rfs_node = LsaUtils.get_remote_nso(sites[site_name])
                action_prefix = root.ncs__devices.device[rfs_node].config
            else:
                action_prefix = root

            internal_action = action_prefix.\
                cisco_flat_L2vpn_fp_internal_common__flat_L2vpn_internal_actions.error_recovery

            internal_action_input = internal_action.get_input()
            internal_action_input.service = service
            internal_action_input.sync_direction = sync_direction

            if "local-site" in component_type:
                internal_action_input.local_site_only = pe_name
            elif "remote-site" in component_type:
                internal_action_input.remote_site_only = pe_name
            elif "site" in component_type:
                internal_action_input.site_only = site_name
            else:
                raise Exception(f"{component_type} is not a valid component.type")

            internal_action_output = internal_action(internal_action_input)

            # Check if internal action output failed/success
            if internal_action_output.success:
                recovery_log.append(internal_action_output.detail)
            else:
                # If failed, return with error.
                recovery_log.append(internal_action_output.detail)
                raise Exception("Service Recovery Failed")
