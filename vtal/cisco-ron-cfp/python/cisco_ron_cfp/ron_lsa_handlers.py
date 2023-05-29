# -*- mode: python; python-indent: 4 -*-
import ncs
import threading
import re

from .ron_ml_errors import CustomActionException
from .constants import StrConstants
from cisco_ron_core_fp_common.status_codes import StatusCodes as codes
from core_fp_common import common_utils


class RemotePlanChangeCallback(ncs.dp.Action):
    """
    Remote plan change notification kicker notification callback
    """

    def init(self, init_args):
        self.sync_set = init_args

    """
    Remote kicker handler
    """

    @ncs.dp.Action.action
    def cb_action(self, uinfo, name, kp, input, output):
        self.log.info(f"Remote kicker notif path: {input.path}")
        """
            The trigger path can be
            /ncs:devices/device{%s}/netconf-notifications/recieved-notifications/notification
            {2021-03-04T20:02:53.301093+00:00 0}/data/kicker-triggered
            This reg-ex will parse and retrieve the LSA device on which the kicker notification
            has arrived.
        """
        rfs_device = re.search("device{(.*?)}/netconf-notifications", input.path).group(
            1
        )
        with ncs.maapi.single_read_trans(
            uinfo.username, "system", db=ncs.OPERATIONAL
        ) as th:
            self.log.info(f"User {uinfo.username}")
            event = ncs.maagic.get_node(th, input.path)
            plan_path = event.path
            self.log.info(f"Remote kicker plan path: {plan_path}")
            self.sync_set.put((rfs_device, plan_path))  # Tuple(rfs_device, plan_path)


class RemotePlanChangeHandlerThread(threading.Thread):
    """
    Remote plan change notification kicker notification handling thread
    """

    def __init__(self, log, sync_set):
        super().__init__()
        self.log = log
        self.sync_set = sync_set

    def run(self):
        self.log.info("Started remote callback handler thread")
        while (item := self.sync_set.get()):
            try:
                self.log.info(f"RemoteKicker : handling {item}")
                remote_nso = item[0]
                remote_plan_path = item[1]
                l_user = common_utils.get_local_user()
                self.copy_over(l_user, remote_nso, remote_plan_path)

                ## Redeploy service/zombie
                with ncs.maapi.single_read_trans(l_user, StrConstants.system) as th:
                    root = ncs.maagic.get_root(th)
                    redeployer = root.cisco_ron_cfp__ron.redeploy_kicker_callback
                    action_input = redeployer.get_input()
                    action_input.path = remote_plan_path
                    redeployer(action_input)
            except Exception as exp:
                err = (
                    CustomActionException(
                        self.log, codes.REMOTE_KICKER_HANDLER_ERROR, str(exp)
                    )
                    .set_context("Remote kicker", "Failed to copy over internal plan")
                    .add_state("internal service", item)
                    .finish()
                )
                self.log.error(err)

        self.log.info(" Good bye! : from RemoteKickerHandler")

    def copy_over(self, l_user, remote_nso, remote_plan_path):

        with ncs.maapi.single_write_trans(l_user, "system", db=ncs.OPERATIONAL) as th:
            root = ncs.maagic.get_root(th)
            live_status = root.ncs__devices.device[remote_nso].live_status
            """
                The remote trigger path can be
                /cisco-zr-cfp:zr/cisco-zr-cfp:dco-plan{%s %s} (or)
                /tapi-common:context/tapi-connectivity:connectivity-context
                /connectivity-service-plan{%s}
            """
            if "cisco-zr-cfp" in remote_plan_path:
                match = re.search("{(.*?) (.*?)}", remote_plan_path)
                service = match.group(1)
                endpoint = match.group(2)
                l_plan = root.cisco_ron_cfp__ron.remote_local_data.dco_plan
                r_plan = live_status.cisco_zr_cfp__zr.dco_plan
                # Delete old copy of the plan if exists
                if (service, endpoint) in l_plan:
                    self.log.debug(
                        f"deleting local plan @ {l_plan._path} for"
                        f"{service} {endpoint}"
                    )
                    del l_plan[(service, endpoint)]

                l_mdata = root.cisco_ron_cfp__ron.remote_local_data.dco
                r_mdata = live_status.cisco_zr_cfp__zr.dco

                if (service, endpoint) in r_plan:
                    # Copy the latest plan over to local copy
                    r_plan_instance = r_plan[(service, endpoint)].plan
                    # Create instance
                    l_plan_instance = l_plan.create(service, endpoint)
                    # Copy plan component
                    self.read_component_data(r_plan_instance, l_plan_instance, False)
                    # Try to read metadata if there is one
                    self.read_dco_metadata(l_mdata, r_mdata, (service, endpoint))
                else:
                    # Clean up the local copy of the ZR/DCO meta-data
                    del l_mdata[(service, endpoint)]
            else:
                match = re.search("plan{(.*?)}", remote_plan_path)
                cs_uuid = match.group(1)
                remote_data = root.cisco_ron_cfp__ron.remote_local_data
                l_plan = remote_data.connectivity_service_plan
                r_plan = (
                    live_status.tapi_common__context.tapi_connectivity__connectivity_context.
                    connectivity_service_plan
                )
                # Delete old copy of the plan if exists
                if cs_uuid in l_plan:
                    self.log.debug(
                        f"deleting local plan @ {l_plan._path} for {cs_uuid}"
                    )
                    del l_plan[cs_uuid]

                if cs_uuid in r_plan:
                    # Copy the latest plan over to local copy
                    r_plan_instance = r_plan[cs_uuid].plan
                    # Create instance
                    l_plan_instance = l_plan.create(cs_uuid)
                    # Copy plan component
                    self.read_component_data(r_plan_instance, l_plan_instance)
                    if (
                        l_plan_instance.plan.component["ncs:self", "self"]
                        .state[StrConstants.ncs_ready]
                        .status
                        == "reached"
                    ):
                        self._read_optics_data(root, remote_data, cs_uuid, live_status)
                else:
                    del remote_data.connectivity_service_oper_data[cs_uuid]
            th.apply()

    def read_component_data(self, r_plan_instance, l_plan_instance, with_details=True):
        # Copy plan component
        for r_comp in r_plan_instance.component:
            l_comp = l_plan_instance.plan.component.create(r_comp.type, r_comp.name)
            l_comp.back_track = r_comp.back_track
            for r_state in r_comp.state:
                name = r_state.name
                if "config-apply" in r_state.name or "installed" in r_state.name:
                    name = "cisco-ron-cfp:" + r_state.name.split(":")[1]
                l_state = l_comp.state.create(name)
                l_state.status = r_state.status
                l_state.when = r_state.when
                l_state.service_reference = r_state.service_reference
                if r_state.post_action_status:
                    l_state.post_action_status = r_state.post_action_status
            if r_comp.status_code:
                l_comp.status_code = r_comp.status_code

        # Copy plan failed
        if r_plan_instance.failed:
            l_plan_instance.plan.failed.create()
        if r_plan_instance.error_info:
            local_error_info = l_plan_instance.plan.error_info.create()
            local_error_info.message = r_plan_instance.error_info.message
            # local_error_info.log_entry = str(r_plan_instance.error_info.log_entry)
        # Copy commit queue
        if r_plan_instance.commit_queue:
            l_cq = l_plan_instance.plan.commit_queue.create()
            for cq_item in r_plan_instance.commit_queue.queue_item:
                l_cq.queue_item.create(cq_item.id)
        # Errors on the plan
        if with_details:
            for r_detail in r_plan_instance.status_code_detail:
                l_detail = l_plan_instance.plan.create(r_detail.type, r_detail.name)
                l_detail.code = r_detail.code
                for r_cntx in r_detail.context:
                    l_cntx = l_detail.create(r_cntx.context_name)
                    l_cntx.context_msg = r_cntx.context_msg
                l_detail.severity = r_detail.severity
                l_detail.recommended_action = r_detail.recommended_action

    def _read_optics_data(self, root, local_cs_data, cs_uuid, rfsnode_live_status):
        remote_cs_data = (
            rfsnode_live_status.tapi_common__context.tapi_connectivity__connectivity_context.
            connectivity_service_oper_data
        )
        if cs_uuid in remote_cs_data:
            local_cs_instance = local_cs_data.connectivity_service_oper_data.create(cs_uuid)
            remote_cs_data_instance = remote_cs_data[cs_uuid]
            for end_point_data in remote_cs_data_instance.end_point:
                local_end_point = local_cs_instance.end_point.create(
                    end_point_data.local_id
                )
                remote_optics_data = end_point_data.optics_data
                local_end_point.optics_data.transmit_power = (
                    remote_optics_data.transmit_power
                )
                local_end_point.optics_data.frequency = remote_optics_data.frequency
                local_end_point.optics_data.grid_type = remote_optics_data.grid_type

    def read_dco_metadata(self, l_mdata, r_mdata, key):
        if key in r_mdata:
            l_mdata_ins = l_mdata.create(key)
            r_mdata_ins = r_mdata[key]
            for interface in r_mdata_ins.metadata.interface:
                l_interface = l_mdata_ins.interface.create(interface.index)
                l_interface.name = interface.name
