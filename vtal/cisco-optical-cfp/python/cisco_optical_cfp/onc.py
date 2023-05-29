import ncs
import traceback
import re
import sys
import xml.etree.ElementTree as ET
import time
import subprocess
from datetime import datetime
from enum import Enum, auto

from .optical_errors import DeviceConfigException, UserError
from .constants import StrConstants as str_con

from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes
from core_fp_common import common_utils


class NotifiType(Enum):

    UNKNOWN = auto()
    LC_STATE_CHANGE = auto()
    CS_DELETE = auto()


class onc:
    def conf_connectivity_service(self, service):
        self.log.info(
            "Configuring CONNECTIVITY-SERVICE NETCONF device {}".format(
                service.controller
            )
        )

        ## Applying connfig
        t_vars = ncs.template.Variables()
        template = ncs.template.Template(service)
        l_user = common_utils.get_local_user()
        # Workaround for ONC device populating extra frequency and power config
        t_vars.add("FREQUENCY", "")
        power = {}
        with ncs.maapi.single_read_trans(l_user, str_con.system, db=ncs.RUNNING) as th:
            root = ncs.maagic.get_root(th)
            if (service.uuid in root.ncs__devices.device[service.controller].config.
                    tapi_common__context.tapi_connectivity__connectivity_context.
                    connectivity_service):
                end_points = root.ncs__devices.device[service.controller].config.\
                    tapi_common__context.tapi_connectivity__connectivity_context.\
                    connectivity_service[service.uuid].end_point
                for ep in service.end_point:
                    otsi_config = end_points[f"{ep.local_id}2"]\
                        .otsia_connectivity_service_end_point_spec\
                        .otsi_config[f"{ep.local_id}2_otsi"]
                    if otsi_config:
                        if otsi_config.central_frequency.central_frequency:
                            self.log.debug("central-frequency from device config:"
                                           + f" {otsi_config.central_frequency.central_frequency}")
                            t_vars.add("FREQUENCY", otsi_config.central_frequency.
                                       central_frequency)
                        if otsi_config.transmit_power.total_power:
                            self.log.debug("transmit-power from device config:"
                                           f" {otsi_config.transmit_power.total_power}")
                            power[ep.local_id] = otsi_config.transmit_power.total_power
        if service.passthrough.exists():
            template.apply("cisco-optical-cfp-onc-passthrough-template", t_vars)
        else:
            template.apply("cisco-optical-cfp-onc-template", t_vars)
            if len(power) > 0:
                root = ncs.maagic.get_root(ncs.maagic.get_trans(service))
                end_points = root.ncs__devices.device[service.controller].config.\
                    tapi_common__context.tapi_connectivity__connectivity_context.\
                    connectivity_service[service.uuid].end_point
                for ep, value in power.items():
                    end_points[f"{ep}2"].otsia_connectivity_service_end_point_spec.\
                        otsi_config[f"{ep}2_otsi"].transmit_power.total_power = value

    def set_connectivity_service_oper_data(self, uinfo, name, kp, input, output):

        # Get the Notification details and validate if we should process it
        notif_type = NotifiType.UNKNOWN
        cs_uuid = None
        event_time = None
        config_chng_notif = ncs.maagic.get_node(self.root, input.path)
        for edit in config_chng_notif.edit:
            if (match := re.search(str_con.lc_state, edit.target)):
                notif_type = NotifiType.LC_STATE_CHANGE
                cs_uuid = match.group(1)
            elif edit.operation == "delete" and (match := re.search(str_con.cs_del, edit.target)):
                notif_type = NotifiType.CS_DELETE
                cs_uuid = match.group(1)

        self.log.info(f"Identified notification type {notif_type}")
        if notif_type == NotifiType.UNKNOWN:
            self.log.info(f" Dropping {notif_type} notification ")
            return

        self.log.info(f"Connectivity service UUID {cs_uuid}")
        ## Get service node from action path
        connectivity_context = ncs.maagic.get_node(self.root, kp)
        ## Do we own the connectivity-service ?
        if ((cs_uuid not in connectivity_context.connectivity_service)
                and (cs_uuid not in connectivity_context.connectivity_service_oper_data)):
            self.log.info(f"Dropping {notif_type}, unknow connectivity service UUID {cs_uuid}")
            return

        if notif_type != NotifiType.UNKNOWN:
            notification = ncs.maagic.cd(config_chng_notif, "../../")
            event_time = notification.event_time

        ## All looks good, Handle the notification
        """
            The trigger path would be
            /ncs:devices/device{ONC-31}/netconf-notifications/received-notifications/notification
            {2021-03-04T20:02:53.301093+00:00 0}/data/ncn:netconf-config-change
            This reg-ex will parse for the name followed by device{}
        """
        match = re.search("device{(.*)}/netconf-notifications", input.path)
        onc_controller = match.group(1)

        l_user = common_utils.get_local_user()
        with ncs.maapi.single_write_trans(l_user, str_con.system, db=ncs.RUNNING) as th:
            rw_root = ncs.maagic.get_root(th)
            connectivity_context = ncs.maagic.get_node(th, kp)
            key = None
            try:
                if notif_type == NotifiType.CS_DELETE:
                    self.log.info(f"Deleting connectivity-service-oper-data for uuid {cs_uuid}")
                    # Makes sure oper-data is not deleted on re-plays
                    if (cs_uuid not in connectivity_context.connectivity_service
                            and cs_uuid in connectivity_context.connectivity_service_oper_data):
                        del connectivity_context.connectivity_service_oper_data[cs_uuid]
                else:
                    self.log.info(f"Handling lifecycle-state change for {cs_uuid}")
                    live_status_context = rw_root.devices.device[onc_controller].live_status. \
                        tapi_common__context
                    # Step 1: Read lifecycle-state
                    lc_state = live_status_context.tapi_connectivity__connectivity_context. \
                        connectivity_service[cs_uuid].lifecycle_state
                    if not lc_state:
                        # lifecycle-state is not initialized yet
                        self.log.info(f"Lifecycle state is not set yet {lc_state}")
                        return

                    # Step 2: update oper data
                    connectivity_oper = connectivity_context. \
                        connectivity_service_oper_data.create(cs_uuid)
                    key = (lc_state, event_time)
                    connectivity_oper.lifecycle_state.create(key)

                    # Step 3: If installed then fetch oper data from device and redeploy service.
                    if lc_state == str_con.installed:

                        ## Only fecth is not set property FETCH_OPER_DATA
                        service = connectivity_context.connectivity_service[cs_uuid]
                        self.service = service
                        try:
                            ep_optic_data = self.fetch_tx_fr(live_status_context)
                            self.log.info(f"endpoint-optics data {ep_optic_data}")
                            for optics_data in ep_optic_data:
                                ep_oper = connectivity_oper.end_point.create(optics_data[0])
                                ep_oper.optics_data.transmit_power = optics_data[1]
                                ep_oper.optics_data.grid_type = optics_data[2]
                                ep_oper.optics_data.frequency = int(optics_data[3])
                        except DeviceConfigException as exp:
                            connectivity_oper.lifecycle_state[key].error = exp.value
                            connectivity_oper.lifecycle_state[key].code = exp.statusCode.code
                    elif lc_state == str_con.pending_removal:
                        lifecycle_state = connectivity_oper.lifecycle_state[key]
                        lifecycle_state.error = "Deployment failed on ONC, reason unknown."
                        lifecycle_state.code = StatusCodes.CS_FAILED_ON_ONC.cfp_code
                    else:
                        self.log.info("ONC oper-date is not ready yet,"
                                      " will wait for more notifications")

            except Exception:
                self.log.error(AsciiArt.roadblock)
                self.log.error(traceback.format_exc())
                _, ex_value, _ = sys.exc_info()
                if key is None:
                    if notif_type == NotifiType.CS_DELETE:
                        time = datetime.utcnow().isoformat()
                        key = (str_con.pending_removal, time)
                    else:
                        time = datetime.utcnow().isoformat()
                        if len(connectivity_oper.lifecycle_state) > 0:
                            last_state = iter(connectivity_oper.lifecycle_state).next()
                            key = (last_state.type, time)
                        else:
                            key = (str_con.planned, time)

                lifecycle_state = connectivity_oper.lifecycle_state.create(key)
                lifecycle_state.error = ex_value
                lifecycle_state.code = StatusCodes.DATA_RETRIVAL_ERROR.cfp_code

            th.apply()

    def fetch_tx_fr(self, live_status_context):
        optics_data = []
        try:
            uuid = self.service.uuid
            connectivity_service = live_status_context.tapi_connectivity__connectivity_context.\
                connectivity_service[uuid]
            for endpoint in self.service.end_point:
                local_id = endpoint.local_id + "2"
                end_point = connectivity_service.end_point[local_id]
                otsi = end_point. \
                    tapi_photonic_media__otsia_connectivity_service_end_point_spec \
                    .otsi_config[local_id + "_otsi"]
                tx_power = otsi.transmit_power.total_power
                self.log.info(f"tx-power {tx_power}")
                central = otsi.central_frequency.central_frequency
                # very weird behaviour noticed with confid.
                # User provided values are returned as is.
                # But, when server set the frz value, then confd *100
                self.log.info(f"central freq {central}")
                # safety valve check
                if not(central >= 1861000 and central <= 1961000):
                    dif_len = len(str(central)) - 7
                    d = 10 ** dif_len
                    # truncate
                    central = central / d
                    self.log.info(f"calculated central freq {central}")

                optics_data.append((endpoint.local_id, tx_power, "100mhz-grid", central))
        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            self.log.error(traceback.format_exc())
            raise DeviceConfigException(self.log, StatusCodes.DATA_RETRIVAL_ERROR, str(exp)) \
                .set_context("oper-data read", "Failed to read endpoint optics data"). \
                add_state("service", self.service._path).finish()
        return optics_data

    def fetch_sip(self, root, username, controller, sip_lookup_keys):

        sip_dict = {}
        i = 0
        for key in sip_lookup_keys:
            if ":" in key:
                # Add-Drop case
                inv_id = self._get_inventory_key(key)
                sip_lookup = self._get_inventory_filter(inv_id)
                sip_dict[i] = (sip_lookup, "AD", inv_id)
            else:
                sip_lookup = self._get_sip_filter(key)
                sip_dict[i] = (sip_lookup, "SIP", key)
            i = i + 1

        sip_query = self._get_sip_query(sip_dict[0][0], sip_dict[1][0])
        self.log.debug("SIP query string is {}".format(sip_query))

        sips = []
        try:
            # ncs_run_dir = os.getenv('NCS_RUN_DIR', '.')
            optical_pkg_dir = root.packages.package["cisco-optical-cfp"].directory
            self.log.debug("Optical package directory {}".format(optical_pkg_dir))

            cmd = (
                "python "
                + optical_pkg_dir
                + "/python/cisco_optical_cfp/ncc.py "
                + "--device='{}' --user='{}' --rpc='--{}--'".format(
                    controller, username, sip_query
                )
            )
            result = subprocess.run(
                cmd, stdout=subprocess.PIPE, shell=True
            ).stdout.decode("utf-8")
            self.log.debug("SIP query result is {}".format(result))

            rt = ET.fromstring(result)
            for k in sip_dict:
                if sip_dict[k][1] == "AD":
                    xpath = (
                        "./{urn:ietf:params:xml:ns:netconf:base:1.0}data/"
                        + "{urn:onf:otcc:yang:tapi-common}context/{urn:onf:otcc:yang:tapi-common}"
                        + "service-interface-point/{urn:onf:otcc:yang:tapi-common}name["
                        + "{urn:onf:otcc:yang:tapi-common}value-name='INVENTORY_ID']["
                        + "{urn:onf:otcc:yang:tapi-common}value='"
                        + sip_dict[k][2]
                        + "']"
                        + "/../{urn:onf:otcc:yang:tapi-common}uuid"
                    )
                else:
                    xpath = (
                        "./{urn:ietf:params:xml:ns:netconf:base:1.0}data/"
                        + "{urn:onf:otcc:yang:tapi-common}context/{urn:onf:otcc:yang:tapi-common}"
                        + "service-interface-point[{urn:onf:otcc:yang:tapi-common}uuid='"
                        + sip_dict[k][2]
                        + "']/{urn:onf:otcc:yang:tapi-common}uuid"
                    )

                sip_uuid = rt.findtext(xpath)
                sips.append(sip_uuid)
                if sip_uuid is None:
                    self.log.error(
                        "Failed to retreive SIP for lookup key {}".format(
                            sip_dict[k][2]
                        )
                    )
        except Exception:
            self.log.error(AsciiArt.roadblock)
            self.log.error("Exception while retreiving SIPs from ONC ")
            self.log.error(traceback.format_exc())

        self.log.info("SIPS retrieved are {}".format(sips))
        return sips

    def _get_sip_filter(self, sip):
        sip_filter = """<service-interface-point>
             <uuid>{}</uuid>
             </service-interface-point>"""
        return sip_filter.format(sip)

    def _get_inventory_filter(self, inv_id):

        inv_filter = """<service-interface-point>
             <uuid/>
             <name>
               <value-name>INVENTORY_ID</value-name>
               <value>{}</value>
             </name>
           </service-interface-point>"""
        return inv_filter.format(inv_id)

    def _get_sip_query(self, sip1, sip2):
        sip_main_query = """<get-config>
                         <source>
                           <running/>
                         </source>
                         <filter type="subtree">
                            <context xmlns="urn:onf:otcc:yang:tapi-common">
                            {}
                            {}
                            </context>
                         </filter>
                       </get-config>"""
        return sip_main_query.format(sip1, sip2)

    def _get_inventory_key(self, add_drop):
        (ne, rem) = add_drop.split(":")
        (rack, shelf, slot, port) = rem.split("/")
        return "/ne={}/r={}/sh={}/sl={}/s_sl=0/p={}".format(ne, rack, shelf, slot, port)

    def connectivity_service_self_test(self, uinfo, root, connectivity_service):
        return ("success", None)

    def run_self_test(self, action, action_input, tunnel_name, device):
        max_retries = 2
        for retry in range(max_retries):
            try:
                output = action(action_input)
                # The output for this action comes with different number of spaces
                # between Admin/ Oper and status so removing all spaces before comparing.
                result = output.result.replace(" ", "")
                self.log.info("Self Test result: {}".format(output.result))
                ## Return self test result
                if "Admin:up" in result and "Oper:up" in result:
                    return ("success", None)
                elif "Admin:down" in result:
                    return ("failed", "Admin state is down")
                elif "Oper:down" in result:
                    return ("failed", "Operational state is down")
                elif "Admin:down" in result and "Oper:down" in result:
                    return ("failed", "Both Admin & Operational states are down")
                else:
                    return ("failed", "No connectivity-service status found on device")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (
                    max_retries - 1
                ):
                    self.log.info(
                        "Connection reset on device, reached max-retries"
                    )
                    return (
                        "failed",
                        "Device ssh session being used by another transaction. "
                        + "Retry self-test with following command: "
                        + "'request ietf-te te tunnels tunnel "
                        + tunnel_name
                        + " action self-test'",
                    )
                elif "Connection reset in new state" in str(e):
                    self.log.info(
                        "Connection reset on device, will retry self test"
                    )
                else:
                    traceback.print_exc()
                    raise e
            time.sleep(30)
            self.log.info("Retrying self test")

    def validate_onc_notif_sub(self, root, log, controller):
        """
        Checks if ONC device for a service to be created has a netconf notification subscription
        and if it is in running state.
        """
        try:
            root.devices.device[controller].netconf_notifications\
                .subscription[controller + '-nc-sub']
        except KeyError:
            raise UserError(
                log, StatusCodes.ONC_NETCONF_NOTIFICATION_ERROR,
                f"Invalid Netconf Subscription for ONC: {controller}"
            ).set_context(
                "Validation",
                f"Invalid Netconf Subscription for ONC: {controller}",
            ).add_state(
                "Configuration Status", "Not Configured"
            ).finish()

    def partial_sync_optical_service_interface(self, controller, root):
        self.log.info(
            f"Performing partial sync-from on CONNECTIVITY-SERVICE NETCONF device {controller} "
            "for service-interface-point"
        )
        path = f"/ncs:devices/device[name='{controller}']" \
               "/config/tapi-common:context/service-interface-point"
        try:
            partial_sync_action = root.ncs__devices.partial_sync_from
            action_input = partial_sync_action.get_input()
            action_input.path.create(path)
            action_output = partial_sync_action(action_input)
        except Exception as e:
            self.log.debug(f"Error performing partial sync-from on {controller}: {str(e)}")
            return False, "device sync result: false; " \
                          f"info: {str(e)}"

        sync_result = action_output.sync_result[controller]
        if sync_result is None or sync_result.result is None:
            return False, f"Failed to partial sync from {controller}"
        elif sync_result.result:
            return True, "device sync successful"
        else:
            return False, f"device sync result: {sync_result.result}; " \
                          f"info: {sync_result.info}"

    @staticmethod
    def list_sips(controller, root):
        optical_service_interface = []  # (sip/uuid, network-element, optical-add-drop)
        pattern = r'/ne=(.*)/r=(.*)/sh=(.*)/sl=(.*)/s_sl=0/p=(.*)'
        for entry in root.ncs__devices.device[controller]\
                .config.tapi_common__context.service_interface_point:
            ne = ad = ""
            if entry.name["INVENTORY_ID"]:
                matches = re.match(pattern, entry.name["INVENTORY_ID"].value).groups()
                if matches:
                    ne = matches[0]
                    ad = f"{matches[1]}/{matches[2]}/{matches[3]}/{matches[4]}"
            optical_service_interface.append((entry.uuid, ne, ad))
        return optical_service_interface

    def validate_sips(self, root, controller, sips):
        device_sips = root.ncs__devices.device[
            controller].config.tapi_common__context.service_interface_point
        sip_map = set()
        for entry in device_sips:
            if entry.name.exists('INVENTORY_ID') and entry.name['INVENTORY_ID'].value is not None:
                sip_map.add(entry.name['INVENTORY_ID'].value)
        sips_validity = []
        for entry in sips:
            if entry.optical_sip is None:
                # Add-Drop case
                (rack, shelf, slot, port) = entry.optical_add_drop.split("/")
                inv_id = \
                    f"/ne={entry.network_element}/r={rack}/sh={shelf}/sl={slot}/s_sl=0/p={port}"
                self.log.debug(f"SIP query string is {inv_id}")
                valid = inv_id in sip_map
            else:
                valid = device_sips.exists(entry.optical_sip)
            sips_validity.append((entry.index, valid))
        return sips_validity

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
