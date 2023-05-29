import ncs
import traceback
import time
from cisco_zr_cfp.network import getIpAddress, getNetMask
from cisco_zr_cfp.constants import StrConstants as locl_conts
from cisco_zr_cfp.namespaces.ciscoZrCfp_ns import ns as dco_zr_ns

from cisco_ron_core_fp_common.constants import AsciiArt
from cisco_ron_core_fp_common.status_codes import StatusCodes
from cisco_ron_core_fp_common import utils as RonUtils


class NativeXR:
    def conf_zr_dco(self):
        self.log.info("Configuring ZR DCO NETCONF XR device {}".format(self.service.router))
        template_tag = self.get_template_tag()
        # TODO: All the required details must be part of the service payload
        capability = self.get_pluggable_capability(self.service, self.service.transceiver_pid)

        # Step 1.0 Configure the port to match requested breakout
        if self.service.flexport_linecard_pid:
            # So we have flexport line card.
            flex_card = self.service.flexport_linecard_pid
            port = str(self.service.line_port).split("/")
            t_vars = ncs.template.Variables()
            t_vars.add("R", port[0])
            t_vars.add("S", port[1])
            t_vars.add("I", "0")
            t_vars.add("P", port[3])
            t_vars.add("P_SEC", int(port[3]) + 1)
            # Identify which one it is and populate the template vars accordingly
            if flex_card in locl_conts.asr9k_flex_cards or flex_card in locl_conts.a99_flex_cards:
                # The ASR 9000 Flex cards have configurable “slices” with 400G of total bandwidth.
                # Specific configuration is used to determine which ports are
                t_vars.add("PORT_MODE", 'A9K')
                if self.service.mode == dco_zr_ns.cisco_zr_cfp_muxponder_:
                    if self.service.bandwidth == "400":
                        t_vars.add("SPEED", '4x-hundred-gig-e')
                    elif self.service.bandwidth == "200":
                        t_vars.add("SPEED", '2x-hundred-gig-e')
                else:
                    if self.service.bandwidth == "400":
                        t_vars.add("SPEED", '1x-four-hundred-gig-e')
                    elif self.service.bandwidth == "100":
                        if flex_card in locl_conts.asr9k_flex_cards:
                            t_vars.add("PORT_MODE", 'None')
                        t_vars.add("SPEED", '1x-hundred-gig-e')

            elif flex_card in locl_conts.asr9903_flex_cards:
                # The ASR 9000 Flex cards have configurable “slices” with 400G of total bandwidth.
                # Specific configuration is used to determine which ports are
                t_vars.add("SPEED", '1x-four-hundred-gig-e')
                if self.service.bandwidth == "100":
                    t_vars.add("PORT_MODE", "None")
                else:
                    t_vars.add("PORT_MODE", flex_card)
                    if self.service.mode == dco_zr_ns.cisco_zr_cfp_muxponder_:
                        t_vars.add("SPEED", '4x-hundred-gig-e')
            else:
                t_vars.add("PORT_MODE", "NCS")
                if self.service.bandwidth == "400":
                    if self.service.mode == dco_zr_ns.cisco_zr_cfp_muxponder_:
                        speed = "4x100"
                    else:
                        speed = "400"
                elif self.service.bandwidth == "300":
                    speed = "3x100"
                elif self.service.bandwidth == "200":
                    speed = "2x100-PAM4"
                elif self.service.bandwidth == "100":
                    speed = "1x100"
                t_vars.add("SPEED", speed)
            if flex_card in locl_conts.nc57_mpa_flex_cards:
                t_vars.add("I", port[2])
            # Apply template to configure the flexline port for the deployment speed
            template = ncs.template.Template(self.service)
            template.apply(f"zr-dco-{template_tag}-flexport-linecard-template", t_vars)

        # Step 2.0 Configure the optical controller
        ## Construct breakout if not flex port
        breakout = ""
        if self.service.flexport_linecard_pid is None:
            if self.service.mode == dco_zr_ns.cisco_zr_cfp_muxponder_:
                if self.service.bandwidth == "400":
                    breakout = "4x100"
                elif self.service.bandwidth == "300":
                    breakout = "3x100"
                elif self.service.bandwidth == "200":
                    breakout = "2x100"
                elif self.service.bandwidth == "100":
                    breakout = "1x100"
            else:
                if self.service.bandwidth == "100":
                    breakout = "1x100"

        fec = self.service.fec
        if self.service.fec is None:
            fec = capability.fec

        # Check for Rack 0: N540-24Q8L2DD-SYS for Arches
        speed_pid = ""
        if self.service.platform_pid \
                and self.service.platform_pid in locl_conts.ncs540_pids:
            speed_pid = "**True**"

        dac = self.service.dac_rate
        if self.service.dac_rate is None:
            ## Get 1st value
            dac = capability.dac_rate.as_list()[0]

        t_vars = ncs.template.Variables()
        t_vars.add("BREAKOUT", breakout)
        t_vars.add("MODULATION", capability.modulation.as_list()[0])
        # t_vars.add("BAUD_RATE", capability.baud_rate)
        t_vars.add("DAC", dac)
        t_vars.add("FEC", fec)
        t_vars.add("SPEED_PID", speed_pid)

        ## Applying controller config
        template = ncs.template.Template(self.service)
        template.apply(f"zr-dco-{template_tag}-template", t_vars)

        # Step 3.0 Configure l2 and l3 interfaces
        for bundle in self.service.bundle:
            t_vars.clear()
            t_vars.add("NAME", "Bundle-Ether" + str(bundle.id))
            t_vars.add("DESCRIPTION", "")
            if bundle.description is not None and bundle.description != "":
                t_vars.add("DESCRIPTION", str(bundle.description))
            t_vars.add("IPV4_ADDR", "")
            t_vars.add("IPV4_MASK", "")
            t_vars.add("IPV6_ADDR", "")
            t_vars.add("IPV6_PREFIX_LENGTH", "")
            if bundle.ip_address.exists():
                if bundle.ip_address.v4 is not None and bundle.ip_address.v4 != "":
                    t_vars.add("IPV4_ADDR", getIpAddress(bundle.ip_address.v4))
                    t_vars.add("IPV4_MASK", getNetMask(bundle.ip_address.v4))

                if bundle.ip_address.v6 is not None and bundle.ip_address.v6 != "":
                    token = str(bundle.ip_address.v6).split("/")
                    t_vars.add("IPV6_ADDR", token[0])
                    t_vars.add("IPV6_PREFIX_LENGTH", token[1])

            ## Applying interface IP config
            template.apply(f"zr-dco-{template_tag}-L3-template", t_vars)

        has_srlg = False
        if len(self.service.srlg.value_list) > 0 or len(self.service.srlg.name_list) > 0 \
           or self.service.srlg.group is not None:
            has_srlg = True

        for interface in self.service.interface:
            if self.service.mode == "muxponder":
                name = (
                    "HundredGigE" + str(self.service.line_port) + "/" + str(interface.index)
                )
            else:
                if self.service.bandwidth == "400":
                    name = "FourHundredGigE" + str(self.service.line_port)
                else:
                    # A9K and 9903 (in 100G case) should use HundredGigE main interface
                    if self.service.flexport_linecard_pid in locl_conts.asr9k_flex_cards \
                            or self.service.flexport_linecard_pid in locl_conts.asr9903_flex_cards:
                        name = "HundredGigE" + str(self.service.line_port)
                    else:
                        name = "HundredGigE" + str(self.service.line_port) \
                               + "/" + str(interface.index)
            t_vars.clear()
            t_vars.add("NAME", name)
            t_vars.add("DESCRIPTION", "")
            if interface.ip_address.exists():
                t_vars.add("IPV4_ADDR", "")
                t_vars.add("IPV4_MASK", "")
                if (
                    interface.ip_address.v4 is not None
                    and interface.ip_address.v4 != ""
                ):
                    t_vars.add("IPV4_ADDR", getIpAddress(interface.ip_address.v4))
                    t_vars.add("IPV4_MASK", getNetMask(interface.ip_address.v4))

                t_vars.add("IPV6_ADDR", "")
                t_vars.add("IPV6_PREFIX_LENGTH", "")
                if (
                    interface.ip_address.v6 is not None
                    and interface.ip_address.v6 != ""
                ):
                    token = str(interface.ip_address.v6).split("/")
                    t_vars.add("IPV6_ADDR", token[0])
                    t_vars.add("IPV6_PREFIX_LENGTH", token[1])

                ## Applying interface IP config
                template.apply(f"zr-dco-{template_tag}-L3-template", t_vars)

            t_vars.clear()
            t_vars.add("NAME", name)
            t_vars.add("BUNDLE_ID", "")
            if interface.membership.exists():
                t_vars.add("BUNDLE_ID", interface.membership.bundle_id)
                t_vars.add("BUNDLE_MODE", interface.membership.mode)

                ## Applying interface bundle config
                template.apply(f"zr-dco-{template_tag}-L2-template", t_vars)

            ## Applying srlg config
            if has_srlg:
                template.apply(f"zr-dco-{template_tag}-srlg-template", t_vars)

            # Persist the Interface into Matadata
            m_data = self.service.metadata.interface.create(interface.index)
            m_data.name = name

    def get_pluggable_capability(self, service, pid):
        root = self.root
        # Should we maintain pluggable_capabilities at two levels
        # RON-ML and ZR ?
        capability = root.cisco_zr_cfp__zr.transceiver_capability[
            pid, service.mode, service.bandwidth
        ]
        return capability

    ## (Applicable - True/False, Valid - True/False, Pid value)
    def get_pid_info(self, router, port, mode, bandwidth, dac_rate):
        # For Transceiver capability validation
        capability_pid = None
        capability_valid = False
        capability_err = None
        # For Flexport card validation
        flexport_app = False
        flexport_pid = None
        flexport_valid = False
        flexport_err = None
        # For specific PID
        platform_pid = None
        platform_pid_app = False
        platform_pid_valid = False
        platform_pid_err = None

        root = self.root

        # Step 1: Validate port format
        if len(str(port).split("/")) != 4:
            capability_err = f"Port {port} provided is not proper R/S/I/P format "
            pids = ((True, False, None, capability_err),
                    (False, False, None, flexport_err),
                    (False, False, None, platform_pid_err))
            return pids

        # Step 2: Validate Transceiver PID and capability
        try:
            ## Get pluggable PID of this controller from optics operational to match capacity
            ##   using Cisco-IOS-XR-controller-optics-oper

            pid = (
                root.devices.device[router]
                .live_status.optics_oper.optics_ports.optics_port["Optics" + str(port)]
                .optics_info.transceiver_info.optics_pid
            )
            self.log.debug(f"PID retreived for port {router} from end-point device {port} is {pid}")
            transceiver_capability = root.cisco_zr_cfp__zr.transceiver_capability
            if (pid, mode, bandwidth) in transceiver_capability:
                if dac_rate is None or \
                        dac_rate in transceiver_capability[pid, mode, bandwidth].dac_rate:
                    capability_pid = pid
                    capability_valid = True
                else:
                    capability_pid = None
                    capability_valid = False
                    capability_err = (StatusCodes.PLUGGABLE_CAPABILITY_NOT_SUPPORTED.cfp_code
                                      + f" : pid {pid} is not supported with dac-rate {dac_rate}"
                                      + f" and bandwidth {bandwidth}")
            else:
                capability_pid = None
                capability_valid = False
                capability_err = (StatusCodes.PLUGGABLE_CAPABILITY_NOT_SUPPORTED.cfp_code
                                  + f" : Hardware capability for the {{{pid}, {mode}, {bandwidth}}}"
                                  " cannot be found")
        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            self.log.error(traceback.format_exc())
            capability_pid = None
            capability_valid = False
            capability_err = StatusCodes.FETCH_OPTICS_ERROR.cfp_code + " : " + str(exp)

        # Step 3: Check for flexport cards
        port = str(port).split("/")
        r = port[0]
        s = port[1]
        i = port[2]
        p = port[3]
        try:
            qstr = f"{r}/{s}"
            if int(i) > 0:
                # If the sub-slot value is not 0 then use it to query
                qstr = f"{r}/{s}/{i}"

            # Get linecard PID using Cisco-IOS-XR-invmgr-oper
            card_pid = None
            try:
                entities = root.devices.device[router].live_status. \
                    Cisco_IOS_XR_invmgr_oper__inventory.entities.entity
                if qstr in entities:
                    card_pid = entities[qstr].attributes.inv_basic_bag.model_name
            except Exception as exp:
                self.log.error(AsciiArt.roadblock)
                self.log.error("Exception while retrieving flex card info {}".format(exp))
                flexport_app = True
                flexport_err = StatusCodes.FETCH_OPTICS_ERROR.cfp_code + " : " + str(exp)

            self.log.info(f"Flexline card pid {card_pid}")
            if card_pid in root.cisco_zr_cfp__zr.flexport_linecard:
                flexport = root.cisco_zr_cfp__zr.flexport_linecard[card_pid]
                if p in flexport.port:
                    flexport_app = True
                    if (card_pid in locl_conts.a99_flex_cards
                            and mode == dco_zr_ns.cisco_zr_cfp_muxponder_
                            and bandwidth == "300"):
                        flexport_err = (
                            StatusCodes.MODE_AND_BANDWIDTH_NOT_SUPPORTED_ON_THE_MODULE.cfp_code
                            + f" : {mode} mode with {bandwidth} bandwidth is not"
                            f" supported on {card_pid} module")
                    elif ((card_pid in locl_conts.asr9903_flex_cards
                           or card_pid in locl_conts.asr9k_flex_cards)
                          and mode == dco_zr_ns.cisco_zr_cfp_muxponder_
                          and (bandwidth == "200" or bandwidth == "300")):
                        flexport_err = (
                            StatusCodes.MODE_AND_BANDWIDTH_NOT_SUPPORTED_ON_THE_MODULE.cfp_code
                            + f" : {mode} mode with {bandwidth} bandwidth is not"
                            f" supported on {card_pid} module")
                    elif card_pid in locl_conts.nc57_mpa_flex_cards and \
                            ((i != "2" and i != "3") or bandwidth == "200" or bandwidth == "100"):
                        flexport_app = False
                    else:
                        flexport_pid = card_pid
                        flexport_valid = True

        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            self.log.error(traceback.format_exc())
            flexport_app = True
            flexport_pid = None
            flexport_valid = False
            flexport_err = StatusCodes.FETCH_OPTICS_ERROR.cfp_code + " : " + str(exp)

        # Step 4: Check for specific PID, like Rack 0: N540-24Q8L2DD-SYS for Arches
        try:
            entity = f"Rack {port[0]}"
            entities = root.devices.device[router].live_status. \
                Cisco_IOS_XR_invmgr_oper__inventory.entities.entity
            if entity in entities:
                entity_pid = entities[entity].attributes.inv_basic_bag.model_name
                if entity_pid in locl_conts.ncs540_pids:
                    platform_pid_app = True
                    if bandwidth == "300":
                        platform_pid_err = (
                            StatusCodes.MODE_AND_BANDWIDTH_NOT_SUPPORTED_ON_THE_MODULE.cfp_code
                            + f": {bandwidth}g bandwidth is not"
                            + f" supported on {entity_pid} module")
                    else:
                        platform_pid_valid = True
                        platform_pid = entity_pid
        except Exception as exp:
            self.log.error(AsciiArt.roadblock)
            self.log.error("Exception while retrieving platform PID info {}".format(exp))
            platform_pid_app = False
            platform_pid_err = StatusCodes.FETCH_OPTICS_ERROR.cfp_code + " : " + str(exp)

        ## (Applicable - True/False, Valid - True/False, Pid value, error)
        pids = (
            (True, capability_valid, capability_pid, capability_err),
            (flexport_app, flexport_valid, flexport_pid, flexport_err),
            (platform_pid_app, platform_pid_valid, platform_pid, platform_pid_err)
        )
        self.log.info("PIDs : {}".format(pids))
        return pids

    # def check_if_interface_exists(self, root, service):
    #     is_netsim = RonUtils.is_netsim_device(root, service.router)
    #     if is_netsim:
    #         return True

    #     return True
    #     if ('act', 'controller' + service.line_port) in \
    #         root.devices.device[service.router].config.\
    #              Cisco_IOS_XR_ifmgr_cfg__interface_configurations.interface_configuration:
    #         return True
    #     else:
    #         return False

    def get_template_tag(self):
        # To form the template name, i.e. zr-dco-native-flexport-linecard-template
        return "native"

    def zr_dco_self_test(self, uinfo, root, service):
        is_netsim = RonUtils.is_netsim_device(root, service.router)
        ## If netsim return success

        if is_netsim:
            return ("success", None)
        else:
            return ("success", None)

        return ("failed", "Failed to run self-test")

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
                    return ("failed", "No zr-dco status found on device")
            except Exception as e:
                if "Connection reset in new state" in str(e) and retry == (
                    max_retries - 1
                ):
                    self.log.info(
                        "Connection reset on device, reached max-retries for"
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

    def __init__(self, log, root, service):
        self.log = log
        self.root = root
        self.service = service
