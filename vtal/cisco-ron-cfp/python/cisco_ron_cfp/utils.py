# -*- mode: python; python-indent: 4 -*-
import traceback
import json

from cisco_ron_core_fp_common.constants import AsciiArt, StrConstants
from cisco_ron_core_fp_common.status_codes import StatusCodes, get_status_by_code
from lsa_utils_pkg.dmap.dm_utils import get_device_remote_nso, get_remote_nso, get_all_remote_nso

from .ron_ml_errors import RonMLException, get_status_code_class, UserError
from .namespaces.ciscoRonCfp_ns import ns as roncfp_ns


def is_lsa(root):
    try:
        ## If this path exists, it is not LSA
        root.cisco_zr_cfp__zr
        return False
    except Exception:
        return True


def get_pid_info(root, service, end_point, log):
    capability_pid = None
    capability_valid = False

    fec = bandwidth = modulation = baud_rate = transmit_power = dac_rate = None
    try:
        ## Get pluggable PID of this controller from optics operational to match capacity
        ##   using Cisco-IOS-XR-controller-optics-oper
        pid = None
        ols_domain = None
        try:
            ols_domain = end_point.ols_domain
            if ols_domain:
                pid = ols_domain.transceiver_capability
        except Exception as e:
            ## Sometimes fetching nonexistent node results in exception
            if "item does not exist" not in str(e):
                raise e

        if pid is None:
            ## Pluggable PID from router through cisco-zr-cfp
            log.debug("Fetching Pluggable PID from operational data")
            end_point_data = root.cisco_ron_cfp__ron.ron_ml[
                service.name
            ].ron_data.end_point[end_point.end_point_device]
            if end_point_data.transceiver_pid:
                pid = end_point_data.transceiver_pid

        if (
            pid,
            service.mode,
            service.bandwidth,
        ) in root.cisco_ron_cfp__ron.transceiver_capability:
            capability = root.cisco_ron_cfp__ron.transceiver_capability[
                pid, service.mode, service.bandwidth
            ]

            capability_pid = pid
            capability_valid = True

            # Bandwidth / service parameter
            bandwidth = service.bandwidth

            ## transmit_power / Service Parameter or capability table
            transmit_power = end_point.terminal_device_optical.transmit_power
            if transmit_power is None:
                transmit_power = capability.transmit_power

            ## Fec / capability table
            fec = capability.fec
            ## Modulation / capability table
            modulation = capability.modulation.as_list()[0]
            ## Baud-rate / capability table
            baud_rate = capability.baud_rate.as_list()[0]
            ## Dac-rate / capability table
            try:
                if service.dac_rate:
                    dac_rate = service.dac_rate
            except Exception as e:
                ## Sometimes fetching nonexistent node results in exception
                if "item does not exist" not in str(e):
                    raise e
            if dac_rate is None:
                dac_rate = capability.dac_rate.as_list()[0]
            elif dac_rate not in capability.dac_rate:
                raise UserError(
                    log, StatusCodes.PLUGGABLE_CAPABILITY_NOT_SUPPORTED
                ).set_context(
                    f"pid {pid} is not supported with dac-rate {dac_rate} "
                    f"and bandwidth {bandwidth}"
                ).add_state(
                    "service", service.name
                ).finish()
        else:
            log.warning(
                f"Cannot identify PID for the endpoint {end_point.end_point_device} :: key "
                f"({pid}, {service.mode}, {service.bandwidth})"
            )
            capability_pid = None
            capability_valid = False
    except UserError:
        raise
    except Exception as exp:
        log.error(AsciiArt.roadblock)
        log.error(traceback.format_exc())
        raise RonMLException(
            log, StatusCodes.INTERNAL_DATA_READ_ERR, str(exp)
        ).set_context(
            "oper-data read", "Failed to read/identify endpoint oper data"
        ).add_state(
            "service", service._path
        ).finish()

    ## (Valid - True/False, Pid value), ...)
    pid = (
        capability_valid,
        capability_pid,
        fec,
        bandwidth,
        modulation,
        baud_rate,
        dac_rate,
        transmit_power,
    )
    log.debug("PID is {}".format(pid))
    return pid


def check_for_cq(th, root, routers):
    result = []
    ## Commit flag has CQ
    cp = th.get_params()
    if (roncfp_ns.cisco_ron_cfp_commit_queue_ in str(cp)) and (
        roncfp_ns.cisco_ron_cfp_bypass_ not in str(cp)
    ):
        return [(router, "True") for router in routers]
    elif (roncfp_ns.cisco_ron_cfp_commit_queue_ in str(cp)) and (
        roncfp_ns.cisco_ron_cfp_bypass_ in str(cp)
    ):
        return [(router, "False") for router in routers]

    ## Committed without CQ flags, check global & device level settings
    if is_lsa(root):
        ## Get RFS Node to device mapping
        device_rfs_map = get_device_remote_nso(routers)
        for rfs_node, rfs_node_devices in device_rfs_map.items():
            rfs_node_ls = root.ncs__devices.device[rfs_node].live_status
            get_device_cq_details(rfs_node_ls, result, rfs_node_devices)
    else:
        get_device_cq_details(root, routers, result)

    return result


def get_device_cq_details(root, routers, result):
    ## Global CQ
    global_cq_enabled = "False"
    if root.ncs__devices.global_settings.commit_queue.enabled_by_default:
        global_cq_enabled = "True"
    ## Per device CQ
    for router in routers:
        per_device_cq = root.ncs__devices.device[router].commit_queue.enabled_by_default
        if per_device_cq is None:
            result.append((router, global_cq_enabled))
        elif per_device_cq:
            result.append((router, "True"))
        else:
            result.append((router, "False"))


def eval_dict(dict_repr):
    dict_repr = dict_repr.replace("'", '"')
    return json.loads(dict_repr)


def save_to_status_code_detail(error_msg, plan, component, log, device=None,):
    # populate plan with status code details
    status_code = None
    if error_msg:
        if "out of sync" in error_msg:
            status_code = StatusCodes.DEVICE_OUT_OF_SYNC
        elif any(error_type in str(error_msg).lower()
                 for error_type in StrConstants.transient_error_message):
            status_code = StatusCodes.CONNECTION_FAILURE
        elif plan.component[component].status_code:
            code = plan.component[component].status_code.split('-')[1]
            status_code = get_status_by_code(code)
        else:
            status_code = StatusCodes.CONFIG_FAILURE
    else:
        code = plan.component[component].status_code.split('-')[1]
        status_code = get_status_by_code(code)
        error_msg = ''

    log.info(f"status_code : {status_code}")
    status_class = get_status_code_class(status_code.value)
    exp = status_class(log, status_code, error_msg)
    exp = exp.set_context(exp.statusCode.reason, error_msg).finish()
    exp.save_to_plan(plan, component, device)


def safe_get_remote_nso(log, device):
    nso = get_remote_nso(device)
    if nso is None and device in get_all_remote_nso():
        raise UserError(log, StatusCodes.INVALID_DEVICE_ERROR).set_context(
            "Invalid device",
            f"Device {device} is a NSO node",
        ).add_state("Device", device).finish()
    return nso
