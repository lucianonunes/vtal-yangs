# -*- mode: python; python-indent: 4 -*-
import traceback
import ncs
import _ncs
import ietf_te_fp.utils as Utils
from lsa_utils_pkg.dmap import dm_utils as LsaUtils
from cisco_tsdn_core_fp_common import utils as TsdnUtils
from cisco_tsdn_core_fp_common.status_codes.ietf_te_status_codes import StatusCodes
from cisco_tsdn_core_fp_common.status_codes.ietf_te_base_exception import UserErrorException
from cisco_tsdn_core_fp_common.diff_iterate_wrapper import DiffIterateWrapper
from core_fp_common import instrumentation
import logging
# ------------------------
# SERVICE CALLBACK EXAMPLE
# ------------------------


class IETFTEValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        TsdnUtils.validate_service(self, Utils.ietf_te_validation_callpoint, tctx, kp)


class P2PTeTunnelCallBack(ncs.application.Service):
    @ncs.application.Service.pre_modification
    @instrumentation.instrument_service(logging.INFO, Utils.ietf_servicepoint)
    def cb_pre_modification(self, tctx, op, kp, root, proplist):
        proplist = dict(proplist)
        proplist["VALIDATION_ERROR"] = ""
        try:
            # If op is create, validate status code mapping is loaded
            status_code_cfp = root.cfp_common_status_codes__status_codes.core_function_pack
            if op == _ncs.dp.NCS_SERVICE_CREATE and not status_code_cfp.exists("IETF-TE"):
                raise UserErrorException(
                    self.log, StatusCodes.STATUS_CODE_NOT_LOADED
                ).set_context(
                    "Status Code", "Missing IETF-TE status code mapping"
                ).add_state(
                    "Keypath", str(kp)
                ).finish()

            # If operation is create or update, check interface Loopback on device
            if (op == _ncs.dp.NCS_SERVICE_CREATE) or (op == _ncs.dp.NCS_SERVICE_UPDATE):
                # Get service
                service = ncs.maagic.get_node(ncs.maagic.get_trans(root), kp)
                if (service.te_bandwidth.technology == "generic"
                        and service.te_bandwidth.generic):
                    te_bandwidth = service.te_bandwidth.generic
                    if te_bandwidth.isdigit():
                        bandwidth = int(te_bandwidth)
                        if bandwidth < 1 or bandwidth > 4294967295:
                            raise UserErrorException(
                                self.log, StatusCodes.VALUE_OUT_OF_RANGE
                            ).set_context(
                                "Input Element's Value Range Validation",
                                "TE bandwidth is out of range",
                            ).add_state(
                                "Service", str(service._path)
                            ).add_state(
                                "Valid range", "[1..4294967295]"
                            ).finish()
                    else:
                        raise UserErrorException(
                            self.log, StatusCodes.VALUE_INVALID_FORMAT
                        ).set_context(
                            "Input Element's Value Invalid Format Validation",
                            "TE bandwidth is not a valid number",
                        ).add_state(
                            "Service", str(service._path)
                        ).add_state(
                            "Valid range in number format", "[1..4294967295]"
                        ).finish()

        except Exception as e:
            traceback.print_exc()
            proplist["VALIDATION_ERROR"] = str(e)

        return list(proplist.items())

    @ncs.application.Service.post_modification
    @instrumentation.instrument_service(logging.INFO, Utils.ietf_servicepoint)
    def cb_post_modification(self, tctx, op, kp, root, proplist):

        # If op is update and CQ enabled, mark modified head-end/tail-end in opaque
        if op == _ncs.dp.NCS_SERVICE_UPDATE:
            th = ncs.maagic.get_trans(root)
            service = ncs.maagic.get_node(th, kp)

            updated_devices = self.diff_iterate(th, root, service).updated_devices

            # Check if devices were updated
            if updated_devices:
                # Check if CQ enabled or not for updated head-ends
                device_cq_details = TsdnUtils.is_cq_enabled(self, root, updated_devices,
                                                            th, Utils.is_lsa)

                # Define plan kp
                plan_kp = f"/te/tunnels/tunnel-plan{{{service.name}}}"

                # Update plan
                if th.exists(plan_kp):
                    plan = ncs.maagic.get_node(th, plan_kp).plan
                    update_components = []
                    # Mark components to update and what to update
                    for device, cq_enabled in device_cq_details:
                        # Add head-end/tail-end component
                        if device == service.head_end:
                            update_components.append((plan.component[(
                                "ietf-te-fp-tunnel-nano-plan-services:source",
                                service.source)], cq_enabled))
                        elif device == service.tail_end:
                            update_components.append((plan.component[(
                                "ietf-te-fp-tunnel-nano-plan-services:destination",
                                service.destination)], cq_enabled))
                        else:
                            continue
                        # Add self component
                        update_components.append((plan.component[("ncs:self", "self")],
                                                  cq_enabled))
                    # Update plan
                    for component, cq_enabled in update_components:
                        # If device updated and CQ enabled, set ready state to not-reached
                        if cq_enabled:
                            component.state["ncs:ready"].status = "not-reached"
                        # If device updated and no CQ, update ready state timestamp
                        else:
                            TsdnUtils.update_state_when_timestamp(self, component,
                                                                  component.state["ncs:ready"],
                                                                  "due to no CQ")

    @staticmethod
    def diff_iterate(th, root, service) -> DiffIterateWrapper:
        def diter(self, keypath, op, oldv, newv):
            # Iterate on service diffset to check for updated devices
            # Filter valid LSA service keypaths
            # EX: /ncs:devices/device{rfs-node-1}/config/cisco-rsvp-te-fp:rsvp-te/
            # tunnel-te{IETF-RSVP-TE-111.1.1.1-internal}/signalled-bandwidth/bandwidth
            if Utils.is_lsa:
                # Keypath must be at least length 8 to reach a leaf node
                if len(keypath) < 8:
                    return ncs.ITER_RECURSE
                # Private config must be skipped as it does not represent device config updates
                elif str(keypath[-8]) == "private":
                    return ncs.ITER_CONTINUE
            # Filter valid non-LSA service keypaths
            # EX:  /cisco-rsvp-te-fp:rsvp-te/tunnel-te{IETF-RSVP-TE-111.1.1.1-internal}/
            # signalled-bandwidth/bandwidth
            else:
                # Keypath must be at least length 4 to reach a leaf node
                if len(keypath) < 4:
                    return ncs.ITER_RECURSE
                # Private config must be skipped as it does not represent device config updates
                elif str(keypath[-4]) == "private":
                    return ncs.ITER_CONTINUE

            # Config update in internal RSVP-TE source keypath will result in head device update
            if str(keypath).startswith(self.internal_source_kp):
                self.updated_devices.add(self.service.head_end)
            # Config update in internal RSVP-TE dest keypath will result in tail device update
            elif str(keypath).startswith(self.internal_destination_kp):
                self.updated_devices.add(self.service.tail_end)
            # Config update in internal RSVP-TE explicit path will result in device update
            # EX: /cisco-rsvp-te-fp:rsvp-te/explicit-path/name{IETF-RSVP-TE-PATH-1}
            elif str(keypath).startswith(self.internal_explicit_path_kp):
                # Get RSVP-TE explicit path name
                path_name = str(keypath[0][0])
                # Ensure explicit path name starts with service name
                if path_name.startswith(self.service.name):
                    # Extract IETF-TE path name
                    path_name = path_name[len(self.service.name) + 1:]
                    # Ensure IETF-TE path is associated with service instance
                    if path_name in self.service.p2p_primary_paths.p2p_primary_path:
                        self.updated_devices.add(self.service.head_end)
                        if self.service.bidirectional:
                            self.updated_devices.add(self.service.tail_end)
                            return ncs.ITER_STOP
            return ncs.ITER_CONTINUE

        # Note: We only have to iterate on internal head-end explicit path as explicit path
        # updates will touch head-end (+ tail-end if bidirectional true)
        internal_explicit_path_kp = P2PTeTunnelCallBack\
            ._get_internal_explicit_path_kp(root, service.head_end)
        internal_source_kp = P2PTeTunnelCallBack\
            ._get_internal_service_kp(root, service, service.head_end, service.source)
        internal_destination_kp = P2PTeTunnelCallBack\
            ._get_internal_service_kp(root, service, service.tail_end, service.destination)

        diff_iter = DiffIterateWrapper(diter,
                                       service=service,
                                       updated_devices=set(),
                                       internal_source_kp=internal_source_kp,
                                       internal_destination_kp=internal_destination_kp,
                                       internal_explicit_path_kp=internal_explicit_path_kp)

        # Iterate through diffset to find all updated internal services
        th.keypath_diff_iterate(diff_iter, 0, internal_source_kp)
        if internal_destination_kp is not None:
            th.keypath_diff_iterate(diff_iter, 0, internal_destination_kp)
        th.keypath_diff_iterate(diff_iter, 0, internal_explicit_path_kp)

        return diff_iter

    @staticmethod
    def _get_internal_service_kp(root, service, device, ip):
        # Return internal RSVP-TE service keypath prefix for diff iterate keypath matching
        keypath = f"/cisco-rsvp-te-fp:rsvp-te/tunnel-te{{{service.name}-{ip}-internal}}"
        # If LSA, prepend keypath with rfs device path
        if Utils.is_lsa:
            if device is None:
                return None
            rfs_device = LsaUtils.get_remote_nso(device)
            keypath = f"/ncs:devices/device{{{rfs_device}}}/config{keypath}"
        return keypath

    @staticmethod
    def _get_internal_explicit_path_kp(root, device):
        # Return internal RSVP-TE explicit path keypath prefix for diff iterate keypath matching
        keypath = "/cisco-rsvp-te-fp:rsvp-te/explicit-path"
        # If LSA, prepend keypath with rfs device path
        if Utils.is_lsa:
            rfs_device = LsaUtils.get_remote_nso(device)
            return f"/ncs:devices/device{{{rfs_device}}}/config{keypath}"
        return keypath


class P2PTeSelfTunnel(ncs.application.NanoService):
    """
    NanoService callback handler for ietf-te plan
    """

    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        try:
            new_opaque = None

            if state == "ncs:ready":
                new_opaque = self._create_ready(tctx, root, service, plan,
                                                component, state, opaque, comp_vars)
            if new_opaque is None:
                return opaque

            return new_opaque
        except Exception as e:
            traceback.print_exc()
            opaque_dict["VALIDATION_ERROR"] = str(e)
            return list(opaque_dict.items())

    def _create_ready(self, _tctx, _root, _service, plan,
                      component, state, opaque, _comp_vars):
        # RT-42737 - This callback will not be needed once this RT is fixed.
        state_node = plan.component[component].state[state]
        for plan_component in plan.component:
            if not (plan_component.name == "self" and plan_component.type == "ncs:self"):
                if (plan_component.state["ncs:ready"].status == "failed"
                        or plan_component.state["ncs:init"].status == "failed"):
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready failed, "
                                  "setting self ready state to failed")
                    state_node.status = "failed"
                    return opaque
        for plan_component in plan.component:
            if not (
                plan_component.name == "self" and plan_component.type == "ncs:self"
            ):
                if plan_component.state["ncs:ready"].status == "not-reached":
                    self.log.info(f"Component '{plan_component.name}' state ncs:ready not-reached, "
                                  "setting self ready state to not-reached")
                    state_node.status = "not-reached"
                    return opaque
        return opaque


class P2PTeTunnel(ncs.application.NanoService):
    # The create() callback is invoked inside NCS FASTMAP and
    # must always exist.
    @ncs.application.NanoService.create
    @instrumentation.instrument_nano(logging.INFO, Utils.ietf_servicepoint)
    def cb_nano_create(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        opaque_dict = dict(opaque)
        if opaque_dict.get("VALIDATION_ERROR") != "":
            return list(opaque_dict.items())

        new_opaque = None

        if state == "ietf-te-fp-tunnel-nano-plan-services:config-apply":
            new_opaque = self._create_config_apply(tctx, root, service, plan,
                                                   component, state, opaque, comp_vars)
        elif state == "ncs:ready":
            new_opaque = self._create_ready(tctx, root, service, plan,
                                            component, state, opaque, comp_vars)

        if new_opaque is None:
            return opaque

        return new_opaque

    def template_rsvp_explicit_path(self, root, service, device, id, name, path_name, index,
                                    keyword, hop_type, hop_value, hop_label):
        rsvp_explicit_path_vars = ncs.template.Variables()
        rsvp_explicit_path_template = ncs.template.Template(service)
        rsvp_explicit_path_vars.add("ID", id)
        rsvp_explicit_path_vars.add("DEVICE", device)
        rsvp_explicit_path_vars.add("NAME", name)
        rsvp_explicit_path_vars.add("PATH_NAME", path_name)
        rsvp_explicit_path_vars.add("INDEX", index)
        rsvp_explicit_path_vars.add("KEYWORD", keyword)
        rsvp_explicit_path_vars.add("HOP_TYPE", hop_type)
        rsvp_explicit_path_vars.add("HOP_VALUE", hop_value)
        rsvp_explicit_path_vars.add("HOP_LABEL", hop_label)
        if Utils.is_lsa:
            rsvp_explicit_path_vars.add("RFS_NODE", LsaUtils.get_remote_nso(device))
        rsvp_explicit_path_template.apply("ietf-te-path-copy-internal-cisco-rsvp-te",
                                          rsvp_explicit_path_vars)

    def template_rsvp_tunnel(self, root, service, device, id, description, name, source,
                             destination, bandwidth, path_preference, path_name, metric, hoplimit,
                             costlimit, delaylimit, dynamic, pce, pce_delegation):
        internal_service_name = name + "-" + str(source) + "-internal"
        rsvp_tunnel_vars = ncs.template.Variables()
        rsvp_tunnel_template = ncs.template.Template(service)
        rsvp_tunnel_vars.add("SERVICE_NAME", internal_service_name)
        rsvp_tunnel_vars.add("ID", id)
        rsvp_tunnel_vars.add("DEVICE", device)
        rsvp_tunnel_vars.add("DESCRIPTION", description)
        rsvp_tunnel_vars.add("NAME", name)
        rsvp_tunnel_vars.add("SIGNALLED_NAME", name)
        rsvp_tunnel_vars.add("DESTINATION", destination)
        rsvp_tunnel_vars.add("BANDWIDTH", bandwidth)
        rsvp_tunnel_vars.add("PATH_PREFERENCE", path_preference)
        rsvp_tunnel_vars.add("PATH_NAME", path_name)
        rsvp_tunnel_vars.add("METRIC", metric)
        rsvp_tunnel_vars.add("HOPLIMIT", hoplimit)
        rsvp_tunnel_vars.add("COSTLIMIT", costlimit)
        rsvp_tunnel_vars.add("DELAYLIMIT", delaylimit)
        rsvp_tunnel_vars.add("DYNAMIC", dynamic)
        rsvp_tunnel_vars.add("PCE", pce)
        rsvp_tunnel_vars.add("PCE_DELEGATION", pce_delegation)
        if Utils.is_lsa:
            rsvp_tunnel_vars.add("RFS_NODE", LsaUtils.get_remote_nso(device))
        rsvp_tunnel_template.apply("ietf-te-tunnel-copy-internal-cisco-rsvp-te",
                                   rsvp_tunnel_vars)

    def _create_config_apply(self, tctx, root, service, plan,
                             component, state, opaque, comp_vars):
        comp_vars = dict(comp_vars)
        service_name = service.name
        identifier_tunnel = service.identifier
        if service.source == comp_vars["SOURCE"]:
            source_tunnel = service.source
            destination_tunnel = service.destination
            device = service.head_end
        else:
            source_tunnel = service.destination
            destination_tunnel = service.source
            device = service.tail_end

        if service.description is None or service.description == "None":
            description_tunnel = False
        else:
            description_tunnel = service.description
        if device:
            if service.te_bandwidth.generic:
                bandwidth = int(service.te_bandwidth.generic)
            else:
                bandwidth = False
            paths = None
            pce_delegation = False
            for path in service.p2p_primary_paths.p2p_primary_path:
                paths = True
                explicit_path = False
                dynamic = (pce) = name_path = metric = hoplimit = costlimit = delaylimit = False
                for (hop) in path.explicit_route_objects_always.route_object_include_exclude:
                    index = hop.index
                    name_path = path.name
                    hop_type = hop_value = hop_label = False
                    explicit_route_usage = hop.explicit_route_usage
                    if explicit_route_usage == "te-types:route-exclude-object":
                        if hop.type == "numbered-node-hop":
                            keyword = "exclude-address"
                            hop_value = hop.numbered_node_hop.node_id
                        else:
                            raise UserErrorException(
                                self.log, StatusCodes.VALUE_NOT_SUPPORTED
                            ).set_context(
                                "Input Element's Value Validation",
                                "Input hop type value is not supported",
                            ).add_state(
                                "Service", str(service._path)
                            ).add_state(
                                "Supporting only", "numbered-node-hop"
                            ).finish()
                    else:
                        if hop.type == "numbered-node-hop":
                            explicit_path = True
                            keyword = "next-address"
                            hop_type = hop.numbered_node_hop.hop_type
                            hop_value = hop.numbered_node_hop.node_id
                        elif hop.type == "label" and hop.label_hop.te_label.generic:
                            explicit_path = True
                            keyword = "next-label"
                            try:
                                hop_label = int.from_bytes(hop.label_hop.te_label.generic, "big")
                                # Check range "0..1048575"
                                if hop_label < 0 or hop_label > 1048575:
                                    raise UserErrorException(
                                        self.log, StatusCodes.VALUE_OUT_OF_RANGE
                                    ).set_context(
                                        "Input Element's Value Range Validation",
                                        "TE label in explicit path is out of range",
                                    ).add_state(
                                        "Service", str(service._path)
                                    ).add_state(
                                        "Valid range", "[0..1048575]"
                                    ).finish()
                            except Exception:
                                raise UserErrorException(
                                    self.log, StatusCodes.VALUE_INVALID_FORMAT
                                ).set_context(
                                    "Input Element's Value Invalid Format"
                                    " Validation",
                                    "TE label in the explicit path is not"
                                    " a valid Base64Binary",
                                ).add_state(
                                    "Service", str(service._path)
                                ).add_state(
                                    "Valid range in Base64Binary format", "[0..1048575]"
                                ).finish()
                        else:
                            raise UserErrorException(
                                self.log, StatusCodes.VALUE_NOT_SUPPORTED
                            ).set_context(
                                "Input Element's Value Validation",
                                "Input hop type value is not supported",
                            ).add_state(
                                "Service", str(service._path)
                            ).add_state(
                                "Supporting only", "numbered-node-hop or label"
                            ).finish()

                    if keyword:
                        self.template_rsvp_explicit_path(root, service, device, identifier_tunnel,
                                                         service_name, name_path, index, keyword,
                                                         hop_type, hop_value, hop_label)
                path_preference = path.preference

                # is path locally-computed or PCE-delegated
                if "path-locally-computed" in str(path.path_computation_method):
                    if explicit_path:
                        raise UserErrorException(
                            self.log, StatusCodes.VALUE_NOT_SUPPORTED
                        ).set_context(
                            "Input Element's Value Validation",
                            "Input path computation method value is not supported",
                        ).add_state(
                            "Service", str(service._path)
                        ).add_state(
                            "Invalid input",
                            "path-computation-method should be path-explicitly-defined when "
                            "path is explicitly defined",
                        ).finish()
                    else:
                        pce = False
                        dynamic = True
                elif "path-explicitly-defined" in str(path.path_computation_method):
                    if explicit_path:
                        # Check if an explicit path needs to be configured on the target Device
                        name_path = path.name
                        dynamic = False
                    else:
                        raise UserErrorException(
                            self.log, StatusCodes.MISSING_INPUT
                        ).set_context(
                            "Missing input", "hop-type (numbered-node-hop or label) is mandatory "
                            "when path-computation-method is path-explicitly-defined",
                        ).add_state(
                            "Service", str(service._path)
                        ).add_state(
                            "Missing Input",
                            "hop-type (numbered-node-hop or label) is mandatory when "
                            "path-computation-method is path-explicitly-defined",
                        ).finish()
                elif "path-externally-queried" in str(path.path_computation_method):
                    if explicit_path:
                        raise UserErrorException(
                            self.log, StatusCodes.VALUE_NOT_SUPPORTED
                        ).set_context(
                            "Input Element's Value Validation",
                            "Input path computation method value is not supported",
                        ).add_state(
                            "Service", str(service._path)
                        ).add_state(
                            "Invalid input",
                            "path-computation-method should be path-explicitly-defined when "
                            "path is explicitly defined",
                        ).finish()
                    else:
                        pce = True
                        dynamic = True
                        pce_delegation = True
                else:
                    raise UserErrorException(
                        self.log, StatusCodes.VALUE_NOT_SUPPORTED
                    ).set_context(
                        "Input Element's Value Validation",
                        "Input path computation method value is not supported",
                    ).add_state(
                        "Service", str(service._path)
                    ).add_state(
                        "Supporting only", "path-locally-computed, path-externally-queried or "
                        "path-explicitly-defined",
                    ).finish()

                if dynamic:
                    # if path is dynamic what is the metric type
                    # TODO: workaround there's never several optimization megtric for a given path
                    # TODO: and only one path-selection metric-type per tunnel
                    for metrics in path.optimizations.optimization_metric:
                        metric_type = str(metrics.metric_type)
                        if "te-types:path-metric-igp" == metric_type:
                            path_metric = "igp"
                        elif "te-types:path-metric-te" == metric_type:
                            path_metric = "te"
                        else:
                            path_metric = "delay"

                        if metric:
                            if metric != path_metric:
                                # we raise excpetion for consistency
                                raise UserErrorException(
                                    self.log, StatusCodes.VALUE_NOT_CONSISTENT
                                ).set_context(
                                    "Input element's value is not consistent",
                                    "All TE paths metric type must be the same",
                                ).add_state(
                                    "Service", str(service._path)
                                ).finish()
                        elif path_metric:
                            metric = path_metric

                self.template_rsvp_tunnel(root, service, device, identifier_tunnel,
                                          description_tunnel, service_name, source_tunnel,
                                          destination_tunnel, bandwidth, path_preference,
                                          name_path, metric, hoplimit, costlimit,
                                          delaylimit, dynamic, pce, pce_delegation)
            if paths is None:
                self.template_rsvp_tunnel(root, service, device, identifier_tunnel,
                                          description_tunnel, service_name, source_tunnel,
                                          destination_tunnel, bandwidth, False, False, False,
                                          False, False, False, False, False, False)

        else:
            self.log.error(f"Cannot get device and loopback id for : {source_tunnel}")
        return opaque

    def _create_ready(self, _tctx, root, service, plan,
                      component, state, opaque, comp_vars):

        comp_vars = dict(comp_vars)
        state_node = plan.component[component].state[state]

        source = comp_vars["SOURCE"]
        internal_service_name = comp_vars["INTERNAL_SERVICE_NAME"]

        if service.source == source:
            source_tunnel = service.source
            device = service.head_end
        else:
            source_tunnel = service.destination
            device = service.tail_end

        self.log.info(f"Internal service name {internal_service_name} and "
                      f"source_tunnel {source_tunnel} "
                      f"Device {device}")
        if Utils.is_lsa:
            internal_plan_path = root.cisco_tsdn_core_fp_common__rfs_rsvp_plan
        else:
            internal_plan_path = root.cisco_rsvp_te_fp__rsvp_te.cisco_rsvp_te_fp__tunnel_te_plan

        # Check for complete plan, then for individual component if not reached,
        # set this component to not reached.
        if internal_service_name not in internal_plan_path:
            self.log.info(f"Internal plan for {service.name} doesn't exist, ready is not-reached")
            state_node.status = "not-reached"
            return opaque

        internal_plan = internal_plan_path[internal_service_name].plan

        opaque = dict(opaque)

        # First check if there is plan error in internal, set it
        if internal_plan.failed:
            self.log.info(f"Internal plan for {service.name} is failed, marking ready is failed")
            state_node.status = "failed"
            plan.failed.create()
            if internal_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = internal_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(internal_plan.error_info.message, plan,
                                       component, self.log, device)
            return list(opaque.items())

        # Check if there is commit-queue with this head-end
        if internal_plan.commit_queue:
            if len(internal_plan.commit_queue.queue_item) > 0:
                self.log.info(f"Internal plan for {service.name} has pending queue-item, "
                              "ready is not-reached")
                state_node.status = "not-reached"
                return list(opaque.items())

        # Internal plan should also have self -> ready:reached if there is no failure or pending CQ
        # This is to handle the case where undeploy-no-networking is performed to recover from
        # create failures - in that scenario, internal plan details (component etc.)
        # would be wiped out but only plan entry would remain.
        if ("ncs:self", "self") not in internal_plan.component:
            state_node.status = "not-reached"

        return list(opaque.items())

    @ncs.application.NanoService.delete
    def cb_nano_delete(self, tctx, root, service, plan,
                       component, state, opaque, comp_vars):
        self.log.info(f"Service delete(service={service._path}, state={state})")

        new_opaque = None

        if state == "ncs:init":
            new_opaque = self._delete_init(tctx, root, service, plan,
                                           component, state, opaque, comp_vars)

        if new_opaque is None:
            return opaque

        return new_opaque

    def _delete_init(self, _tctx, root, service, plan,
                     component, state, opaque, comp_vars):

        comp_vars = dict(comp_vars)
        internal_service_name = comp_vars["INTERNAL_SERVICE_NAME"]

        state_node = plan.component[component].state[state]

        # Check the rsvp-te stacked service plan
        # Check if rsvp-te plan is removed, return
        if Utils.is_lsa:
            rsvp_te_plan_path = root.cisco_tsdn_core_fp_common__rfs_rsvp_plan
        else:
            rsvp_te_plan_path = root.cisco_rsvp_te_fp__rsvp_te.tunnel_te_plan

        # Get internal plan if it exists
        try:
            rsvp_te_plan = rsvp_te_plan_path[internal_service_name].plan
        except KeyError:
            self.log.info(f"No RSVP internal plan for: {internal_service_name}")
            # Remove delete status-code-detail augmentation
            # as platform doesn't remove what is created in delete callback
            TsdnUtils.remove_status_code_detail(plan, component)
            return opaque

        # Check if internal service zombie exists
        if Utils.is_lsa:
            rsvp_te_zombie_exists = rsvp_te_plan.zombie_exists
        else:
            rsvp_te_zombie_service = (f"/rsvp-te/tunnel-te[name='{internal_service_name}']")
            rsvp_te_zombie_exists = root.ncs__zombies.service.exists(rsvp_te_zombie_service)

        self.log.info(f"Internal service zombie exists for {internal_service_name}: "
                      f"{rsvp_te_zombie_exists}")

        if rsvp_te_plan.failed and rsvp_te_zombie_exists:
            device = comp_vars["DEVICE"]
            self.log.info(f"rsvp-te_plan {internal_service_name} is failed during delete...")
            state_node.status = "failed"
            if rsvp_te_plan.error_info:
                plan.error_info.create()
                plan.error_info.message = rsvp_te_plan.error_info.message
                # Set Status Code in plan
                Utils.save_status_code(rsvp_te_plan.error_info.message, plan,
                                       component, self.log, device)
        else:
            self.log.info(f"rsvp-te plan {internal_service_name} failure is from create...")
            # Failure might be from create failures:
            # mark the state reached
            # mark the state reached & wait for zombie redeploy if things change in rsvp-te plan
            state_node.status = "reached"

        return opaque
