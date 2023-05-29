'''
Common methods used in cleanup actions:
  run_action
  invoke_backtrack_actions
  remove_zombie
  delete_service
  remove_service_oper_data
  get_all_side_effect_for_service
  remove_service_side_effects
  get_kicker_with_service
  remove_kickers
  remove_kickers_with_device
  remove_plan_paths
  cleanup_status_codes
'''
import ncs
import _ncs
import ncs.maapi as maapi
from .common_utils import create_cdb_session, end_cdb_session, safe_delete, get_ncs_major_version
import traceback


def run_action(self, service_name, fbt_action, fbt_input):
    """Executes an action and throws an exception if the result was not successful"""
    try:
        output = fbt_action(fbt_input)
        self.log.info(f"Cleanup result for {fbt_action} {service_name} :: {output.result}")
        self.log.debug(f"Cleanup result for {fbt_action} {service_name} :: {output.info}")
        return output
    except Exception as e:
        self.log.error(f"Exception run_action: {str(e)}")
        if "item does not exist" in str(e):
            # Race conditions during cleanup action can cause component to get removed
            # before force backtrack is executed.
            pass
        elif "data_missing" in str(e) and "RPC error" in str(e):
            # To cover the scenario where cleanup is executed even before
            # deleting external service.
            # In case of LSA, this would mean that lower service is removed from RFS NSO but it
            # exists on RFS device in CFS NSO &
            # sending a delete over NETCONF will raise an ALARM
            pass
        else:
            raise e


def invoke_backtrack_actions(self, service_name, back_track_actions,
                             is_no_networking, no_lsa=False):
    """invokes the force-back-track actions"""
    for fbt_action in back_track_actions:
        zombie_path = None
        if type(fbt_action) == tuple:
            zombie_path = fbt_action[1]
            fbt_action = fbt_action[0]
        fbt_input = get_backtrack_action_input(self, fbt_action, is_no_networking, no_lsa)
        output = run_action(self, service_name, fbt_action, fbt_input)

        # In NSO 5.7, calling force-back-track on deleted service will only return
        # an error, the workaround is to call the action on corresponding zombie
        # we need to add the fbt_action and zombie_path for service as tuple to
        # back_track_actions list. RT #48440
        if output and output.info and "Failed: service is not deployed" in str(output.info) \
                and zombie_path is not None:
            try:
                kp = f'/ncs:zombies/ncs:service{{"{zombie_path}"}}' \
                     f'/plan/{fbt_action._path.split("/plan/")[1]}'
                self.log.debug(f"keypath for zombie plan : {kp}")
                th = ncs.maagic.get_trans(fbt_action)
                fbt_zombie = ncs.maagic.get_node(th, kp)
                fbt_input = get_backtrack_action_input(self, fbt_zombie, is_no_networking, no_lsa)
                run_action(self, service_name, fbt_zombie, fbt_input)
            except Exception as e:
                if "item does not exist" in str(e):
                    pass
                elif "not in /ncs:zombies/service" in str(e):
                    pass
                else:
                    raise e


def get_backtrack_action_input(self, action, is_no_networking, no_lsa):
    fbt_input = action.get_input()
    if is_no_networking:
        fbt_input.no_networking.create()
    if no_lsa and get_ncs_major_version() > 5.3:
        fbt_input.no_lsa.create()
    return fbt_input


def remove_zombie(self, zombie_service_path, cleanup_log, uinfo):
    """Checks for any remaining zombie services and removes them if there"""
    kp = f'/ncs:zombies/ncs:service{{"{zombie_service_path}"}}'
    sock_cdb_oper = create_cdb_session()
    try:
        self.log.info(f"removing zombie service: {kp}")
        cleanup_log.append(f"\n Removing zombie service: {kp}")
        safe_delete(sock_cdb_oper, kp)
        cleanup_log.append("\n Removed zombie service")
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)


def delete_service(self, service_path, cleanup_log, uinfo, commit_params=None):
    """Deletes the northbound service """
    self.log.info(f"Deleting service {service_path}")
    with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
        if th.exists(service_path):
            cleanup_log.append(f"\n Removing service {service_path}")
            try:
                th.delete(service_path)
                if commit_params:
                    th.apply_params(False, commit_params)
                else:
                    th.apply(False)
            except KeyError:
                pass
            cleanup_log.append(f"\n Removed service {service_path}")


def remove_service_oper_data(self, service_oper_paths, cleanup_log, uinfo):
    """Remove all service oper paths"""
    sock_cdb_oper = create_cdb_session()
    try:
        for service_oper_path in service_oper_paths:
            self.log.info(f"removing service oper: {service_oper_path}")
            cleanup_log.append(f"\n Removing service oper: {service_oper_path}")
            safe_delete(sock_cdb_oper, service_oper_path)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)
    cleanup_log.append("\n Removed service oper")


def remove_service_side_effects(self, side_effects, cleanup_log, uinfo):
    """Remove all service side-effect queue"""
    sock_cdb_oper = create_cdb_session()
    try:
        for side_effect_path in side_effects:
            kp = f'/ncs:side-effect-queue/side-effect{{{side_effect_path}}}'
            self.log.info(f"removing side-effect queue: {kp}")
            cleanup_log.append(f"\n Removing side-effect queue: {kp}")
            safe_delete(sock_cdb_oper, kp)
    except Exception:
        traceback.print_exc()
    end_cdb_session(sock_cdb_oper)
    cleanup_log.append("\n Removed side-effects")


def remove_kickers(self, kickers, cleanup_log, uinfo):
    """Remove all service kickers"""
    with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
        for kicker_id in kickers:
            kp = f'/kickers/data-kicker{{"{kicker_id}"}}'
            if th.exists(kp):
                self.log.info(f"removing kickers: {kp}")
                cleanup_log.append(f"\n Removing kicker: {kp}")
                th.delete(kp)
        th.apply()
        cleanup_log.append("\n Removed kickers")


def remove_kickers_with_device(self, kickers, device, cleanup_log, uinfo):
    """Remove all service kickers with given device"""
    with maapi.single_write_trans(uinfo.username, "system", db=ncs.RUNNING) as th:
        for kicker_id in kickers:
            if device in kicker_id:
                kp = f'/kickers/data-kicker{{"{kicker_id}"}}'
                if th.exists(kp):
                    self.log.info(f"removing kickers: {kp}")
                    cleanup_log.append(f"\n Removing kicker: {kp}")
                    th.delete(kp)
        th.apply()
        cleanup_log.append("\n Removed kickers")


def get_all_side_effect_for_service(self, service_path):
    side_effects = []
    with ncs.maapi.single_read_trans("", "system", db=ncs.OPERATIONAL) as trans:
        qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                    f'/ncs:side-effect-queue/side-effect[service="{service_path}"]',
                                    '/', 0, 1,
                                    _ncs.QUERY_STRING, ["id"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)

        for r in res:
            side_effects.append(r[0])

        _ncs.maapi.query_stop(trans.maapi.msock, qh)
    return side_effects


def get_kicker_with_service(self, service_path):
    kickers = []
    with ncs.maapi.single_read_trans("", "system", db=ncs.RUNNING) as trans:
        qh = _ncs.maapi.query_start(trans.maapi.msock, trans.th,
                                    f'/kickers/data-kicker/variable[value="{service_path}"]',
                                    '/', 0, 1,
                                    _ncs.QUERY_STRING, ["../id"], [])

        res = _ncs.maapi.query_result(trans.maapi.msock, qh)

        for r in res:
            kickers.append(r[0])

        _ncs.maapi.query_stop(trans.maapi.msock, qh)
    return kickers


def remove_plan_paths(self, remove_plan_path, cleanup_log, uinfo):
    """Checks for any remaining plan paths and remove"""
    if remove_plan_path is not None:
        with maapi.single_write_trans(uinfo.username, "system", db=ncs.OPERATIONAL) as th:
            if th.exists(remove_plan_path):
                self.log.info(f"removing plan path: {remove_plan_path}")
                cleanup_log.append(f"\n Removing plan path: {remove_plan_path}")
                try:
                    th.delete(remove_plan_path)
                    th.apply()
                except KeyError:
                    pass
                except Exception as e:
                    self.log.error(f"Exception remove_plan_paths: {str(e)}")
                    if "item does not exist" in str(e):
                        # Race conditions during cleanup action can cause path to get removed
                        # before the transaction is applied
                        pass
                    else:
                        raise e
                self.log.info(f"removed plan path: {remove_plan_path}")
                cleanup_log.append("\n Removed plan path")


def cleanup_status_codes(self, status_code_paths, cleanup_log):
    """Checks for any remaining status codes and removes them if there"""
    sock_cdb_oper = create_cdb_session()

    for status_code_path in status_code_paths:
        try:
            self.log.info(f"removing status-code-detail: {status_code_path}")
            cleanup_log.append(f"\n Removing status-code-detail: {status_code_path}")
            safe_delete(sock_cdb_oper, status_code_path)
            cleanup_log.append("\n Removed status-code-detail")
        except Exception:
            traceback.print_exc()
    end_cdb_session(sock_cdb_oper)
