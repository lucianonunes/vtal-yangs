import concurrent.futures

from .rfs_path_queue import WorkQueue as WorkQueue
from threading import Thread
from . import utils as Utils
from .flat_L2vpn import FlatL2vpn
from .flat_L3vpn import FlatL3vpn
from .sr_te_odn import SrTeOdn
from .ietf_te import IetfTe
from .sr_te_policy import SrTePolicy
from .sr_te_cfp_rfs_change_handler import handle_odn_template_plan, handle_policy_template_plan
from .flat_L3vpn_fp_rfs_change_handler import handle_L3vpn_plan
from .ietf_te_fp_rfs_change_handler import handle_ietf_plan
from .flat_L2vpn_fp_rfs_change_handler import handle_flat_L2VPN_plan
import traceback


class PlanChangeExecutor(Thread):
    """
    Threadpool executor to handle plan changes
    """

    def __init__(self, log, max_threads=5):
        Thread.__init__(self)
        self.log = log
        self.max_threads = max_threads
        self.currently_handling = set()
        self.work_queue = WorkQueue.getInstance()

    def run(self):
        with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_threads) as executor:
            self.log.info("PlanChangeExecutor Started")

            # Schedule threads as the data becomes available from queue
            futures = {
                executor.submit(self.RfsPathChangeHandler, changed_path, rfs_node)
                for (changed_path, rfs_node) in self.get_next_batch(self.max_threads)
            }

            while futures:
                # The function will return when any future finishes or is cancelled.
                done, futures = concurrent.\
                    futures.wait(futures, return_when=concurrent.futures.FIRST_COMPLETED)

                # Remove the handled changed_path from currently handling to take more requests
                for future in done:
                    self.log.debug(f"Remove {future.result()} from currently_handling")
                    self.currently_handling.remove(future.result())

                # Reschedule freed up threads for more work
                free_workers = (self.max_threads) - len(list(self.currently_handling))
                for (changed_path, rfs_node) in self.get_next_batch(free_workers):
                    futures.add(executor.submit(self.RfsPathChangeHandler, changed_path, rfs_node))

    def get_next_batch(self, free_workers):
        changed_paths = set()
        work_queue_size = self.work_queue.get_len()

        # If work queue has less items than available workers, assign all & start processing
        # If work queue is empty, wait for items to become available
        if work_queue_size < free_workers:
            if work_queue_size == 0:
                free_workers = 1
            else:
                free_workers = work_queue_size

        for i in range(free_workers):
            (new_change_path, rfs_node) = self.work_queue.pop()
            if (new_change_path, rfs_node) not in self.currently_handling:
                changed_paths.add((new_change_path, rfs_node))
                self.currently_handling.add((new_change_path, rfs_node))
            else:
                # Put it back in the queue if already being handled
                # TODO - this would make the entry go back to end of queue,
                # should we insert in middle for fairer approach?
                self.work_queue.put((new_change_path, rfs_node))

        self.log.info(f"Next batch to process: {changed_paths}")
        return changed_paths

    def RfsPathChangeHandler(self, changed_path, rfs_node):
        self.log.info(f"RfsPathChangeHandler starting for: {changed_path} from {rfs_node}")
        try:
            service_wrapper, handle_plan = self.get_service_and_plan_handler(changed_path)

            if Utils.check_service_cleanup_flag(self.log, service_wrapper.service_kp,
                                                service_wrapper.username):
                self.log.info(f"Cleanup flag set for {service_wrapper.service_kp}. "
                              "Will not handle.")
                return changed_path, rfs_node

            handle_plan(changed_path, rfs_node, self.log)

            if service_wrapper:
                self.redeploy_zombie_or_service(service_wrapper)

        except Exception as e:
            self.log.error(f"RfsPathChangeHandler failed for {changed_path}: {e}")
            traceback.print_exc()
            pass

        self.log.info(f"RfsPathChangeHandler finished for: {changed_path} from {rfs_node}")

        return changed_path, rfs_node

    def get_service_and_plan_handler(self, changed_path):
        service_wrapper = None
        handle_plan = None
        if "odn-template-plan" in changed_path:
            service_wrapper = SrTeOdn(changed_path, self.log)
            handle_plan = handle_odn_template_plan
        if "policy-plan" in changed_path:
            service_wrapper = SrTePolicy(changed_path, self.log)
            handle_plan = handle_policy_template_plan
        if "flat-L3vpn-plan" in changed_path:
            service_wrapper = FlatL3vpn(changed_path, self.log)
            handle_plan = handle_L3vpn_plan
        if "tunnel-te-plan" in changed_path:
            service_wrapper = IetfTe(changed_path, self.log)
            handle_plan = handle_ietf_plan
        if "flat-L2vpn-plan" in changed_path:
            service_wrapper = FlatL2vpn(changed_path, self.log)
            handle_plan = handle_flat_L2VPN_plan

        return service_wrapper, handle_plan

    def redeploy_zombie_or_service(self, service_wrapper):
        # Redeploy service/zombie
        # If cleanup is in progress, do not take any action
        if Utils.check_service_cleanup_flag(self.log, service_wrapper.service_kp,
                                            service_wrapper.username):
            self.log.info(f"Cleanup flag set for {service_wrapper.service_kp}. Will not re-deploy.")
            return

        try:
            service_wrapper.redeploy()
        except Exception as e:
            self.log.error(f"Failed to re-deploy service {service_wrapper.service_kp}: {e}")
            traceback.print_exc()
