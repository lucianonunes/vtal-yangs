import collections
import threading


class WorkQueue:
    """
    This is a ordered thread safe set backed by deque. Deques support memory efficient appends
    and pops from either side of the deque with approximately the same O(1) performance in
    either direction.
    """

    instance = None

    def __init__(self):
        self.mutex = threading.Lock()

        # Notify not_empty whenever an item is added to the queue; a
        # thread waiting to get is notified then.
        self.not_empty = threading.Condition(self.mutex)
        self.work_q = collections.deque()
        self.aborted = False
        WorkQueue.instance = self

    def getInstance():
        if WorkQueue.instance is None:
            WorkQueue()
        return WorkQueue.instance

    def put(self, item):
        with self.mutex:
            if item not in self.work_q:
                self.work_q.append(item)
            self.not_empty.notify()

    def pop(self):
        with self.not_empty:
            if not len(self.work_q):
                self.not_empty.wait()
            if not self.aborted:
                item = self.work_q.popleft()
                return item

    def abort(self):
        with self.not_empty:
            self.aborted = True
            self.not_empty.notify_all()

    def get_len(self):
        return len(list(self.work_q))
