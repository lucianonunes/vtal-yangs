import collections
import threading


class SynchronizedSet:
    """
    This is a ordered thread safe set backed by deque. Deques support memory efficient appends
    and pops from either side of the deque with approximately the same O(1) performance in
    either direction.
    """

    def __init__(self):
        self.mutex = threading.Lock()

        # Notify not_empty whenever an item is added to the queue; a
        # thread waiting to get is notified then.
        self.not_empty = threading.Condition(self.mutex)
        self.deck = collections.deque()
        self.aborted = False

    def put(self, item):
        with self.mutex:
            if item not in self.deck:
                self.deck.append(item)
            self.not_empty.notify()

    def get(self):
        with self.not_empty:
            if not len(self.deck):
                self.not_empty.wait()
            if not self.aborted:
                item = self.deck.popleft()
                return item

    def abort(self):
        with self.not_empty:
            self.aborted = True
            self.not_empty.notify_all()
