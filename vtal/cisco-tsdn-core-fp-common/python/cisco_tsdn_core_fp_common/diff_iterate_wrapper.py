class DiffIterateWrapper(object):
    def __init__(self, diter, **kwargs):
        self.diter = diter
        self.__dict__.update(kwargs)

    def __call__(self, keypath, op, oldv, newv):
        return self.diter(self, keypath, op, oldv, newv)
