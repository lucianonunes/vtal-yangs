import ncs

# ---------------------------------------------
# COMPONENT THREAD THAT WILL BE STARTED BY NCS.
# ---------------------------------------------


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("sr-te-multi-vendors RUNNING")

    def teardown(self):
        self.log.info("sr-te-multi-vendors FINISHED")
