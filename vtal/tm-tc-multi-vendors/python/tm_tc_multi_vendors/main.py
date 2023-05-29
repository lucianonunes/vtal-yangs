import ncs

class Main(ncs.application.Application):
    def setup(self):
        self.log.info("tm-tc-multi-vendors RUNNING")

    def teardown(self):
        self.log.info("tm-tc-multi-vendors FINISHED")
