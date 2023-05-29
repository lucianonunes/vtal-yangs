from abc import ABC, abstractmethod
from core_fp_common.common_utils import get_local_user


class CncService(ABC):
    def __init__(self, path):
        super().__init__()
        self.path = path
        self.service_name = self.get_service_name_from_path(path)
        self.service_key = self.get_service_key_from_path(path)
        self.service_kp = self.get_service_kp(self.service_name)
        self.plan_kp = self.get_plan_kp(self.service_name)
        self.service_xpath = self.get_service_xpath(self.service_name)
        self.internal_plan_path = self.get_internal_plan_path()
        self.plan_path = self.get_plan_path()
        self.username = get_local_user()

    @abstractmethod
    def redeploy(self):
        pass

    @staticmethod
    @abstractmethod
    def get_service_name_from_path(path):
        pass

    @staticmethod
    @abstractmethod
    def get_service_key_from_path(path):
        pass

    @staticmethod
    @abstractmethod
    def get_service_kp(service_name):
        pass

    @staticmethod
    @abstractmethod
    def get_service_xpath(service_name):
        pass

    @staticmethod
    @abstractmethod
    def get_plan_kp(service_name):
        pass

    @staticmethod
    @abstractmethod
    def get_plan_path():
        pass

    @staticmethod
    @abstractmethod
    def get_internal_plan_path():
        pass
