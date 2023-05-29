import os
import sys
import _ncs
import ncs.maapi as maapi
import ncs.maagic as maagic

'''
The following try/catch statement for enum library will need to be cleaned up when the minimum
requirements moves to python 3.X. When we move to 3.x we'll need to update the local enum library to
to 3.X. If minimum requirements become 3.4 or greater, then we can just use the standard library
python provides
'''
try:
    # uses standard python libary if enum34 exists, either manually or python version >= 3.4
    from enum import EnumMeta
except ImportError:
    # uses local enum.py for python versions 2.X
    from lib.enum import EnumMeta


class StatusCodeValidator(object):
    def __init__(self, log):
        self.log = log

    def cb_validate(self, tctx, kp, newval):
        try:
            cfp_node = get_cfp_node(self, tctx, str(kp))
            if is_python_enum(cfp_node):
                validate_cfp_status_codes(self, cfp_node)
        except Exception as e:
            self.log.error(e)
            raise
        return _ncs.OK


def is_python_enum(cfp_node):
    return "/" in str(cfp_node.status_code_enum_path)


def get_cfp_node(self, tctx, cfp_kp_str):
    self.log.info("cfpKpStr: " + cfp_kp_str)
    m = maapi.Maapi()
    th = m.attach(tctx)
    return maagic.get_node(th, cfp_kp_str)


def validate_status_codes(self):
    self.log.info("validating all cfp status codes")
    try:
        with maapi.single_read_trans('admin', 'system') as th:
            cfp_nodes = maagic.get_node(th, '/cfp-common-status-codes:status-codes'
                                            '/core-function-pack')
            for cfp_node in cfp_nodes:
                if is_python_enum(cfp_node):
                    validate_cfp_status_codes(self, cfp_node)
    except Exception:
        raise


def validate_cfp_status_codes(self, cfp_node):
    err = ''
    try:
        cfp_status_codes = read_cfp_status_codes(self, cfp_node)
        status_codes = [int(str(key)[1:-1]) for key in cfp_node.status_code.keys()]
        self.log.info("status-codes: " + str(status_codes))
        for code in status_codes:
            if code in cfp_status_codes:
                cfp_status_codes.remove(code)
        if len(cfp_status_codes) > 0:
            err = "Following status codes of core function pack: " + cfp_node.name + \
                  ", are not set: "
            for code in cfp_status_codes:
                err += str(code) + ", "
        if len(err) > 2:
            raise Exception(err[:-2])
    except Exception:
        raise


def read_cfp_status_codes(self, cfp_node):
    cfp_status_codes = []
    try:
        status_enum_path = cfp_node.status_code_enum_path
        path = os.getcwd() + "/state/packages-in-use/1/" + status_enum_path
        if os.path.isfile(path + ".pyc"):
            path = path + ".pyc"
        else:
            path = path + ".py"

        self.log.info("status enum absolute path: " + path)

        if os.path.isfile(path):
            module_name = status_enum_path[status_enum_path.rindex("/") + 1:]
            if sys.version_info[0] == 2:
                self.log.info("python version: 2.x")
                import imp
                if ".pyc" in path:
                    cfp_status_enums = imp.load_compiled(module_name, path)
                else:
                    cfp_status_enums = imp.load_source(module_name, path)
            else:
                self.log.info("python version: 3.x")
                from importlib.machinery import SourceFileLoader, SourcelessFileLoader
                if ".pyc" in path:
                    cfp_status_enums = SourcelessFileLoader(module_name, path).load_module()
                else:
                    cfp_status_enums = SourceFileLoader(module_name, path).load_module()

            clsName = None
            for key in dir(cfp_status_enums):
                if key != 'Enum' and isinstance(getattr(cfp_status_enums, key), EnumMeta):
                    clsName = key
            if clsName:
                statusCodes = getattr(cfp_status_enums, clsName)
                for n in dir(statusCodes):
                    if not(n.startswith("__")):
                        cfp_status_codes.append(getattr(statusCodes, n).value[0])
            else:
                raise Exception("Enum class is not defined in " + module_name)
        else:
            raise Exception("status code enum file: " + path + " does not exists")

        self.log.info("cfp-status-enum-codes: " + str(cfp_status_codes))
    except Exception:
        raise
    return cfp_status_codes
