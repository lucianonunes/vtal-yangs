'''
This file need to be copied by the core function pack to it's python package.

@author: abhatta
'''
from xml.dom import minidom
from xml.etree import ElementTree
from xml.etree.ElementTree import Element, SubElement
from xml.sax.saxutils import unescape
import importlib

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


def generate_payload(abs_path, cfp_name):
    '''
    generate_payload function is called to generate status codes xml payload.
    abs_path is the file path of status enum file in cfp, which is module.__file__
    cfp_name is the core function pack name.
    Usage example:
        if __name__ == '__main__':
            StatusCodePayload.generate_payload(CfpStatusCodes.__file__, 'sae')

    In this example CfpStatusCodes.py is the file where status code enums are defined.
    '''
    doc = Element('config', xmlns='http://tail-f.com/ns/config/1.0')
    status_code_cfp = SubElement(doc, 'status-code-cfp',
                                 xmlns='http://com/cisco/cfp-common-status-codes')
    SubElement(status_code_cfp, 'name').text = cfp_name
    status_codes = SubElement(doc, 'status-codes', xmlns='http://com/cisco/cfp-common-status-codes')
    core_fp = SubElement(status_codes, 'core-function-pack')
    SubElement(core_fp, 'name').text = cfp_name
    enum_path, module_name = _get_status_enum_path(abs_path)
    SubElement(core_fp, 'status-code-enum-path').text = enum_path
    enum_list = _get_status_enums(module_name)
    for item in enum_list:
        status_code = SubElement(core_fp, 'status-code')
        SubElement(status_code, 'code').text = str(item[0])
        SubElement(status_code, 'reason').text = str(item[1])
        if (len(item) > 2):
            SubElement(status_code, 'category').text = str(item[2])
            if (len(item) > 3):
                SubElement(status_code, 'severity').text = str(item[3])
                if (len(item) > 4):
                    SubElement(status_code, 'recommended-actions').text = str(item[4])
    _save_pretty(doc, cfp_name)


def _get_status_enum_path(abs_path):
    path_arr = abs_path.split('/')
    arr = path_arr[path_arr.index("python") - 1:]
    file_name = arr[-1]
    module_name = file_name[:file_name.index('.')]
    status_path = '/'.join(arr)
    status_path = status_path[:status_path.index('.')]
    return status_path, module_name


def _get_status_enums(module_name):
    enum_list = []
    cfpStatusCodes = importlib.import_module(module_name)
    clsName = None
    for key in dir(cfpStatusCodes):
        if key != 'Enum' and isinstance(getattr(cfpStatusCodes, key), EnumMeta):
            clsName = key
    if clsName:
        statusCodes = getattr(cfpStatusCodes, clsName)
        for n in dir(statusCodes):
            if not(n.startswith("__")):
                enum_list.append(getattr(statusCodes, n).value)
    else:
        raise Exception("Enum class is not defined in " + module_name)
    return enum_list


def _save_pretty(elem, cfp):
    rough_string = ElementTree.tostring(elem, 'utf-8')
    reparsed = minidom.parseString(rough_string)
    old_data = unescape(reparsed.toprettyxml(indent="  "))
    new_data = old_data[old_data.find("?>") + 3:]
    payload = open("../../" + cfp + "-status-codes.xml", "w")
    payload.write(new_data)
