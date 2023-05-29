from . import NativeXR


class UMXR(NativeXR.NativeXR):
    def get_template_tag(self):
        # To form the template name, i.e. zr-dco-um-flexport-linecard-template
        return "um"
