/*
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 * This file has been auto-generated by the confdc compiler.
 * Source: ../load-dir/custom-template-status.fxs
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 */

package com.cisco.nso.common.ctutils.namespaces;

import com.tailf.conf.ConfNamespace;

/** Autogenerated namespace class for YANG module custom-template-status.yang */
public class customTemplateStatus extends ConfNamespace {
    public static final int hash = 298813609;
    public int hash() {
        return customTemplateStatus.hash;
    }

    public static final String id = "http://cisco.com/yang/nso/custom-template-status";
    public String id() {
        return customTemplateStatus.id;
    }

    public static final String uri = "http://cisco.com/yang/nso/custom-template-status";
    public String uri() {
        return customTemplateStatus.uri;
    }

    public String xmlUri() {
        return ConfNamespace.truncateToXMLUri(customTemplateStatus.uri);
    }

    public static final String prefix = "ct-status";
    public String prefix() {
        return customTemplateStatus.prefix;
    }

    public customTemplateStatus() {}

    public static int stringToHash(String str) {
        return ConfNamespace.stringToHash(str);
    }

    public static String hashToString(int hash) {
        return ConfNamespace.hashToString(hash);
    }

    public static final int    _custom_template_status = 1132444590;
    public static final String _custom_template_status_ = "custom-template-status";
    public static final int    _device = 617911018;
    public static final String _device_ = "device";
    public static final int    _status = 1950810920;
    public static final String _status_ = "status";
    public static final int    _owner = 206925960;
    public static final String _owner_ = "owner";
    public static final int    _message = 1674281495;
    public static final String _message_ = "message";
    public static final int    _applied_custom_templates = 2125620922;
    public static final String _applied_custom_templates_ = "applied-custom-templates";
    public static final int    _name = 1998270519;
    public static final String _name_ = "name";
    public static final int    _customization_notif = 1390042093;
    public static final String _customization_notif_ = "customization-notif";
}
