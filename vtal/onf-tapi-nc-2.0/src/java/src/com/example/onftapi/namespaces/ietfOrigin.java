/*
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 * This file has been auto-generated by the confdc compiler.
 * Source: ../load-dir/ietf-origin.fxs
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 */

package com.example.onftapi.namespaces;

import com.tailf.conf.ConfNamespace;

/** Autogenerated namespace class for YANG module ietf-origin.yang */
public class ietfOrigin extends ConfNamespace {
    public static final int hash = 394060208;
    public int hash() {
        return ietfOrigin.hash;
    }

    public static final String id = "_onf-tapi-nc-2.0:onf-tapi-nc-2.0#urn:ietf:params:xml:ns:yang:ietf-origin";
    public String id() {
        return ietfOrigin.id;
    }

    public static final String uri = "_onf-tapi-nc-2.0:onf-tapi-nc-2.0#urn:ietf:params:xml:ns:yang:ietf-origin";
    public String uri() {
        return ietfOrigin.uri;
    }

    public String xmlUri() {
        return ConfNamespace.truncateToXMLUri(ietfOrigin.uri);
    }

    public static final String prefix = "or";
    public String prefix() {
        return ietfOrigin.prefix;
    }

    public ietfOrigin() {}

    public static int stringToHash(String str) {
        return ConfNamespace.stringToHash(str);
    }

    public static String hashToString(int hash) {
        return ConfNamespace.hashToString(hash);
    }

    public static final int    _dynamic = 1339207079;
    public static final String _dynamic_ = "dynamic";
    public static final int    _system = 1534086422;
    public static final String _system_ = "system";
    public static final int    _default = 515137925;
    public static final String _default_ = "default";
    public static final int    _origin = 2002579816;
    public static final String _origin_ = "origin";
    public static final int    _learned = 1220332201;
    public static final String _learned_ = "learned";
    public static final int    _unknown = 966572115;
    public static final String _unknown_ = "unknown";
    public static final int    _intended = 270744734;
    public static final String _intended_ = "intended";
}
