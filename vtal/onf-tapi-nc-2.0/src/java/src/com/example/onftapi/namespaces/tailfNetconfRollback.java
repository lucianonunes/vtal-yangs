/*
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 * This file has been auto-generated by the confdc compiler.
 * Source: ../load-dir/tailf-netconf-rollback.fxs
 * BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE BEWARE
 */

package com.example.onftapi.namespaces;

import com.tailf.conf.ConfNamespace;

/** Autogenerated namespace class for YANG module tailf-netconf-rollback.yang */
public class tailfNetconfRollback extends ConfNamespace {
    public static final int hash = 1704065176;
    public int hash() {
        return tailfNetconfRollback.hash;
    }

    public static final String id = "_onf-tapi-nc-2.0:onf-tapi-nc-2.0#http://tail-f.com/ns/netconf/rollback";
    public String id() {
        return tailfNetconfRollback.id;
    }

    public static final String uri = "_onf-tapi-nc-2.0:onf-tapi-nc-2.0#http://tail-f.com/ns/netconf/rollback";
    public String uri() {
        return tailfNetconfRollback.uri;
    }

    public String xmlUri() {
        return ConfNamespace.truncateToXMLUri(tailfNetconfRollback.uri);
    }

    public static final String prefix = "netconf-rollback";
    public String prefix() {
        return tailfNetconfRollback.prefix;
    }

    public tailfNetconfRollback() {}

    public static int stringToHash(String str) {
        return ConfNamespace.stringToHash(str);
    }

    public static String hashToString(int hash) {
        return ConfNamespace.hashToString(hash);
    }

    public static final int    _rollback_label = 1809663520;
    public static final String _rollback_label_ = "rollback-label";
    public static final int    _rollback_comment = 724872097;
    public static final String _rollback_comment_ = "rollback-comment";
}
