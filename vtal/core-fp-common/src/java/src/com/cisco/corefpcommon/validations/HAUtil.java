// **************************************************
// Copyright (c) 2017 Cisco Systems, Inc.
// All rights reserved.
// **************************************************
package com.cisco.corefpcommon.validations;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.ConfEnumeration;
import com.tailf.maapi.Maapi;

public class HAUtil {

    private HAUtil() {
        // private empty constructor
    }

    private static final Logger UTIL_LOGGER = LogManager.getLogger(HAUtil.class);
    private static String NCS_STATE_HA_MODE = "/tfnm:ncs-state/ha/mode";

    public static boolean isHaSlave(Maapi maapi, int th) {
        try {
            // based on NFVO is_ha_master_or_no_ha()
            if (maapi.exists(th, "/tfnm:ncs-state/ha")) {
                ConfEnumeration haModeEnum = (ConfEnumeration) maapi.getElem(th, NCS_STATE_HA_MODE);
                String haMode = ConfEnumeration.getLabelByEnum(NCS_STATE_HA_MODE, haModeEnum);
                UTIL_LOGGER.info("isHaSlave(): ha_mode = " + haMode);
                return "master".equals(haMode) ? false : true;
            } else {
                return false;
            }
        } catch (Exception e) {
            UTIL_LOGGER.error("Exception during isHaSlave : " + e.getMessage(), e);
        }
        return false;
    }

    public static String getHaMode(Maapi maapi, int th) {
        try {
            if (!maapi.exists(th, "/tfnm:ncs-state/ha")) {
                return "none";
            }
            ConfEnumeration haModeEnum = (ConfEnumeration) maapi.getElem(th, NCS_STATE_HA_MODE);
            String haMode = ConfEnumeration.getLabelByEnum(NCS_STATE_HA_MODE, haModeEnum);
            UTIL_LOGGER.info("getHaMode(): ha_mode = " + haMode);
            return haMode;
        } catch (Exception e) {
            UTIL_LOGGER.error("Exception during getHaMode : " + e.getMessage(), e);
        }
        return "none";
    }
}
