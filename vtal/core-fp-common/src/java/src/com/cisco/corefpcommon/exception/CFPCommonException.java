/*
 ***************************************************
 * Copyright (c) 2019 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.corefpcommon.exception;

/*
 * CFPCommonException (Core Function Pack Common Exception) represents all the
 * exceptions raised by common package.
 */
public class CFPCommonException extends CoreFunctionPackException {

    /**
     *
     */
    private static final long serialVersionUID = 1723161131166904272L;
    private static final String CFP_NAME = "common";

    public CFPCommonException(String errorCode) {
        super(errorCode, CFP_NAME);
    }

    public CFPCommonException(String message, String errorCode) {
        super(message, errorCode, CFP_NAME);
    }

    public CFPCommonException(String message, String errorCode, Throwable throwable) {
        super(message, errorCode, throwable, CFP_NAME);
    }
}
