/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

/**
 * @author abhatta
 *
 */
@SuppressWarnings("serial")
class CustomTemplateException extends Exception {

  CustomTemplateException(String message) {
    super(message);
  }

  CustomTemplateException(Throwable cause) {
    super(cause);
  }

  CustomTemplateException(String message, Throwable cause) {
    super(message, cause);
  }
}
