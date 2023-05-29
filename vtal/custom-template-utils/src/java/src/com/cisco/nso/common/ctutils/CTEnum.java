/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import java.util.HashSet;
import java.util.Set;

/**
 * @author abhatta
 *
 */
enum CTEnum {
  INSTANCE;

  private boolean isApplyCT = true;
  private Set<CTInfo> ctInfoSet = new HashSet<>();

  CTEnum() {}

  Set<CTInfo> getCtInfoSet() {
    return ctInfoSet;
  }

  boolean isApplyCT() {
    return isApplyCT;
  }

  void setApplyCT(boolean isApplyCT) {
    this.isApplyCT = isApplyCT;
  }
}
