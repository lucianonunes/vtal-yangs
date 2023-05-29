/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

class CTInfo {
  private String templateName;
  private String type;
  private String deviceVariable;
  private Set<String> variables;

  CTInfo(String templateName) {
    this.templateName = templateName;
  }

  CTInfo(CTInfo ctInfo) {
    this.templateName = ctInfo.getTemplateName();
    this.type = ctInfo.getType();
    this.deviceVariable = ctInfo.getDeviceVariable();
    this.variables = ctInfo.getVariables();
  }

  String getTemplateName() {
    return templateName;
  }

  String getType() {
    return type;
  }

  Set<String> getVariables() {
    return new HashSet<>(variables);
  }

  String getDeviceVariable() {
    return deviceVariable;
  }

  void setDeviceVariable(String deviceVariable) {
    this.deviceVariable = deviceVariable;
  }

  void setType(String type) {
    this.type = type;
  }

  void setVariables(Set<String> variables) {
    this.variables = variables;
  }

  @Override
  public int hashCode() {
    return Objects.hash(templateName);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    CTInfo other = (CTInfo) obj;
    return Objects.equals(templateName, other.templateName);
  }

  @Override
  public String toString() {
    return "CTInfo [templateName=" + templateName + ", type=" + type + ", deviceVariable="
        + deviceVariable + ", variables=" + variables + "]";
  }
}
