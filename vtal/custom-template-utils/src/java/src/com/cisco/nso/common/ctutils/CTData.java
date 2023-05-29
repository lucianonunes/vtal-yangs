/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import com.tailf.conf.ConfPath;

/**
 * @author abhatta
 *
 */
class CTData {
  private CTInfo ctInfo;
  private String deviceName;
  private Map<String, String> variables;
  private Map<Integer, Map<String, String>> iterations;
  private ConfPath ctxPath;

  CTData(String templateName, String deviceName, ConfPath ctxPath) {
    this.ctInfo = CTInfoFunction.getCTInfo(templateName);
    this.deviceName = deviceName;
    this.ctxPath = ctxPath;
    this.iterations = new HashMap<>();
    this.variables = new HashMap<>();
  }

  CTInfo getCtInfo() {
    return ctInfo;
  }

  String getDeviceName() {
    return deviceName;
  }

  ConfPath getCtxPath() {
    return ctxPath;
  }

  Map<Integer, Map<String, String>> getIterations() {
    return iterations;
  }

  Map<String, String> getVariables() {
    return variables;
  }

  void setCtInfo(CTInfo ctInfo) {
    this.ctInfo = ctInfo;
  }

  void setVariables(Map<String, String> variables) {
    this.variables = variables;
    this.iterations = null;
  }

  void setIterations(Map<Integer, Map<String, String>> iterations) {
    this.iterations = iterations;
    this.variables = null;
  }

  void addIteration(Integer itrNumber, Map<String, String> variables)
      throws CustomTemplateException {
    if (null != this.iterations) {
      this.iterations.put(itrNumber, variables);
      this.variables = null;
    } else {
      throw new CustomTemplateException("CTData iterations is not initialized.");
    }
  }

  void addVariable(String variable, String value) throws CustomTemplateException {
    if (null != this.variables) {
      this.variables.put(variable, value);
      this.iterations = null;
    } else {
      throw new CustomTemplateException("CTData variables is not initialized.");
    }
  }

  @Override
  public int hashCode() {
    return Objects.hash(ctInfo);
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
    CTData other = (CTData) obj;
    return Objects.equals(ctInfo, other.ctInfo);
  }

  @Override
  public String toString() {
    final StringBuilder ctStr = new StringBuilder("[Custom Template: " + ctInfo.getTemplateName());
    ctStr.append(", type: " + ctInfo.getType());
    if (null != variables && !variables.isEmpty()) {
      ctStr.append(" --> {Variables --> ");
      List<String> variableStr = new ArrayList<>();
      for (Map.Entry<String, String> variable : variables.entrySet()) {
        variableStr.add(variable.getKey() + ": " + variable.getValue());
      }
      ctStr.append(String.join(", ", variableStr)).append("}");
    } else if (null != iterations && !iterations.isEmpty()) {
      List<String> itrStrs = new ArrayList<>();
      for (Map.Entry<Integer, Map<String, String>> iteration : iterations.entrySet()) {
        final StringBuilder itrStr = new StringBuilder("{Iteration: " + iteration.getKey());
        Map<String, String> vars = iteration.getValue();
        if (null != vars && !vars.isEmpty()) {
          itrStr.append(" --> (Variables --> ");
          List<String> variableStr = new ArrayList<>();
          for (Map.Entry<String, String> variable : vars.entrySet()) {
            variableStr.add(variable.getKey() + ": " + variable.getValue());
          }
          itrStr.append(String.join(", ", variableStr)).append(")");
        }
        itrStr.append("}");
        itrStrs.add(itrStr.toString());
      }
      ctStr.append(" --> ").append(String.join(", ", itrStrs));
    }
    ctStr.append("]");
    return ctStr.toString();
  }
}
