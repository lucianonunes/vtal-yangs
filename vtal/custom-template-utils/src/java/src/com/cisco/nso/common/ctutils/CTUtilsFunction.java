/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.CT_APPLIED_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TREE_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.INP_ERR_MSG;
import static com.cisco.nso.common.ctutils.CTConstants.MSG_FLAG_OFF;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.namespaces.customTemplate;
import com.cisco.nso.common.ctutils.namespaces.customTemplateStatus;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfPath;
import com.tailf.dp.services.ServiceContext;
import com.tailf.maapi.Maapi;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;

/**
 *
 * @author abhatta
 *
 */
class CTUtilsFunction {
  private static Logger LOGGER = LogManager.getLogger(CTUtilsFunction.class);

  private CTUtilsFunction() {}

  static boolean applyCustomTemplates(ServiceContext context, String deviceName, String keyPath,
      Map<String, String> extraVariables) throws ConfException {
    LOGGER.info("##################### Custom Template triggered for keyPath: " + keyPath
        + " #####################");
    boolean success = true;
    try {
      if (validateInputs(context, deviceName, keyPath)) {
        NavuNode ncsRoot = context.getRootNode();
        NavuList customTemplates = (NavuList) ncsRoot.getNavuNode(new ConfPath(keyPath));
        for (NavuNode customTemplateNode : customTemplates) {
          processCustomTemplate(context, customTemplateNode, deviceName, null, extraVariables);
        }
      } else {
        success = false;
        LOGGER.info(MSG_FLAG_OFF + " Reverting all custom templates which are applied to device: "
            + deviceName);
      }
    } catch (CustomTemplateException e) {
      LOGGER.error("", e);
      throw new ConfException(e.getMessage());
    }
    return success;
  }

  static boolean applyCustomTemplate(ServiceContext context, String deviceName, String keyPath,
      Map<String, String> extraVariables) throws ConfException {
    LOGGER.info("##################### Custom Template triggered for keyPath: " + keyPath
        + " #####################");
    boolean success = true;
    try {
      if (validateInputs(context, deviceName, keyPath)) {
        NavuNode ncsRoot = context.getRootNode();
        NavuNode customTemplateNode = ncsRoot.getNavuNode(new ConfPath(keyPath));
        processCustomTemplate(context, customTemplateNode, deviceName, null, extraVariables);
      } else {
        success = false;
        LOGGER.info(MSG_FLAG_OFF + " Reverting the custom template which is applied to device: "
            + deviceName);
      }
    } catch (CustomTemplateException e) {
      LOGGER.error("", e);
      throw new ConfException(e.getMessage());
    }
    return success;
  }

  static boolean applyTemplate(ServiceContext context, String deviceName, String templateName,
      Map<String, String> variables) throws ConfException {
    boolean success = true;
    try {
      if (validateInputs(context, deviceName, templateName)) {
        Maapi maapi = context.getRootNode().context().getMaapi();
        int tid = context.getRootNode().context().getMaapiHandle();
        ConfPath contextPath = getContextPath(maapi, tid, context.getServiceNode(), null);
        CTData data = new CTData(templateName, deviceName, contextPath);
        if (null == data.getCtInfo()) {
          data.setCtInfo(CTInfoFunction.createCTInfo(maapi, tid, templateName));
        }
        Set<String> ctVariables = data.getCtInfo().getVariables();
        if (!ctVariables.isEmpty()) {
          populateTemplateIteration(data, CTUtilities.processNullMap(variables), ctVariables);
        }

        LOGGER.info("########## START applying template: " + templateName + " ##########");
        LOGGER.debug(data);
        CTFunction.applyCustomTemplate(maapi, tid, data);
        LOGGER.info("########## END applying template: " + templateName + " ##########");
      } else {
        success = false;
        LOGGER.info(MSG_FLAG_OFF + " Reverting custom template: " + templateName + " to device: "
            + deviceName + ", if applied.");
      }
    } catch (CustomTemplateException e) {
      throw new ConfException(e.getMessage());
    }
    return success;
  }

  static void processCustomTemplate(ServiceContext context, NavuNode customTemplateNode,
      String deviceName, String ctxPath, Map<String, String> extraVariables)
      throws CustomTemplateException, ConfException {
    Maapi maapi = context.getRootNode().context().getMaapi();
    int tid = context.getRootNode().context().getMaapiHandle();
    String ctName = customTemplateNode.leaf(customTemplate._name_).valueAsString();
    ConfPath contextPath = getContextPath(maapi, tid, context.getServiceNode(), ctxPath);

    CTData ctData = createTemplateData(customTemplateNode, ctName, deviceName, contextPath,
        CTUtilities.processNullMap(extraVariables));
    LOGGER.info("########## START Applying custom template: " + ctName + " ##########");
    LOGGER.debug(ctData);
    CTFunction.applyCustomTemplate(maapi, tid, ctData);
    LOGGER.info("########## END Applying custom template: " + ctName + " ##########");
  }

  static void createAppliedStatus(NavuNode ncsRoot, String templateName, String deviceName,
      String owner) throws CustomTemplateException {
    try {
      ncsRoot.getParent().container(customTemplateStatus.hash)
          .list(customTemplateStatus._applied_custom_templates_)
          .safeCreate(new String[] {templateName, deviceName, owner});
    } catch (ConfException e) {
      throw new CustomTemplateException(e);
    }
  }

  static boolean isApplyCustomTemplate() {
    boolean applyCustomTemplate = CTEnum.INSTANCE.isApplyCT();
    LOGGER.info("Is apply custom template: " + applyCustomTemplate);
    return applyCustomTemplate;
  }

  static ConfPath getContextPath(Maapi maapi, int tid, NavuNode service, String ctxPath) {
    ConfPath contextPath = service.getConfPath();
    try {
      if (null == ctxPath) {
        LOGGER.warn("Falling back to default context as context path not provided.");
      } else if (maapi.exists(tid, ctxPath)) {
        LOGGER.info("Changing context path of template to: " + ctxPath);
        contextPath = new ConfPath(ctxPath);
      }
    } catch (ConfException | IOException e) {
      LOGGER.warn("Falling back to default context. Context path error: " + e.getMessage());
    }
    return contextPath;
  }

  static boolean isTemplateApplied(ServiceContext context, String templateName, String deviceName,
      String owner) throws CustomTemplateException {
    boolean isApplied = false;
    try {
      Maapi maapi = context.getRootNode().context().getMaapi();
      int tid = maapi.startTrans(Conf.DB_OPERATIONAL, Conf.MODE_READ);
      isApplied = maapi.exists(tid, CT_APPLIED_PATH, templateName, deviceName, owner);
      maapi.finishTrans(tid);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    }
    return isApplied;
  }

  private static boolean validateInputs(ServiceContext context, String deviceName, String keyPath)
      throws CustomTemplateException, ConfException {
    boolean retVal = isApplyCustomTemplate();
    if (retVal) {
      List<String> errMsgs = new ArrayList<>();
      if (null == deviceName || deviceName.trim().length() == 0) {
        errMsgs.add(String.format(INP_ERR_MSG, "deviceName"));
      }
      if (null == keyPath || keyPath.trim().length() == 0) {
        errMsgs.add(String.format(INP_ERR_MSG, "keyPath/templateName"));
      } else if (keyPath.contains("/") && !keyPath.contains(customTemplate._custom_template_)) {
        errMsgs.add(String.format(INP_ERR_MSG, "keyPath"));
      }
      if (null == context) {
        errMsgs.add(String.format(INP_ERR_MSG, "context"));
      } else if (null != deviceName) {
        NavuList deviceList =
            (NavuList) context.getRootNode().getNavuNode(new ConfPath(DEVICE_TREE_PATH));
        if (!deviceList.containsNode(deviceName)) {
          errMsgs.add("Input: " + deviceName + " does not exist in nso device tree.");
        }
      }
      if (errMsgs.size() > 0) {
        String errMsg = errMsgs.stream().collect(Collectors.joining("\n"));
        throw new CustomTemplateException(errMsg);
      }
    }
    return retVal;
  }

  private static CTData createTemplateData(NavuNode customTemplateNode, String ctName,
      String deviceName, ConfPath ctxPath, Map<String, String> extraVariables)
      throws CustomTemplateException, NavuException {
    CTData data = new CTData(ctName, deviceName, ctxPath);
    if (null == data.getCtInfo()) {
      throw new CustomTemplateException("Custom template: " + ctName + " is not loaded into NSO.");
    }
    Set<String> ctVariables = data.getCtInfo().getVariables();
    if (!ctVariables.isEmpty()) {
      if (customTemplateNode.list(customTemplate._iteration_).isEmpty()) {
        LOGGER.debug("Variables defined in template: " + ctName);
        populateTemplateIteration(data,
            CTUtilities.getInputVariableMap(customTemplateNode.list(customTemplate._variable_)),
            ctVariables, null, extraVariables);
      } else {
        for (NavuNode iteration : customTemplateNode.list(customTemplate._iteration_)) {
          Integer itrNumber =
              Integer.parseInt(iteration.leaf(customTemplate._number_).valueAsString());
          if (ctVariables.isEmpty()) {
            ctVariables = data.getCtInfo().getVariables();
          }
          LOGGER
              .debug("Variables defined in template: " + ctName + " with iteration: " + itrNumber);
          populateTemplateIteration(data,
              CTUtilities.getInputVariableMap(iteration.list(customTemplate._variable_)),
              ctVariables, itrNumber, extraVariables);
        }
      }
    } else {
      LOGGER.debug("No variables defined in template: " + ctName);
    }
    return data;
  }

  private static void populateTemplateIteration(CTData ctData, Map<String, String> inputVars,
      Set<String> ctVariables, Integer itrNumber, Map<String, String> extraVariables)
      throws CustomTemplateException {
    Map<String, String> variableMap = new HashMap<>();
    CTUtilities.populateVariableMap(variableMap, ctVariables, extraVariables);
    CTUtilities.populateVariableMap(variableMap, ctVariables, inputVars);

    ctVariables.removeAll(variableMap.keySet());

    if (!ctVariables.isEmpty()) {
      StringBuilder errMsg =
          new StringBuilder("For custom-template: ").append(ctData.getCtInfo().getTemplateName())
              .append(", following variable's value not provided");
      if (null != itrNumber) {
        errMsg.append(" for iteration-").append(itrNumber);
      }
      errMsg.append(": ").append(String.join(",", ctVariables));
      throw new CustomTemplateException(errMsg.toString());
    }
    if (null != itrNumber) {
      ctData.addIteration(itrNumber, variableMap);
    } else {
      ctData.setVariables(variableMap);
    }
  }

  private static void populateTemplateIteration(CTData ctData, Map<String, String> inputVars,
      Set<String> ctVariables) throws CustomTemplateException {
    Map<String, String> variableMap = new HashMap<>();
    CTUtilities.populateVariableMap(variableMap, ctVariables, inputVars);

    ctVariables.removeAll(variableMap.keySet());

    if (!ctVariables.isEmpty()) {
      StringBuilder errMsg =
          new StringBuilder("For custom-template: ").append(ctData.getCtInfo().getTemplateName())
              .append(", following variable's value not provided: ")
              .append(String.join(",", ctVariables));
      throw new CustomTemplateException(errMsg.toString());
    }
    ctData.setVariables(variableMap);
  }
}
