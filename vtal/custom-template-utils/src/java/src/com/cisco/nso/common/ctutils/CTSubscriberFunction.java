/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.APPLY_CT_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATE;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATES_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATE_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATE_VAR_PATTERN;
import static com.cisco.nso.common.ctutils.CTConstants.FEATURE_TEMPLATE;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfPath;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiConfigFlag;
import com.tailf.maapi.MaapiInputStream;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ns.Ncs;

/**
 * @author abhatta
 *
 */
class CTSubscriberFunction {
  private static final Logger LOGGER = LogManager.getLogger(CTSubscriberFunction.class);

  private CTSubscriberFunction () {
    // sonar fix
  }

  static void initilizeCustomTemplate(Maapi maapi) throws CustomTemplateException {
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ_WRITE);
      CTEnum.INSTANCE.getCtInfoSet().clear();
      initilizeFeatureTemplate(maapi);
      initilizeDeviceTemplate(maapi);
      CTInfoFunction.persistCTInfo(maapi, tid);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    } finally {
      CTUtilities.finishTransaction(maapi, tid);
    }
  }

  static void addDeviceTemplate(Maapi maapi, String templateName) throws CustomTemplateException {
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ_WRITE);
      CTInfo ctInfo = readDeviceTemplate(maapi, tid, templateName);
      CTInfoFunction.addCTInfo(maapi, tid, ctInfo);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    } finally {
      CTUtilities.finishTransaction(maapi, tid);
    }
  }

  static void removeDeviceTemplate(Maapi maapi, String templateName)
      throws CustomTemplateException {
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ_WRITE);
      CTInfoFunction.removeCTInfo(maapi, tid, templateName);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    } finally {
      CTUtilities.finishTransaction(maapi, tid);
    }
  }

  static void initFlag(Maapi maapi) throws CustomTemplateException {
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
      boolean newValue = ((ConfBool) maapi.getElem(tid, APPLY_CT_PATH)).booleanValue();
      CTEnum.INSTANCE.setApplyCT(newValue);
      LOGGER.info("Initialized CustomTemplateFlag to " + newValue);
    } catch (ConfException | IOException e) {
      throw new CustomTemplateException(e);
    } finally {
      CTUtilities.finishTransaction(maapi, tid);
    }
  }

  static CTInfo readDeviceTemplate(Maapi maapi, int tid, String templateName)
      throws CustomTemplateException {
    CTInfo ctInfo = new CTInfo(templateName);
    ctInfo.setType(DEVICE_TEMPLATE);
    try (
        MaapiInputStream mis = maapi.saveConfig(tid, EnumSet.of(MaapiConfigFlag.XML_FORMAT),
            new ConfPath(DEVICE_TEMPLATE_PATH, templateName));
        BufferedReader br = new BufferedReader(new InputStreamReader(mis))) {

      Set<String> variables = new HashSet<String>();
      String line;
      Pattern pattern = Pattern.compile(DEVICE_TEMPLATE_VAR_PATTERN);
      while ((line = br.readLine()) != null) {
        Matcher matcher = pattern.matcher(line);
        while (matcher.find()) {
          variables.add(matcher.group(1));
        }
      }
      LOGGER.debug("For device template: " + templateName + ", variables read: " + variables);
      ctInfo.setVariables(variables);
    } catch (ConfException | IOException e) {
      throw new CustomTemplateException(e);
    }
    return ctInfo;
  }

  static CTInfo readFeatureTemplate(Maapi maapi, String templateName)
      throws CustomTemplateException {
    CTInfo ctInfo = null;
    try {
      ctInfo = new CTInfo(templateName);
      ctInfo.setType(FEATURE_TEMPLATE);
      LOGGER.debug("Getting variables for feature template: " + templateName);
      List<String> variableList =
          new ArrayList<String>(Arrays.asList(maapi.ncsGetTemplateVariables(templateName)));
      ctInfo.setDeviceVariable(CTUtilities.getDeviceNameVariable(variableList));
      variableList.remove(ctInfo.getDeviceVariable());
      ctInfo.setVariables(new HashSet<>(variableList));
    } catch (ConfException | IOException e) {
      throw new CustomTemplateException(e);
    }
    return ctInfo;
  }

  private static void initilizeFeatureTemplate(Maapi maapi) throws CustomTemplateException {
    try {
      for (String templateName : maapi.ncsTemplates()) {
        if (CTUtilities.isCustomTemplate(templateName)) {
          CTInfo ctInfo = readFeatureTemplate(maapi, templateName);
          CTEnum.INSTANCE.getCtInfoSet().add(ctInfo);
        }
      }
    } catch (ConfException | IOException e) {
      throw new CustomTemplateException(e);
    }
  }

  private static void initilizeDeviceTemplate(Maapi maapi) throws CustomTemplateException {
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
      NavuContainer ncsRoot = new NavuContainer(maapi, tid, Ncs.hash);
      List<NavuNode> deviceTemplates =
          ((NavuList) ncsRoot.getNavuNode(new ConfPath(DEVICE_TEMPLATES_PATH))).children().stream()
              .filter(item -> {
                try {
                  return CTUtilities.isCustomTemplate(item.leaf(Ncs._name_).valueAsString());
                } catch (NavuException e) {
                  LOGGER.error("" + e);
                  return false;
                }
              }).collect(Collectors.toList());
      if (deviceTemplates != null) {
        for (NavuNode template : deviceTemplates) {
          String templateName = template.leaf(Ncs._name_).valueAsString();
          CTInfo ctInfo = readDeviceTemplate(maapi, tid, templateName);
          if (!CTEnum.INSTANCE.getCtInfoSet().add(ctInfo)) {
            throw new CustomTemplateException("Custom-template: " + templateName
                + " exists as both device and feature template.");
          }
        }
      }
    } catch (ConfException | IOException e) {
      throw new CustomTemplateException(e);
    } finally {
      CTUtilities.finishTransaction(maapi, tid);
    }
  }
}
