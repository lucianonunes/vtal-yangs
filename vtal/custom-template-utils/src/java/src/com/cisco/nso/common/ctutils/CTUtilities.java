/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.CONTROLLER_NAME;
import static com.cisco.nso.common.ctutils.CTConstants.CTX_SYSTEM;
import static com.cisco.nso.common.ctutils.CTConstants.CUSTOM_TEMPLATE_PREFIX;
import static com.cisco.nso.common.ctutils.CTConstants.CT_EVENT_STREAM_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_NAME;
import static com.cisco.nso.common.ctutils.CTConstants.HA_MODE_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.HA_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.USER_ADMIN;
import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.namespaces.customTemplate;
import com.tailf.cdb.Cdb;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfPath;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;

/**
 * @author abhatta
 *
 */
class CTUtilities {
  private static final Logger LOGGER = LogManager.getLogger(CTUtilities.class);

  private CTUtilities() {}

  static Maapi getMaapi(Socket socket) throws IOException, ConfException {
    Maapi maapi = new Maapi(socket);
    startSession(maapi);
    return maapi;
  }

  static void startSession(Maapi maapi) throws IOException, ConfException {
    maapi.startUserSession(USER_ADMIN, maapi.getSocket().getInetAddress(), CTX_SYSTEM,
        new String[] {USER_ADMIN}, MaapiUserSessionFlag.PROTO_TCP);
  }

  static void safeclose(Maapi maapi, Cdb cdb) {
    try {
      if (null != cdb) {
        cdb.close();
      }
      if (maapi != null) {
        maapi.getSocket().close();
      }
    } catch (Exception ignore) {
      LOGGER.warn("" + ignore);
    }
  }

  static void finishTransaction(Maapi maapi, int tid) {
    try {
      if (tid > 0 && null != maapi) {
        maapi.finishTrans(tid);
      }
    } catch (IOException | ConfException ignore) {
      LOGGER.warn("" + ignore);
    }
  }

  static boolean isCustomTemplate(String templateName) {
    return templateName.substring(0, 3).equalsIgnoreCase(CUSTOM_TEMPLATE_PREFIX);
  }

  static String[] getMultiKey(ConfPath path) throws ConfException {
    String key = ((ConfKey) path.getKP()[0]).toString();
    return key.substring(1, key.length() - 1).split(" ");
  }

  static Map<String, String> processNullMap(Map<String, String> map) {
    if (null != map) {
      return map;
    }
    return new HashMap<String, String>();
  }

  static Map<String, String> getInputVariableMap(NavuList inputVars) throws NavuException {
    Map<String, String> inpVarMap = new HashMap<>();
    for (NavuNode inputVar : inputVars) {
      inpVarMap.put(inputVar.leaf(customTemplate._name_).valueAsString(),
          inputVar.leaf(customTemplate._value_).valueAsString());
    }
    LOGGER.debug("Input variable map: " + inpVarMap);
    return inpVarMap;
  }

  static void populateVariableMap(Map<String, String> variableMap, Set<String> ctVariables,
      Map<String, String> inputVars) {
    for (Map.Entry<String, String> entry : inputVars.entrySet()) {
      if (ctVariables.contains(entry.getKey()) && null != entry.getValue()) {
        variableMap.put(entry.getKey(), entry.getValue());
      }
    }
  }

  static String getDeviceNameVariable(List<String> variableList) {
    String variableName = null;
    if (null != variableList && !variableList.isEmpty()) {
      if (variableList.contains(CONTROLLER_NAME)) {
        variableName = CONTROLLER_NAME;
      } else if (variableList.contains(DEVICE_NAME)) {
        variableName = DEVICE_NAME;
      } else if (variableList.contains(DEVICE)) {
        variableName = DEVICE;
      }
    }
    return variableName;
  }

  static boolean isHaModeMaster(Maapi maapi) {
    boolean isMaster = true;
    try {
      String haMode = getHaMode(maapi);
      if (null != haMode && !"master".equals(haMode)) {
        isMaster = false;
      }
    } catch (CustomTemplateException e) {
      LOGGER.error("", e);
    }
    return isMaster;
  }

  private static String getHaMode(Maapi maapi) throws CustomTemplateException {
    String haMode = null;
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
      if (maapi.exists(tid, HA_PATH)) {
        ConfEnumeration haModeEnum = (ConfEnumeration) maapi.getElem(tid, HA_MODE_PATH);
        haMode = ConfEnumeration.getLabelByEnum(HA_MODE_PATH, haModeEnum);
      }
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException("Error getting HA mode", e);
    } finally {
      finishTransaction(maapi, tid);
    }
    LOGGER.info("Getting HA mode: " + haMode);
    return haMode;
  }

  static boolean isStreamEnabled(Maapi maapi) {
    boolean streamValue = false;
    int tid = -1;
    try {
      tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
      streamValue =
          ((ConfBool) maapi.getElem(tid, CT_EVENT_STREAM_PATH)).booleanValue();
    } catch (IOException | ConfException e) {
      LOGGER.error("", e);
    } finally {
      CTUtilities.finishTransaction(maapi, tid);
    }
    return streamValue;
  }
}
