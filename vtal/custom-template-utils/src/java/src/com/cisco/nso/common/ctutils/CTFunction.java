/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.APPLY_DT_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATE;
import static com.cisco.nso.common.ctutils.CTConstants.FEATURE_TEMPLATE;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfXMLParam;
import com.tailf.conf.ConfXMLParamLeaf;
import com.tailf.conf.ConfXMLParamStart;
import com.tailf.conf.ConfXMLParamStop;
import com.tailf.conf.ConfXMLParamValue;
import com.tailf.maapi.Maapi;
import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.template.TemplateVariables;

/**
 * @author abhatta
 *
 */
class CTFunction {
  private static final Logger LOGGER = LogManager.getLogger(CTFunction.class);

  private CTFunction () {
    // sonar fix
  }

  static void applyCustomTemplate(Maapi maapi, int tid, CTData ctData)
      throws CustomTemplateException {
    if (FEATURE_TEMPLATE.equals(ctData.getCtInfo().getType())) {
      applyFeatureTemplate(maapi, tid, ctData);
    } else if (DEVICE_TEMPLATE.equals(ctData.getCtInfo().getType())) {
      applyDeviceTemplate(maapi, tid, ctData);
    } else {
      throw new CustomTemplateException("Un-supported custom-template type");
    }
  }

  private static void applyFeatureTemplate(Maapi maapi, int tid, CTData ctData)
      throws CustomTemplateException {
    try {
      TemplateVariables tempVariables = new TemplateVariables();
      if (null != ctData.getVariables()) {
        populateTemplateVariables(tempVariables, ctData.getCtInfo().getDeviceVariable(),
            ctData.getDeviceName(), ctData.getVariables());
        maapi.ncsApplyTemplate(tid, ctData.getCtInfo().getTemplateName(), ctData.getCtxPath(),
            tempVariables, true, true);
      } else {
        for (Map.Entry<Integer, Map<String, String>> entry : ctData.getIterations().entrySet()) {
          LOGGER.info("##### START Iteration: " + entry.getKey() + " #####");
          populateTemplateVariables(tempVariables, ctData.getCtInfo().getDeviceVariable(),
              ctData.getDeviceName(), entry.getValue());
          maapi.ncsApplyTemplate(tid, ctData.getCtInfo().getTemplateName(), ctData.getCtxPath(),
              tempVariables, true, true);
          tempVariables.clear();
          LOGGER.info("##### END Iteration: " + entry.getKey() + " #####");
        }
      }
    } catch (ConfException | IOException e) {
      throw new CustomTemplateException(e);
    }
  }

  private static void applyDeviceTemplate(Maapi maapi, int tid, CTData ctData)
      throws CustomTemplateException {
    int hash = new Ncs().hash();
    String templateName = ctData.getCtInfo().getTemplateName();
    if (null != ctData.getVariables()) {
      processDeviceTemplate(maapi, tid, ctData.getDeviceName(), templateName, ctData.getVariables(),
          hash);
    } else {
      for (Map.Entry<Integer, Map<String, String>> entry : ctData.getIterations().entrySet()) {
        LOGGER.info("##### START Iteration: " + entry.getKey() + " #####");
        processDeviceTemplate(maapi, tid, ctData.getDeviceName(), templateName, entry.getValue(),
            hash);
        LOGGER.info("##### END Iteration: " + entry.getKey() + " #####");
      }
    }
  }

  private static void populateTemplateVariables(TemplateVariables tempVariables,
      String deviceVariableName, String deviceName, Map<String, String> variableMap) {
    if (null != deviceVariableName) {
      tempVariables.putQuoted(deviceVariableName, deviceName);
    }
    for (Map.Entry<String, String> entry : variableMap.entrySet()) {
      tempVariables.putQuoted(entry.getKey(), entry.getValue());
    }
  }

  private static void processDeviceTemplate(Maapi maapi, int tid, String device, String template,
      Map<String, String> variableMap, int hash) throws CustomTemplateException {
    try {
      List<ConfXMLParam> params = createDeviceTemplateVariables(template, hash, variableMap);
      ConfXMLParam[] response = maapi.requestActionTh(tid, params, APPLY_DT_PATH, device);
      processResponse(template, device, response);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    }
  }

  private static List<ConfXMLParam> createDeviceTemplateVariables(String templateName, int hash,
      Map<String, String> variableMap) {
    List<ConfXMLParam> params = new ArrayList<>();
    params.add(new ConfXMLParamLeaf(hash, Ncs._suppress_positive_result));
    params.add(new ConfXMLParamValue(hash, Ncs._template_name, new ConfBuf(templateName)));
    for (Map.Entry<String, String> entry : variableMap.entrySet()) {
      params.add(new ConfXMLParamStart(hash, Ncs._variable));
      params.add(new ConfXMLParamValue(hash, Ncs._name, new ConfBuf(entry.getKey())));
      params
          .add(new ConfXMLParamValue(hash, Ncs._value, new ConfBuf("'" + entry.getValue() + "'")));
      params.add(new ConfXMLParamStop(hash, Ncs._variable));
    }
    return params;
  }

  private static void processResponse(String template, String device, ConfXMLParam[] response)
      throws CustomTemplateException, ConfException {
    if (ConfXMLParam.toXML(response).length() > 0) {
      for (ConfXMLParam resp : response) {
        if (Ncs._info_.equals(resp.getTag())) {
          throw new CustomTemplateException("Error applying device-template: " + template
              + ", on device: " + device + " : " + resp.getValue().toString());
        }
      }
    }
  }
}
