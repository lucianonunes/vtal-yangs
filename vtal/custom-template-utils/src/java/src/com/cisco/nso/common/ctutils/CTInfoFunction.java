/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.CT_INFO_ITEM_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATE_PATH;
import java.io.IOException;
import java.util.Optional;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfList;
import com.tailf.conf.ConfObject;
import com.tailf.maapi.Maapi;

/**
 * @author abhatta
 *
 */
class CTInfoFunction {
  private static final Logger LOGGER = LogManager.getLogger(CTInfoFunction.class);

  private CTInfoFunction() {}

  static CTInfo getCTInfo(String templateName) {
    Optional<CTInfo> opt = CTEnum.INSTANCE.getCtInfoSet().stream()
        .filter(item -> item.getTemplateName().equals(templateName)).findFirst();
    if (opt.isPresent()) {
      return new CTInfo(opt.get());
    }
    return null;
  }

  static CTInfo createCTInfo(Maapi maapi, int tid, String templateName)
      throws CustomTemplateException {
    CTInfo ctInfo = null;
    try {
      if (maapi.ncsTemplates().contains(templateName)) {
        ctInfo = CTSubscriberFunction.readFeatureTemplate(maapi, templateName);
      } else if (maapi.exists(tid, DEVICE_TEMPLATE_PATH, templateName)) {
        ctInfo = CTSubscriberFunction.readDeviceTemplate(maapi, tid, templateName);
      } else {
        throw new CustomTemplateException(templateName + " is not loaded into NSO.");
      }
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    }
    LOGGER.info("Created " + ctInfo);
    CTEnum.INSTANCE.getCtInfoSet().add(ctInfo);
    return ctInfo;
  }

  static void removeCTInfo(Maapi maapi, int tid, String templateName)
      throws CustomTemplateException {
    try {
      LOGGER.info("Removing " + templateName);
      String path = String.format(CT_INFO_ITEM_PATH, templateName);
      maapi.safeDelete(tid, path);
      maapi.applyTrans(tid, false);
      CTEnum.INSTANCE.getCtInfoSet().remove(new CTInfo(templateName));
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    }
  }

  static void addCTInfo(Maapi maapi, int tid, CTInfo ctInfo) throws CustomTemplateException {
    try {
      processCTInfo(maapi, tid, ctInfo);
      maapi.applyTrans(tid, false);
      CTEnum.INSTANCE.getCtInfoSet().add(ctInfo);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    }
  }

  static void persistCTInfo(Maapi maapi, int tid) throws CustomTemplateException {
    try {
      for (CTInfo ctInfo : CTEnum.INSTANCE.getCtInfoSet()) {
        processCTInfo(maapi, tid, ctInfo);
      }
      maapi.applyTrans(tid, false);
    } catch (IOException | ConfException e) {
      throw new CustomTemplateException(e);
    }
  }

  private static void processCTInfo(Maapi maapi, int tid, CTInfo ctInfo)
      throws IOException, ConfException {
    LOGGER.info("Adding " + ctInfo);
    String path = String.format(CT_INFO_ITEM_PATH, ctInfo.getTemplateName());
    maapi.safeCreate(tid, path);
    maapi.setElem(tid, ctInfo.getType(), path + "/type");
    if (null != ctInfo.getDeviceVariable()) {
      maapi.setElem(tid, ctInfo.getDeviceVariable(), path + "/device-variable");
    }
    if (!ctInfo.getVariables().isEmpty()) {
      ConfObject[] vars = new ConfBuf[ctInfo.getVariables().size()];
      int i = 0;
      for (String var : ctInfo.getVariables()) {
        vars[i] = new ConfBuf(var);
        i++;
      }
      maapi.setElem(tid, new ConfList(vars), path + "/variables");
    }
  }
}
