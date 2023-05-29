/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.MSG_APPLY_SUCCESS;
import static com.cisco.nso.common.ctutils.CTConstants.MSG_DELETE_SUCCESS;
import static com.cisco.nso.common.ctutils.CTConstants.MSG_FLAG_OFF;
import static com.cisco.nso.common.ctutils.CTConstants.MSG_UPDATE_SUCCESS;
import static com.cisco.nso.common.ctutils.CTConstants.SRVC_POINT_CT;
import java.util.Map;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.CTConstants.STATUS;
import com.cisco.nso.common.ctutils.namespaces.customTemplate;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfPath;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.annotations.ServiceCallback;
import com.tailf.dp.proto.ServiceCBType;
import com.tailf.dp.services.ServiceContext;
import com.tailf.dp.services.ServiceOperationType;
import com.tailf.navu.NavuNode;

public class CTRFS {
  private static final Logger LOGGER = LogManager.getLogger(CTRFS.class);

  @ServiceCallback(servicePoint = SRVC_POINT_CT, callType = ServiceCBType.PRE_MODIFICATION)
  public Properties preModification(ServiceContext context, ServiceOperationType operation,
      ConfPath path, Properties opaque) throws DpCallbackException {
    LOGGER.debug("PreMod: operation: " + operation + ", ConfPath: " + path);
    try {
      String[] key = CTUtilities.getMultiKey(path);
      if (ServiceOperationType.DELETE.equals(operation) && !CTUtilsFunction.isApplyCustomTemplate()
          && CTUtilsFunction.isTemplateApplied(context, key[0], key[1], key[2])) {
        throw new CustomTemplateException(
            "Deletion of template: " + key[0] + ", for device: " + key[1] + ", for owner: " + key[2]
                + " is not allowed." + "\nBecause this template is already applied to device"
                + " and custom-template feature is off.");
      }
    } catch (ConfException | CustomTemplateException e) {
      LOGGER.error("", e);
      throw new DpCallbackException(e.getMessage(), e);
    }
    return opaque;
  }

  @ServiceCallback(servicePoint = SRVC_POINT_CT, callType = ServiceCBType.CREATE)
  public Properties create(ServiceContext context, NavuNode service, NavuNode ncsRoot,
      Properties opaque) throws DpCallbackException {
    String deviceName = null, templateName = null, owner = null;
    try {
      deviceName = service.leaf(customTemplate._device_).valueAsString();
      templateName = service.leaf(customTemplate._name_).valueAsString();
      owner = service.leaf(customTemplate._owner_).valueAsString();
      String ctxPath = service.leaf(customTemplate._ctx_path_).valueAsString();
      Map<String, String> extraVars =
          CTUtilities.getInputVariableMap(service.list(customTemplate._extra_var_));

      if (CTUtilsFunction.isApplyCustomTemplate()) {
        CTUtilsFunction.processCustomTemplate(context, service, deviceName, ctxPath, extraVars);
        CTUtilsFunction.createAppliedStatus(ncsRoot, templateName, deviceName, owner);
      }
    } catch (CustomTemplateException | ConfException e) {
      LOGGER.error("", e);
      CTNotif.sendNotification(templateName, deviceName, owner, STATUS.FAILED, e.getMessage());
      throw new DpCallbackException(e.getMessage(), e);
    }
    return opaque;
  }

  @ServiceCallback(servicePoint = SRVC_POINT_CT, callType = ServiceCBType.POST_MODIFICATION)
  public Properties postModification(ServiceContext context, ServiceOperationType operation,
      ConfPath path, Properties opaque) {
    LOGGER.debug("PostMod: operation: " + operation + ", ConfPath: " + path);
    try {
      String[] key = CTUtilities.getMultiKey(path);
      if (CTUtilsFunction.isApplyCustomTemplate()) {
        if (ServiceOperationType.CREATE.equals(operation)) {
          CTNotif.sendNotification(key[0], key[1], key[2], STATUS.APPLIED, MSG_APPLY_SUCCESS);
        } else if (ServiceOperationType.UPDATE.equals(operation)) {
          CTNotif.sendNotification(key[0], key[1], key[2], STATUS.APPLIED, MSG_UPDATE_SUCCESS);
        } else if (ServiceOperationType.DELETE.equals(operation)) {
          CTNotif.sendNotification(key[0], key[1], key[2], STATUS.DELETED, MSG_DELETE_SUCCESS);
        }
      } else if (CTUtilsFunction.isTemplateApplied(context, key[0], key[1], key[2])) {
        LOGGER.info("Deleted custom template: " + key[0] + " for device: " + key[1]
            + ", for owner: " + key[2]);
        CTNotif.sendNotification(key[0], key[1], key[2], STATUS.DELETED, MSG_FLAG_OFF);
      } else if (!ServiceOperationType.DELETE.equals(operation)) {
        LOGGER.info("No operation of custom template: " + key[0] + " for device: " + key[1]
            + ", for owner: " + key[2]);
        CTNotif.sendNotification(key[0], key[1], key[2], STATUS.NO_OPERATION, MSG_FLAG_OFF);
      }
    } catch (ConfException | CustomTemplateException e) {
      LOGGER.error("", e);
    }
    return opaque;
  }
}
