/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import com.cisco.nso.common.ctutils.namespaces.customTemplateHook;
import com.cisco.nso.common.ctutils.namespaces.customTemplateInfo;
import com.cisco.nso.common.ctutils.namespaces.customTemplateStatus;
import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.ns.NcsState;

/**
 * @author abhatta
 *
 */
class CTConstants {
  private static final String PS = "/";
  private static final String COL = ":";
  private static final String KEY = "{%s}";

  static final String DEVICE_TEMPLATES_PATH =
      genConst(PS, Ncs.prefix, COL, Ncs._devices_, PS, Ncs._template_);
  static final String DEVICE_TEMPLATE_PATH =
      genConst(PS, Ncs.prefix, COL, Ncs._devices_, PS, Ncs._template_, KEY);
  static final String DEVICE_TREE_PATH =
      genConst(PS, Ncs.prefix, COL, Ncs._devices_, PS, Ncs._device_);
  static final String APPLY_CT_PATH =
      genConst(PS, customTemplateHook.prefix, COL, customTemplateHook._apply_custom_template_);
  static final String CT_APPLIED_PATH = genConst(PS, customTemplateStatus.prefix, COL,
      customTemplateStatus._applied_custom_templates_, "{%s %s %s}");
  static final String CT_INFO_TEMP_PATH = genConst(PS, customTemplateInfo.prefix, COL,
      customTemplateInfo._custom_template_info_, "{%s}");
  static final String CT_INFO_PATH =
      genConst(PS, customTemplateInfo.prefix, COL, customTemplateInfo._custom_template_info_);
  static final String CT_EVENT_STREAM_PATH =
      genConst(PS, customTemplateInfo.prefix, COL, customTemplateInfo._ct_event_stream_enabled_);
  static final String CT_INFO_ITEM_PATH = genConst(CT_INFO_PATH, KEY);
  static final String APPLY_DT_PATH = genConst(DEVICE_TREE_PATH, KEY, PS, Ncs._apply_template_);

  // HA mode
  static final String HA_PATH =
      genConst(PS, NcsState.prefix, COL, NcsState._ncs_state_, PS, NcsState._ha_);
  static final String HA_MODE_PATH = genConst(HA_PATH, PS, NcsState._mode_);

  static final String SRVC_POINT_CT = "ct-servicepoint";
  static final String DEVICE_TEMPLATE_VAR_PATTERN = "\\{\\$(.*?)\\}";
  static final String CUSTOM_TEMPLATE_PREFIX = "ct-";
  static final String USER_ADMIN = "admin";
  static final String CTX_SYSTEM = "system";
  static final String FEATURE_TEMPLATE = "FEATURE";
  static final String DEVICE_TEMPLATE = "DEVICE";
  static final String MSG_APPLY_SUCCESS = "APPLY SUCCESS";
  static final String MSG_UPDATE_SUCCESS = "UPDATE SUCCESS";
  static final String MSG_DELETE_SUCCESS = "DELETE SUCCESS";
  static final String MSG_FLAG_OFF = "apply-custom-template flag is false";
  static final String NOTIF_STREAM = "customization-notif";
  static final String CT_NOTIF_STREAM = "custom-template-events";
  static final String DEVICE_NAME = "DEVICE_NAME";
  static final String CONTROLLER_NAME = "CONTROLLER_NAME";
  static final String DEVICE = "DEVICE";
  static final String INP_ERR_MSG = "Input: %s is not valid.";

  enum STATUS {
    APPLIED, DELETED, FAILED, NO_OPERATION
  }

  private static String genConst(String... strs) {
    return String.join("", strs);
  }
}
