
/*
 ***************************************************
 * Copyright (c) 2018 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.CT_INFO_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.CT_NOTIF_STREAM;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.namespaces.customTemplateInfo;
import com.cisco.nso.common.ctutils.namespaces.customTemplate;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfDatetime;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfTag;
import com.tailf.conf.ConfXMLParam;
import com.tailf.conf.ConfXMLParamStart;
import com.tailf.conf.ConfXMLParamStop;
import com.tailf.conf.ConfXMLParamValue;
import com.tailf.dp.DpActionTrans;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.annotations.ActionCallback;
import com.tailf.dp.proto.ActionCBType;
import com.tailf.maapi.Maapi;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ns.Ncs;

public class CTInfoAction {
  private static final Logger LOGGER = LogManager.getLogger(CTInfoAction.class);

  @ActionCallback(callPoint = "sync-custom-templates", callType = ActionCBType.ACTION)
  public ConfXMLParam[] action(DpActionTrans trans, ConfTag name, ConfObject[] kp,
      ConfXMLParam[] params) throws DpCallbackException {
    Maapi maapi = null;
    ConfXMLParam[] finalMsg;
    try (Socket s = new Socket(InetAddress.getByName("localhost").getHostAddress(),
        Integer.parseInt(System.getenv("NCS_IPC_PORT")))) {
      LOGGER.info("ENTRY POINT for sync-custom-templates");
      maapi = CTUtilities.getMaapi(s);
      if (CTUtilities.isStreamEnabled(maapi)) {
        int tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
        List<ConfXMLParam> vals = new ArrayList<>();
        vals.add(new ConfXMLParamStart(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_events_));
        NavuContainer ncsRoot = new NavuContainer(maapi, tid, Ncs.hash);
        NavuList customTemplatesInfoLists =
            (NavuList) ncsRoot.getNavuNode(new ConfPath(CT_INFO_PATH));
        if (customTemplatesInfoLists != null) {
          for (NavuNode template : customTemplatesInfoLists) {
            String tempName = template.leaf(customTemplateInfo._template_name_).valueAsString();
            vals.addAll(
                CTInfoSub.createNotificationXml(ncsRoot, tempName, CTInfoSub.Operation.MODIFIED));
          }
        }
        vals.add(new ConfXMLParamStop(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_events_));
        ConfXMLParam[] finalNt = vals.stream().toArray(ConfXMLParam[]::new);
        LOGGER.debug(finalNt);
        CTInfoSub.stream.send(ConfDatetime.getConfDatetime(), finalNt);
        finalMsg = new ConfXMLParam[] {
            new ConfXMLParamValue(customTemplate.hash, customTemplate._success, new ConfBool(true)),
            new ConfXMLParamValue(customTemplate.hash, customTemplate._detail,
                new ConfBuf("Successfully sync'd custom template info."))};
        LOGGER.info("EXIT POINT for sync-custom-templates, Notification sent:" + CT_NOTIF_STREAM);
        CTUtilities.finishTransaction(maapi, tid);
      } else {
        finalMsg = new ConfXMLParam[] {
            new ConfXMLParamValue(customTemplate.hash, customTemplate._success,
                new ConfBool(false)),
            new ConfXMLParamValue(customTemplate.hash, customTemplate._detail,
                new ConfBuf("custom-templates sync action not allowed on CFS node."))};
      }
    } catch (Exception e) {
      LOGGER.error("", e);
      finalMsg = new ConfXMLParam[] {
          new ConfXMLParamValue(customTemplate.hash, customTemplate._success, new ConfBool(false)),
          new ConfXMLParamValue(customTemplate.hash, customTemplate._detail,
              new ConfBuf(e.toString()))};
    }
    return finalMsg;
  }
}
