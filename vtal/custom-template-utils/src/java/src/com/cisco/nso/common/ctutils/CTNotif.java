/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.NOTIF_STREAM;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.CTConstants.STATUS;
import com.cisco.nso.common.ctutils.namespaces.customTemplateStatus;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfDatetime;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfXMLParam;
import com.tailf.conf.ConfXMLParamStart;
import com.tailf.conf.ConfXMLParamStop;
import com.tailf.conf.ConfXMLParamValue;
import com.tailf.dp.Dp;
import com.tailf.dp.DpNotifStream;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.NcsMain;

/**
 *
 * @author abhatta
 *
 */

public class CTNotif implements ApplicationComponent {
  private static final Logger LOGGER = LogManager.getLogger(CTNotif.class);
  private static DpNotifStream stream;
  private Socket dpSocket;

  @Override
  public void init() {
    try {
      dpSocket = new Socket(NcsMain.getInstance().getNcsHost(), NcsMain.getInstance().getNcsPort());
      stream = new Dp(NOTIF_STREAM, dpSocket).createNotifStream(NOTIF_STREAM); // NOSONAR
      LOGGER.info(NOTIF_STREAM + " stream created.");
    } catch (IOException | ConfException e) {
      LOGGER.error(NOTIF_STREAM + " stream can not be created.", e);
    }
  }

  @Override
  public void finish() {
    if (dpSocket != null) {
      try {
        dpSocket.close();
      } catch (IOException e) {
        LOGGER.error("Failed to close socket.", e);
      }
    }
  }

  static synchronized void sendNotification(String templateName, String deviceName, String owner,
      STATUS status, String message) {
    try {
      ConfXMLParam[] vals = createNotification(templateName, deviceName, owner, status, message);
      LOGGER.debug(ConfXMLParam.toXML(vals));
      stream.send(ConfDatetime.getConfDatetime(), vals);
    } catch (ConfException | IOException e) {
      LOGGER.error("Failed to send notification on stream " + NOTIF_STREAM, e);
    }
  }

  private static ConfXMLParam[] createNotification(String templateName, String deviceName,
      String owner, STATUS status, String message) {
    List<ConfXMLParam> notifXml = new ArrayList<>();

    notifXml.add(0, new ConfXMLParamStart(customTemplateStatus.prefix,
        customTemplateStatus._customization_notif_));
    notifXml.add(1, new ConfXMLParamValue(customTemplateStatus.prefix, customTemplateStatus._name_,
        new ConfBuf(templateName)));
    notifXml.add(2, new ConfXMLParamValue(customTemplateStatus.prefix,
        customTemplateStatus._device_, new ConfBuf(deviceName)));
    notifXml.add(3, new ConfXMLParamValue(customTemplateStatus.prefix, customTemplateStatus._owner_,
        new ConfBuf(owner)));
    notifXml.add(4, new ConfXMLParamValue(customTemplateStatus.prefix,
        customTemplateStatus._status_, new ConfBuf(status.toString())));
    notifXml.add(5, new ConfXMLParamValue(customTemplateStatus.prefix,
        customTemplateStatus._message_, new ConfBuf(message)));
    notifXml.add(6, new ConfXMLParamStop(customTemplateStatus.prefix,
        customTemplateStatus._customization_notif_));

    ConfXMLParam[] notif = notifXml.stream().toArray(ConfXMLParam[]::new);
    return notif;
  }

  @Override
  public void run() {
    // sonar fix
  }
}
