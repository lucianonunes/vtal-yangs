/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.CT_INFO_TEMP_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.CT_INFO_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.CT_NOTIF_STREAM;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.namespaces.customTemplateInfo;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDiffIterate;
import com.tailf.cdb.CdbSubscription;
import com.tailf.cdb.CdbSubscriptionSyncType;
import com.tailf.cdb.CdbSubscriptionType;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfDatetime;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfXMLParam;
import com.tailf.conf.ConfXMLParamStart;
import com.tailf.conf.ConfXMLParamStop;
import com.tailf.conf.ConfXMLParamValue;
import com.tailf.conf.DiffIterateFlags;
import com.tailf.conf.DiffIterateOperFlag;
import com.tailf.conf.DiffIterateResultFlag;
import com.tailf.conf.ErrorCode;
import com.tailf.dp.Dp;
import com.tailf.dp.DpNotifStream;
import com.tailf.maapi.Maapi;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.NcsMain;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.ns.Ncs;


/**
 * @author
 *
 */
public class CTInfoSub implements ApplicationComponent {
  private static final Logger LOGGER = LogManager.getLogger(CTInfoSub.class);
  public static DpNotifStream stream;
  private Socket dpSocket;

  @Resource(type = ResourceType.CDB, scope = Scope.INSTANCE, qualifier = "ct-info-cdb")
  private Cdb cdb;

  @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE, qualifier = "ct-info-maapi")
  private Maapi maapi;

  private CdbSubscription sub = null;

  public CTInfoSub() {
    // sonar fix
  }

  @Override
  public void init() throws CustomTemplateException {
    try {
      CTUtilities.startSession(maapi);
      sub = cdb.newSubscription();
      sub.subscribe(CdbSubscriptionType.SUB_RUNNING, 1, new Ncs(), CT_INFO_PATH);
      sub.subscribeDone();
      LOGGER.info("subscribeDone");
      dpSocket = new Socket(NcsMain.getInstance().getNcsHost(), NcsMain.getInstance().getNcsPort());
      LOGGER.info("dpSocket: "+dpSocket.getLocalPort());
      stream = new Dp(CT_NOTIF_STREAM, dpSocket).createNotifStream(CT_NOTIF_STREAM); // NOSONAR
      LOGGER.info("stream :"+stream.isAlive());
    } catch (Exception e) {
      LOGGER.error("", e);
      throw new CustomTemplateException(e);
    }
  }

  @Override
  public void finish() {
    CTUtilities.safeclose(maapi, cdb);
    if (dpSocket != null) {
      try {
        dpSocket.close();
      } catch (IOException e) {
        LOGGER.error("Failed to close socket.", e);
      }
    }
  }

  @Override
  public void run() {
    try {
      while (true) {
        int[] points;

        try {
          LOGGER.debug("Starting subscription read");
          points = sub.read();
          LOGGER.debug("Done with subscription read");
        } catch (ConfException e) {
          if (e.getErrorCode() == ErrorCode.ERR_EOF) {
            LOGGER.warn("Socket Closed!");
          }
          return;
        } catch (Exception e) {
          return;
        }

        LOGGER.debug("Iterating subs ");
        EnumSet<DiffIterateFlags> enumSet = EnumSet.<DiffIterateFlags>of(
            DiffIterateFlags.ITER_WANT_PREV, DiffIterateFlags.ITER_WANT_ANCESTOR_DELETE,
            DiffIterateFlags.ITER_WANT_SCHEMA_ORDER);

        ArrayList<Request> reqs = new ArrayList<Request>();

        try {
          try {
            sub.diffIterate(points[0], new Iter(sub), enumSet, reqs);

            // It's important that we return as early as possible here,
            // This is a common technique, gather the work to do, tell
            // CDB that we're done and then do the work.
            // It could be that the allocator needs to reach out (RPC)
            // and that can be slow

            if (!reqs.isEmpty()) {
              ReqHandler req = new ReqHandler();
              for (Request r : reqs) {
                req.addRequest(r);
              }
              Thread t = new Thread(req);
              t.start();
            }
          } finally {
            LOGGER.debug("sending sync for ct-info subscriber");
            sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);
            LOGGER.debug("sending sync done for ct-info subscriber");
          }

        } catch (Exception e) {
          LOGGER.error("", e);
        }
      }
    } catch (Exception e) {
      LOGGER.error("", e);
    }
  }

  public enum Operation {
    CREATED, DELETED, MODIFIED
  }

  private class Request {
    private Operation op;
    private String templateName;
  }

  private class Iter implements CdbDiffIterate {

    Iter(CdbSubscription sub) {}

    @Override
    public DiffIterateResultFlag iterate(ConfObject[] kp, DiffIterateOperFlag op,
        ConfObject oldValue, ConfObject newValue, Object initState) {
      if (!CTUtilities.isHaModeMaster(maapi)) {
        LOGGER.info("HA mode enabled and it is not master. Hence stopping CTInfoSub.");
        return DiffIterateResultFlag.ITER_STOP;
      }

      if (!CTUtilities.isStreamEnabled(maapi)) {
        LOGGER.info("Stopping CTInfoSub because of CFS node.");
        return DiffIterateResultFlag.ITER_STOP;
      }

      @SuppressWarnings("unchecked")
      ArrayList<Request> reqs = (ArrayList<Request>) initState;

      try {
        Request r = new Request();
        ConfKey nameKey = (ConfKey) kp[0];
        String templateName = ((ConfBuf) nameKey.elementAt(0)).toString();
        r.templateName = templateName;
        LOGGER.debug("Complete keypath: " + Arrays.toString(kp));
        LOGGER.debug("Template Name: " + r.templateName);

        if (op == DiffIterateOperFlag.MOP_CREATED) {
          r.op = Operation.CREATED;
        } else if (op == DiffIterateOperFlag.MOP_DELETED) {
          r.op = Operation.DELETED;
        } else if (op == DiffIterateOperFlag.MOP_MODIFIED) {
          r.op = Operation.MODIFIED;
        } else {
          return DiffIterateResultFlag.ITER_RECURSE;
        }
        // collect all the requests together
        reqs.add(r);
        return DiffIterateResultFlag.ITER_CONTINUE;
      } catch (Exception e) {
        LOGGER.error("", e);
        return DiffIterateResultFlag.ITER_CONTINUE;
      }
    }
  }

  private class ReqHandler implements Runnable {
    private Request r;
    private ArrayList<Request> reqs = new ArrayList<Request>();

    public void addRequest(Request request) {
      reqs.add(request);
    }

    public ReqHandler() {
      // sonar fix
    }

    @Override
    public void run() {
      int tid = -1;
      try {
        List<ConfXMLParam> vals = new ArrayList<>();
        vals.add(new ConfXMLParamStart(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_events_));
        tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
        NavuContainer ncsRoot = new NavuContainer(maapi, tid, Ncs.hash);
        while (!this.reqs.isEmpty()) {
          LOGGER.debug("Inside Run: No of reqs:" + reqs.size());
          this.r = reqs.remove(0);
          vals.addAll(createNotificationXml(ncsRoot, r.templateName, r.op));
        }
        vals.add(new ConfXMLParamStop(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_events_));
        try {
          ConfXMLParam[] finalNt = vals.stream().toArray(ConfXMLParam[]::new);
          LOGGER.debug(finalNt);
          stream.send(ConfDatetime.getConfDatetime(), finalNt);
          LOGGER.info("Notification sent: " + CT_NOTIF_STREAM);
        } catch (ConfException | IOException e) {
          LOGGER.error("Failed to send notification on stream" + CT_NOTIF_STREAM, e);
        }
        LOGGER.debug("Ending RequestHandler Thread for template");
      } catch (Exception e) {
        LOGGER.error("", e);
      } finally {
        CTUtilities.finishTransaction(maapi, tid);
      }
    }
  }

  public static List<ConfXMLParam> createNotificationXml(NavuContainer ncsRoot, String templateName,
      Operation operation) {
    List<ConfXMLParam> childnotifXml = new ArrayList<>();
    try {
      if (operation.equals(Operation.CREATED) || operation.equals(Operation.MODIFIED)) {
        int index = 0;
        NavuNode template = ncsRoot.getNavuNode(new ConfPath(CT_INFO_TEMP_PATH, templateName));
        childnotifXml.add(index++, new ConfXMLParamStart(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_info_));
        childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
            customTemplateInfo._template_name_, new ConfBuf(templateName)));
        if (template.leaf(customTemplateInfo._type_).valueAsString().equals("FEATURE")) {
          childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
              customTemplateInfo._type_, new ConfEnumeration(0)));

          if (template.leaf(customTemplateInfo._device_variable_).valueAsString()
              .equals("CONTROLLER_NAME")) {
            childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
                customTemplateInfo._device_variable_, new ConfEnumeration(0)));
          } else if (template.leaf(customTemplateInfo._device_variable_).valueAsString()
              .equals("DEVICE")) {
            childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
                customTemplateInfo._device_variable_, new ConfEnumeration(1)));
          } else {
            childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
                customTemplateInfo._device_variable_, new ConfEnumeration(2)));
          }
        } else {
          childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
              customTemplateInfo._type_, new ConfEnumeration(1)));
        }

        if (template.leaf(customTemplateInfo._variables_).valueAsString() != null) {
          String strArray[] =
              template.leaf(customTemplateInfo._variables_).valueAsString().split(" ");
          for (String var : strArray) {
            childnotifXml.add(index++,
                new ConfXMLParamStart(customTemplateInfo.prefix, customTemplateInfo._variable_));
            childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
                customTemplateInfo._name_, new ConfBuf(var)));
            childnotifXml.add(index++,
                new ConfXMLParamStop(customTemplateInfo.prefix, customTemplateInfo._variable_));
          }
        }
        if (operation.equals(Operation.CREATED)) {
          childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
              customTemplateInfo._operation_, new ConfEnumeration(0)));
        } else if (operation.equals(Operation.MODIFIED)) {
          childnotifXml.add(index++, new ConfXMLParamValue(customTemplateInfo.prefix,
              customTemplateInfo._operation_, new ConfEnumeration(1)));
        }
        childnotifXml.add(index++, new ConfXMLParamStop(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_info_));
      } else if (operation.equals(Operation.DELETED)) {
        int i = 0;
        childnotifXml.add(i++, new ConfXMLParamStart(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_info_));
        childnotifXml.add(i++, new ConfXMLParamValue(customTemplateInfo.prefix,
            customTemplateInfo._template_name_, new ConfBuf(templateName)));
        childnotifXml.add(i++, new ConfXMLParamValue(customTemplateInfo.prefix,
            customTemplateInfo._operation_, new ConfEnumeration(2)));
        childnotifXml.add(i++, new ConfXMLParamStop(customTemplateInfo.prefix,
            customTemplateInfo._custom_template_info_));
      }
    } catch (Exception e) {
      LOGGER.error("", e);
    }
    return childnotifXml;
  }
}
