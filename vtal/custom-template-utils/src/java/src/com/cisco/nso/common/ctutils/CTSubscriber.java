/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import static com.cisco.nso.common.ctutils.CTConstants.APPLY_CT_PATH;
import static com.cisco.nso.common.ctutils.CTConstants.DEVICE_TEMPLATES_PATH;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.nso.common.ctutils.namespaces.customTemplateHook;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDiffIterate;
import com.tailf.cdb.CdbSubscription;
import com.tailf.cdb.CdbSubscriptionSyncType;
import com.tailf.cdb.CdbSubscriptionType;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfBuf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.DiffIterateFlags;
import com.tailf.conf.DiffIterateOperFlag;
import com.tailf.conf.DiffIterateResultFlag;
import com.tailf.conf.ErrorCode;
import com.tailf.maapi.Maapi;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.ns.Ncs;

/**
 * @author abhatta
 *
 */
public class CTSubscriber implements ApplicationComponent {
  private static final Logger LOGGER = LogManager.getLogger(CTSubscriber.class);

  @Resource(type = ResourceType.CDB, scope = Scope.INSTANCE, qualifier = "custom-template-cdb")
  private Cdb cdb;

  @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE, qualifier = "custom-template-maapi")
  private Maapi maapi;

  private CdbSubscription sub = null;

  public CTSubscriber() {
    // sonar fix
  }

  @Override
  public void init() throws CustomTemplateException {
    try {
      CTUtilities.startSession(maapi);
      sub = cdb.newSubscription();
      sub.subscribe(CdbSubscriptionType.SUB_RUNNING, 1, new Ncs(), DEVICE_TEMPLATES_PATH);
      sub.subscribe(CdbSubscriptionType.SUB_RUNNING, 1, new customTemplateHook(), APPLY_CT_PATH);
      sub.subscribeDone();
    } catch (Exception e) {
      LOGGER.error("", e);
      throw new CustomTemplateException(e);
    }
  }

  @Override
  public void run() {
    try {
      try {
        // Following methods moved to run() so that subscribers on this path get triggered
        // at the start of packages also which is not possible if it is in init() method
        CTSubscriberFunction.initilizeCustomTemplate(maapi);
        CTSubscriberFunction.initFlag(maapi);
      } catch (Exception e) {
        LOGGER.error("", e);
        throw new CustomTemplateException(e);
      }
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

            ReqHandler req = new ReqHandler();
            for (Request r : reqs) {
              req.addRequest(r);
            }

            Thread t = new Thread(req);
            t.start();
          } finally {
            LOGGER.debug("sending sync for device template subscriber");
            sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);
            LOGGER.debug("sending sync done for device template subscriber");
          }

        } catch (Exception e) {
          LOGGER.error("", e);
        }
      }
    } catch (Exception e) {
      LOGGER.error("", e);
    }
  }

  @Override
  public void finish() throws Exception {
    CTUtilities.safeclose(maapi, cdb);
  }

  private enum Operation {
    CREATED, DELETED
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
        LOGGER.info("HA mode enabled and it is not master. Hence stopping CTSubscriber.");
        return DiffIterateResultFlag.ITER_STOP;
      }
      String kpStr = Conf.kpToString(kp);
      LOGGER.debug("diffIterate: kp= " + kpStr + ", OP=" + op + ", old_value=" + oldValue
          + ", new_value=" + newValue);
      if (kpStr.contains(customTemplateHook._apply_custom_template_)) {
        CTEnum.INSTANCE.setApplyCT(((ConfBool) newValue).booleanValue());
        LOGGER.info("Changed CustomTemplateFlag to " + newValue);
        return DiffIterateResultFlag.ITER_CONTINUE;
      } else {
        @SuppressWarnings("unchecked")
        ArrayList<Request> reqs = (ArrayList<Request>) initState;

        try {
          Request r = new Request();
          ConfKey nameKey = (ConfKey) kp[0];
          String templateName = ((ConfBuf) nameKey.elementAt(0)).toString();
          if (CTUtilities.isCustomTemplate(templateName)) {
            r.templateName = templateName;
            LOGGER.debug("Complete keypath: " + Arrays.toString(kp));
            LOGGER.debug("Template Name: " + r.templateName);

            if (op == DiffIterateOperFlag.MOP_CREATED || op == DiffIterateOperFlag.MOP_VALUE_SET
                || op == DiffIterateOperFlag.MOP_MODIFIED) {
              r.op = Operation.CREATED;
            } else if (op == DiffIterateOperFlag.MOP_DELETED) {
              r.op = Operation.DELETED;
            } else {
              return DiffIterateResultFlag.ITER_RECURSE;
            }
            // collect all the requests together
            reqs.add(r);
          }
          return DiffIterateResultFlag.ITER_CONTINUE;
        } catch (Exception e) {
          LOGGER.error("", e);
          return DiffIterateResultFlag.ITER_CONTINUE;
        }
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
      try {
        while (!this.reqs.isEmpty()) {

          LOGGER.debug("Inside Run: No of reqs:" + reqs.size());
          this.r = reqs.remove(0);

          if (r.op == Operation.CREATED) {
            CTSubscriberFunction.addDeviceTemplate(maapi, r.templateName);
          } else if (r.op == Operation.DELETED) {
            CTSubscriberFunction.removeDeviceTemplate(maapi, r.templateName);
          }
        }
        LOGGER.debug("Ending RequestHandler Thread for device template");
      } catch (Exception e) {
        LOGGER.error("", e);
      }
    }
  }
}
