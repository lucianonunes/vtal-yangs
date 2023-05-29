package com.cisco.corefpcommon.templateutils;

import java.io.IOException;
import java.net.Socket;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.corefpcommon.namespaces.coreFpCommonTemplateMapper;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDiffIterate;
import com.tailf.cdb.CdbSubscription;
import com.tailf.cdb.CdbSubscriptionSyncType;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfEnumeration;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfTag;
import com.tailf.conf.DiffIterateFlags;
import com.tailf.conf.DiffIterateOperFlag;
import com.tailf.conf.DiffIterateResultFlag;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.NcsMain;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.ns.Ncs;

public class TemplateMapperSub implements ApplicationComponent, CdbDiffIterate {
    private static final Logger LOGGER = LogManager.getLogger(TemplateMapperSub.class);

    private CdbSubscription sub = null;
    private int tid;
    private NavuContainer ncsRoot;

    private Cdb cdb;

    @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE,
        qualifier = "cfp-common-template-mapper-m")
    private Maapi maapi;

    public enum Type {
        GLOBAL, PROVIDER, TURN_OFF_FLAG
    }

    @Override
    public void init() throws Exception {
        try {
            /*
             * system session, a dedicated session for HA check
             */
            maapi.startUserSession("", maapi.getSocket().getInetAddress(), "system", new String[] {
                ""}, MaapiUserSessionFlag.PROTO_TCP);

            tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
            ncsRoot = new NavuContainer(maapi, tid, Ncs.hash);

            Socket cdbSocket = new Socket(NcsMain.getInstance().getNcsHost(), NcsMain.getInstance()
                .getNcsPort());
            cdb = new Cdb("cdbSubscriptionSocket", cdbSocket);
            // creating new subscription
            sub = cdb.newSubscription();
            sub.subscribe(1, new coreFpCommonTemplateMapper(), "/"
                + coreFpCommonTemplateMapper.prefix + ":"
                + coreFpCommonTemplateMapper._override_template_ + "/"
                + coreFpCommonTemplateMapper.prefix + ":"
                + coreFpCommonTemplateMapper._ignore_override_settings_);
            sub.subscribe(1, new coreFpCommonTemplateMapper(), "/"
                + coreFpCommonTemplateMapper.prefix + ":"
                + coreFpCommonTemplateMapper._override_template_ + "/"
                + coreFpCommonTemplateMapper.prefix + ":"
                + coreFpCommonTemplateMapper._service_template_mapper_);
            sub.subscribe(1, new coreFpCommonTemplateMapper(), "/"
                + coreFpCommonTemplateMapper.prefix + ":"
                + coreFpCommonTemplateMapper._override_template_ + "/"
                + coreFpCommonTemplateMapper.prefix + ":"
                + coreFpCommonTemplateMapper._per_provider_);
            sub.subscribeDone();
            TemplateFinderFunction.init().loadTemplateMappingData(maapi, tid);
        } catch (Exception e) {
            LOGGER.error(
                "Failed to initilize TemplateMapperSub, template override feature will be affected",
                e);
        }
    }

    @Override
    public void run() {
        try {
            while (true) {
                LOGGER.info("Iterating TemplateMapper yang monitor");
                int[] points;

                try {
                    points = sub.read();
                    if (points == null) {
                        return;
                    }
                } catch (Exception ignore) {
                    LOGGER.error(ignore);
                    return;
                }

                EnumSet<DiffIterateFlags> enumSet = EnumSet.<DiffIterateFlags>of(
                    DiffIterateFlags.ITER_WANT_PREV, DiffIterateFlags.ITER_WANT_ANCESTOR_DELETE,
                    DiffIterateFlags.ITER_WANT_SCHEMA_ORDER);

                HashMap<String, Request> changes = new HashMap<String, Request>();
                try {
                    for (int subId : points) {
                        sub.diffIterate(subId, (CdbDiffIterate) this, enumSet, changes);
                    }
                } catch (Exception ignore) {
                    LOGGER.error("Error while handling IwanWorkFlowSub monitor " + ignore
                        .getMessage(), ignore);
                } finally {
                    sub.sync(CdbSubscriptionSyncType.DONE_PRIORITY);
                }
                // Read changes
                for (Map.Entry<String, Request> entry : changes.entrySet()) {
                    ConfTag tag = (ConfTag) entry.getValue().kp[1];
                    if (entry.getValue().op == DiffIterateOperFlag.MOP_CREATED || entry
                        .getValue().op == DiffIterateOperFlag.MOP_MODIFIED || entry
                            .getValue().op == DiffIterateOperFlag.MOP_VALUE_SET) {
                        entry.getValue().op = DiffIterateOperFlag.MOP_CREATED;
                    }
                    NavuNode node = entry.getValue().op == DiffIterateOperFlag.MOP_CREATED ? ncsRoot
                        .getNavuNode(new ConfPath(entry.getValue().kp)) : null;
                    if (entry.getValue().name.equalsIgnoreCase(
                        coreFpCommonTemplateMapper._ignore_override_settings_)) {
                        LOGGER.info(entry.getValue().op + " on " + entry.getKey() + " at global");
                        TemplateFinderFunction.init().readTemplateMappingChange(Type.TURN_OFF_FLAG,
                            entry.getKey(), entry.getValue().op, node);
                    } else if (tag
                        .getTagHash() == coreFpCommonTemplateMapper._service_template_mapper) {
                        LOGGER.info(entry.getValue().op + " on " + entry.getKey() + " at global");
                        TemplateFinderFunction.init().readTemplateMappingChange(Type.GLOBAL, entry
                            .getKey(), entry.getValue().op, node);
                    } else {
                        LOGGER.info(entry.getValue().op + " on " + entry.getKey() + " at provider");
                        TemplateFinderFunction.init().readTemplateMappingChange(Type.PROVIDER, entry
                            .getKey(), entry.getValue().op, node);
                    }
                }
            }
        } catch (Exception exp) {
            LOGGER.error("Exception while processing IwanWorkFlow cdb-sub" + exp.getMessage(), exp);
        } finally {
            LOGGER.info(" run end ");
            safeSocketClose(cdb);
        }
    }

    @Override
    public void finish() {
        try {
            if (maapi != null) {
                maapi.finishTrans(tid);
            }
        } catch (Exception ignore) {
        }
        try {
            if (maapi != null) {
                maapi.getSocket().close();
            }
        } catch (IOException ignore) {
        }
    }

    private void safeSocketClose(Cdb cdb) {
        if (cdb != null) {
            try {
                Socket s = cdb.getSocket();
                if (!s.isClosed()) {
                    cdb.getSocket().close();
                }
            } catch (IOException ignore) {
                LOGGER.error(ignore);
            }
        }
    }

    class Request {
        String name;
        DiffIterateOperFlag op;
        ConfObject[] kp;

        @Override
        public String toString() {
            return "Request [name=" + name + ", op=" + op + ", kp=" + Arrays.toString(kp) + "]";
        }
    }

    @Override
    public DiffIterateResultFlag iterate(ConfObject[] kp, DiffIterateOperFlag op,
        ConfObject oldValue, ConfObject newValue, Object initstate) {

        if (isHaSlave(maapi, tid)) {
            return DiffIterateResultFlag.ITER_STOP;
        }
        @SuppressWarnings("unchecked")
        HashMap<String, Request> changes = (HashMap<String, Request>) initstate;
        Request req = new Request();
        if (op != DiffIterateOperFlag.MOP_VALUE_SET) {
            ConfKey key = (ConfKey) kp[0];
            req.name = key.elementAt(0).toString();
        } else {
            req.name = coreFpCommonTemplateMapper._ignore_override_settings_;
        }
        req.op = op;
        req.kp = kp;
        changes.put(req.name, req);

        return DiffIterateResultFlag.ITER_CONTINUE;
    }

    private boolean isHaSlave(Maapi maapi, int th) {
        boolean isSalve = false;
        try {
            if (isHaEnabled(maapi, th)) {
                isSalve = !isMaster(maapi, th);
            }
        } catch (Exception e) {
            LOGGER.error("Failed reading HA mode:", e);
            isSalve = false;
        }
        return isSalve;
    }

    private boolean isMaster(Maapi maapi, int tid)

        throws ConfException, IOException {

        ConfEnumeration ha_mode_enum = (ConfEnumeration) maapi.getElem(tid,
            "/tfnm:ncs-state/ha/mode");

        String ha_mode = ConfEnumeration.getLabelByEnum("/tfnm:ncs-state/ha/mode", ha_mode_enum);

        if ("none".equals(ha_mode) || "normal".equals(ha_mode) || "master".equals(ha_mode)) {
            return true;
        }

        // slave or relay-slave
        return false;
    }

    private boolean isHaEnabled(Maapi maapi, int tid) throws ConfException, IOException {

        return maapi.exists(tid, "/tfnm:ncs-state/ha");
    }
}
