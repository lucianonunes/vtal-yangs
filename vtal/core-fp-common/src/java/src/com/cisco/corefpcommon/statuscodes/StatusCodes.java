/*
 ***************************************************
 * Copyright (c) 2019 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.corefpcommon.statuscodes;

import java.io.IOException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.corefpcommon.namespaces.coreFpCommonStatusCodes;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfIdentityRef;
import com.tailf.conf.ConfPath;
import com.tailf.dp.DpCallbackException;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.ns.Ncs;

/**
 * StatusCodes to validate status codes on package reload and provide utility method to fetch a
 * specific status code from cdb.
 *
 * @author abhatta
 */
public class StatusCodes implements ApplicationComponent {
    private static final Logger LOGGER = LogManager.getLogger(StatusCodes.class);

    @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE, qualifier = "status-codes-maapi")
    private Maapi maapi;

    private int tid = -1;
    private static StatusCodes statusCodes;

    @Override
    public void init() throws DpCallbackException {
        try {
            startMaapiTransaction();
            StatusCodeUtil.validateStatusCodes(maapi, tid);
            statusCodes = this; // NOSONAR
        } catch (DpCallbackException e) {
            throw e;
        } catch (ConfException | IOException e) {
            LOGGER.error("Error while initializing Status Code Subscriber: ", e);
        }
    }

    @Override
    public void run() {
        // Sonar fix
    }

    @Override
    public void finish() throws Exception {
        try {
            if (tid != -1 && maapi != null) {
                maapi.finishTrans(tid);
                maapi.getSocket().close();
            }
        } catch (IOException | ConfException e) {
            LOGGER.warn("" + e);
        }
    }

    /**
     * Get a StatusCode from cdb with core-function-pack name and status code.
     *
     * @param cfpName: core function pack name
     * @param statusName: status code value maintained by core function pack
     * @return StatusCode
     */
    public static StatusCode getStatusCode(String cfpName, String code) {
        StatusCode statusCode = new StatusCode(cfpName, code);
        try {
            /*
             * If StatusCodes is not initialized due to error in other Application Component class.
             */
            statusCodes.startMaapiTransaction();

            String statusCodePath = "/" + coreFpCommonStatusCodes.prefix + ":"
                + coreFpCommonStatusCodes._status_codes_ + "/"
                + coreFpCommonStatusCodes._core_function_pack_ + "{%s}/"
                + coreFpCommonStatusCodes._status_code_ + "{%s}";
            if (statusCodes.maapi.exists(statusCodes.tid, statusCodePath, cfpName, code)) {
                NavuNode ncsRoot = new NavuContainer(statusCodes.maapi, statusCodes.tid, Ncs.hash);
                NavuNode statusCodeNode = ncsRoot.getParent().getNavuNode(new ConfPath(
                    statusCodePath, cfpName, code));
                statusCode.setReason(statusCodeNode.leaf(coreFpCommonStatusCodes._reason_)
                    .valueAsString());
                statusCode.setCategory(statusCodeNode.leaf(coreFpCommonStatusCodes._category_)
                    .valueAsString());
                String severity = ((ConfIdentityRef) statusCodeNode.leaf(
                    coreFpCommonStatusCodes._severity_).value()).getTag();
                statusCode.setSeverity(severity);
                if (null != statusCodeNode.leaf(coreFpCommonStatusCodes._recommended_actions_)
                    .value()) {
                    statusCode.setRecommendedAction(statusCodeNode.leaf(
                        coreFpCommonStatusCodes._recommended_actions_).valueAsString());
                }
            }
        } catch (ConfException | IOException e) {
            LOGGER.error("", e);
        }
        return statusCode;
    }

    private void startMaapiTransaction() throws IOException, ConfException {
        if (tid == -1) {
            maapi.startUserSession("admin", maapi.getSocket().getInetAddress(), "system",
                new String[] {"admin"}, MaapiUserSessionFlag.PROTO_TCP);
            tid = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
        }
    }
}
