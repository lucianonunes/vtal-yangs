/*
 ***************************************************
 * Copyright (c) 2019 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.corefpcommon.statuscodes;

import java.io.IOException;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfObject;
import com.tailf.conf.ConfPath;
import com.tailf.conf.ConfValue;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.DpTrans;
import com.tailf.dp.annotations.TransValidateCallback;
import com.tailf.dp.annotations.ValidateCallback;
import com.tailf.dp.proto.TransValidateCBType;
import com.tailf.dp.proto.ValidateCBType;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.ns.Ncs;

/**
 * Status Codes validator and cli completion utility class
 *
 * @author abhatta
 */
public class StatusCodeValidator {
    private static final Logger LOGGER = LogManager.getLogger(StatusCodeValidator.class);

    @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE,
        qualifier = "status-code-validator")
    private Maapi maapi;

    private int th = -1;

    @TransValidateCallback(callType = TransValidateCBType.INIT)
    public void init(DpTrans trans) {
        try {
            maapi.startUserSession("admin", maapi.getSocket().getInetAddress(), "system",
                new String[] {}, MaapiUserSessionFlag.PROTO_TCP);
            th = trans.getTransaction();
            maapi.attach(th, new Ncs().hash(), trans.getUserInfo().getUserId());
        } catch (ConfException | IOException e) {
            LOGGER.error("Failed to attach via maapi: " + e.getMessage(), e);
        }
    }

    @ValidateCallback(callPoint = "status-code-validation", callType = {ValidateCBType.VALIDATE})
    public void validate(DpTrans trans, ConfObject[] kp, ConfValue newval)
        throws DpCallbackException {
        try {
            LOGGER.debug("Status code validation ==>" + Conf.kpToString(kp));
            NavuNode ncsRoot = new NavuContainer(maapi, th, Ncs.hash);
            NavuNode cfpNode = ncsRoot.getParent().getNavuNode(new ConfPath(kp));
            if (StatusCodeUtil.isJavaEnum(cfpNode)) {
                StatusCodeUtil.validateCfpStatusCodes(cfpNode);
            }
        } catch (DpCallbackException e) {
            throw e;
        } catch (NavuException e) {
            LOGGER.error("Unexpected error: ", e);
        }
    }

    @TransValidateCallback(callType = {TransValidateCBType.STOP})
    public void stop(DpTrans trans) {
        try {
            maapi.detach(trans.getTransaction());
        } catch (ConfException | IOException e) {
            LOGGER.warn("", e);
        }
    }
}
