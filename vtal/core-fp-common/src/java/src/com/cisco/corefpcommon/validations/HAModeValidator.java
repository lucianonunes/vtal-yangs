package com.cisco.corefpcommon.validations;

import com.tailf.conf.*;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.DpTrans;
import com.tailf.dp.annotations.TransValidateCallback;
import com.tailf.dp.annotations.ValidateCallback;
import com.tailf.dp.proto.TransValidateCBType;
import com.tailf.dp.proto.ValidateCBType;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;

import java.io.IOException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class HAModeValidator {
    private static final Logger LOGGER = LogManager.getLogger(HAModeValidator.class);

    @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE, qualifier = "haModeValidator")
    public Maapi maapi;

    @TransValidateCallback(callType = TransValidateCBType.INIT)
    public void init(DpTrans trans) throws DpCallbackException {
        LOGGER.info("Init method in HAModeValidator");

        // attach to the transaction
        try {
            String localUser = trans.getUserInfo().getUserName();
            maapi.startUserSession(localUser, maapi.getSocket().getInetAddress(), "system",
                new String[] {localUser}, MaapiUserSessionFlag.PROTO_TCP);

            maapi.attach(trans.getTransaction(), 0, trans.getUserInfo().getUserId());

            LOGGER.info("HAMode Contraint Compliance validator init");
        } catch (Exception e) { // IOException, MaapiException
            LOGGER.trace("Initialization error:", e);
            throw new DpCallbackException("failed to attach via maapi: " + e.getMessage());
        }
    }

    @TransValidateCallback(callType = {TransValidateCBType.STOP})
    public void stop(DpTrans trans) {
        LOGGER.info("Stop method in HAModeValidator");

        try {
            LOGGER.info("HAMode Constraint Compliance validator stop");
            maapi.detach(trans.getTransaction());
        } catch (Exception e) { // IOException, MaapiException
            LOGGER.trace("Callback validation transaction closing error:", e);
        }
    }

    @ValidateCallback(callPoint = "ha-mode-validation-hook", callType = {ValidateCBType.VALIDATE})
    public void validate(DpTrans trans, ConfObject[] kp, ConfValue newval) throws IOException,
        ConfException {

        LOGGER.info("ha-mode-validation-hook invoked from path: " + new ConfPath(kp).toString());

        int th = trans.getTransaction();

        // Throws error if HA enabled and HA node in non-master mode
        if (HAUtil.isHaSlave(maapi, th)) {
            String haMode = HAUtil.getHaMode(maapi, th);
            throw new DpCallbackException("Commits are not allowed for the given HA mode "
                + haMode);
        }

    }


}
