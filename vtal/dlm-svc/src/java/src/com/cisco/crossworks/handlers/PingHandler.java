package com.cisco.crossworks.handlers;

import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import org.apache.log4j.Logger;

import java.io.IOException;

public class PingHandler implements Handler {
    private static final Logger log = Logger.getLogger(PingHandler.class);

    @Override
    public String handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        return Handler.OK;
    }

    @Override
    public String command() {
        return "ping";
    }

    @Override
    public boolean ReadOnly() {
        return true;
    }
}