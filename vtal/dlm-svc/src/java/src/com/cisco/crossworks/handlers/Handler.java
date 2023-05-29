package com.cisco.crossworks.handlers;

import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;

import java.io.IOException;

public interface Handler {
    public static final String OK = "OK";

    public Object handle(NsoMessage message, MaapiSession ms) throws IOException, ConfException;

    public String command();

    public boolean ReadOnly();
}
