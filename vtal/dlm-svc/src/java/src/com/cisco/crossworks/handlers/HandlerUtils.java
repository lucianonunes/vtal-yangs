package com.cisco.crossworks.handlers;

import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceList;
import com.tailf.conf.ConfXMLParam;
import org.apache.log4j.Logger;

public class HandlerUtils {
    private static final Logger log = Logger.getLogger(HandlerUtils.class);

    public static final void setDeviceSucess(NsoDeviceData nsoDeviceData, NsoDeviceList.Builder result, boolean b, String msg) {
        NsoDeviceData.Builder deviceBuilder = NsoDeviceData.newBuilder(nsoDeviceData);
        deviceBuilder.setSuccess(b);
        deviceBuilder.setMsg(msg);
        result.addDeviceList(deviceBuilder.build());
    }

    public static String getErrorMessage(ConfXMLParam[] opResult) {
        String msg = "";
        String status = "";
        if (opResult != null) {
            for (ConfXMLParam param : opResult) {
                log.info("param:" + param.getTag() + " : " + param.getValue());
                if (param.getTag().toLowerCase().equals("info") || param.getTag().toLowerCase().equals("diff")) {
                    msg = param.getValue().toString();
                } else if (param.getTag().equals("result")) {
                    status = param.getValue().toString();
                }
            }
        }
        if (status.equals("Enum(1)") || status.equals("true")) {
            return "";
        } else if (status.equals("Enum(0)")) {
            return "Unknown";
        }
        return msg;
    }

    public static String getErrorCause(Exception e) {
        if (e.getMessage() != null) {
            log.error("There was an Exception in NSO:", e);
            return "NSO Exception:" + e.getMessage();
        }
        if (e.getCause() != null && e.getCause().getMessage() != null) {
            log.error("There was an Exception in NSO:", e);
            return "NSO Exception:" + e.getCause().getMessage();
        }
        log.error("There was an Exception in NSO:", e);
        return "NSO Exception:" + e.toString();
    }
}
