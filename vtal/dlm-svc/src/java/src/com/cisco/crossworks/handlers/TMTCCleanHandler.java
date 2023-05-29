package com.cisco.crossworks.handlers;

import com.cisco.crossworks.encoder.Encoder;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceList;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import org.apache.log4j.Logger;

import java.io.IOException;

public class TMTCCleanHandler implements Handler {
    private static final Logger log = Logger.getLogger(TMTCCleanHandler.class);

    @Override
    public Object handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        log.info("TMTC Clean...");
        NsoDeviceList nsoDeviceList = (NsoDeviceList) Encoder.decode(msg.getData().toByteArray());
        com.cisco.nso.dao.model.ssh.devices.DeviceList deviceList = ms.getRoot().getSsh().getDevices().getDevice();
        NsoDeviceList.Builder result = NsoDeviceList.newBuilder();
        for (NsoDeviceData nsoDeviceData : nsoDeviceList.getDeviceListList()) {
            com.cisco.nso.dao.model.ssh.devices.Device device = deviceList.get(nsoDeviceData.getName());
            //@TODO TMTC clean
            HandlerUtils.setDeviceSucess(nsoDeviceData, result, true, "");
        }
        log.info("TMTC Clean was executed on " + nsoDeviceList.getDeviceListList().size() + " devices.");
        return result.build();
    }

    @Override
    public String command() {
        return "tmtc-clean";
    }

    @Override
    public boolean ReadOnly() {
        return true;
    }
}
