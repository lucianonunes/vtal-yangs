package com.cisco.crossworks.handlers;

import com.cisco.crossworks.encoder.Encoder;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceList;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfXMLParam;
import org.apache.log4j.Logger;

import java.io.IOException;

public class FetchSshKeysHandler implements Handler {
    private static final Logger log = Logger.getLogger(FetchSshKeysHandler.class);

    @Override
    public Object handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        log.info("Device Fetch Ssh Keys...");
        NsoDeviceList nsoDeviceList = (NsoDeviceList) Encoder.decode(msg.getData().toByteArray());
        com.cisco.nso.dao.model.ssh.devices.DeviceList deviceList = ms.getRoot().getSsh().getDevices().getDevice();
        NsoDeviceList.Builder result = NsoDeviceList.newBuilder();
        for (NsoDeviceData nsoDeviceData : nsoDeviceList.getDeviceListList()) {
            com.cisco.nso.dao.model.ssh.devices.Device device = deviceList.get(nsoDeviceData.getName());
            if (device == null) {
                HandlerUtils.setDeviceSucess(nsoDeviceData, result, false, "Device has no provider or provider was removed.");
            } else
                try {
                    log.info("Executing Fetch Ssh Keys for device: " + device.getName());
                    ConfXMLParam[] opr = ms.getMaapi().requestAction(new ConfXMLParam[0], "/ncs:devices/device{%s}/ssh/fetch-host-keys", device.getName());
                    String errMsg = HandlerUtils.getErrorMessage(opr);
                    if (errMsg.equals("")) {
                        HandlerUtils.setDeviceSucess(nsoDeviceData, result, true, "");
                    } else {
                        HandlerUtils.setDeviceSucess(nsoDeviceData, result, false, errMsg);
                    }
                } catch (Exception e) {
                    log.info("Fetch ssh keys has failed for device " + device.getName() + " with the following exception:", e);
                    HandlerUtils.setDeviceSucess(nsoDeviceData, result, false, HandlerUtils.getErrorCause(e));
                }
        }
        log.info("Ssh Fetch Keys was executed on " + nsoDeviceList.getDeviceListList().size() + " devices.");
        return result.build();
    }

    @Override
    public String command() {
        return "fetch-ssh-keys";
    }

    @Override
    public boolean ReadOnly() {
        return true;
    }
}
