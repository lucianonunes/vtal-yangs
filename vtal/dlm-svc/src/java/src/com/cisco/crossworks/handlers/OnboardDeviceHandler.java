package com.cisco.crossworks.handlers;

import com.cisco.crossworks.encoder.Encoder;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceList;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import org.apache.log4j.Logger;

import java.io.IOException;

public class OnboardDeviceHandler implements Handler {
    private static final Logger log = Logger.getLogger(OnboardDeviceHandler.class);

    @Override
    public Object handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        log.info("Device Onboarding...");
        NsoDeviceList nsoDeviceList = (NsoDeviceList) Encoder.decode(msg.getData().toByteArray());

        try {
            NsoDeviceList.Builder result = NsoDeviceList.newBuilder();
            tryInBulk(nsoDeviceList, result, ms);
            return result.build();
        } catch (Exception e) {
            //Don't care as we are going to try one by one.
            if (ms != null) {
                ms.closeSession();
            }
        }

        NsoDeviceList.Builder result = NsoDeviceList.newBuilder();

        log.info("Bulk onboard has failed, trying 1 by 1...");
        for (NsoDeviceData nsoDeviceData : nsoDeviceList.getDeviceListList()) {
            try {
                ms = MaapiSession.newReadWriteMaapiSession();
                com.cisco.nso.dao.model.ssh.devices.DeviceList deviceList = ms.getRoot().getSsh().getDevices().getDevice();

                com.cisco.nso.dao.model.ssh.devices.Device newDevice = deviceList.add(nsoDeviceData.getName());
                newDevice.setAuthgroup(nsoDeviceData.getProfile());
                newDevice.setAddress(nsoDeviceData.getIp());
                newDevice.getDevice_type().getCli().setNed_id(nsoDeviceData.getNed());
                newDevice.getState().setAdmin_state("unlocked");

                ms.commitSession();

                NsoDeviceData.Builder builder = NsoDeviceData.newBuilder(nsoDeviceData);
                builder.setSuccess(true);
                builder.setMsg("");
                result.addDeviceList(builder.build());
                log.info("Onboarding device: " + nsoDeviceData.getName() + " IP: " + nsoDeviceData.getIp() + " was successful.");
            } catch (Exception e) {
                NsoDeviceData.Builder builder = NsoDeviceData.newBuilder(nsoDeviceData);
                builder.setSuccess(false);
                builder.setMsg(HandlerUtils.getErrorCause(e));
                result.addDeviceList(builder.build());
                log.info("Onboarding device: " + nsoDeviceData.getName() + " IP: " + nsoDeviceData.getIp() + " has failed.");
            } finally {
                if (ms != null) {
                    ms.closeSession();
                }
            }
        }
        return result.build();
    }

    public void tryInBulk(NsoDeviceList nsoDeviceList, NsoDeviceList.Builder result, MaapiSession ms) throws IOException, ConfException {
        log.info("Trying to onboard " + nsoDeviceList.getDeviceListList().size() + " devices in one transaction");
        com.cisco.nso.dao.model.ssh.devices.DeviceList deviceList = ms.getRoot().getSsh().getDevices().getDevice();
        for (NsoDeviceData nsoDeviceData : nsoDeviceList.getDeviceListList()) {
            log.info("Adding device " + nsoDeviceData.getName() + " to bulk");
            com.cisco.nso.dao.model.ssh.devices.Device newDevice = deviceList.add(nsoDeviceData.getName());
            newDevice.setAuthgroup(nsoDeviceData.getProfile());
            newDevice.setAddress(nsoDeviceData.getIp());
            if (nsoDeviceData.getNed().contains("-nc-")) {
                newDevice.getDevice_type().getNetconf().setNed_id(nsoDeviceData.getNed());
            } else {
                newDevice.getDevice_type().getCli().setNed_id(nsoDeviceData.getNed());
            }
            newDevice.getState().setAdmin_state("unlocked");
        }
        log.info("Finish adding " + nsoDeviceList.getDeviceListList().size() + " devices to transaction, committing it...");
        ms.commitSession();
        log.info("Successfully committed transaction with " + nsoDeviceList.getDeviceListList().size() + " devices.");
        for (NsoDeviceData nsoDeviceData : nsoDeviceList.getDeviceListList()) {
            NsoDeviceData.Builder builder = NsoDeviceData.newBuilder(nsoDeviceData);
            builder.setSuccess(true);
            builder.setMsg("");
            result.addDeviceList(builder.build());
        }
    }

    @Override
    public String command() {
        return "onboardDevices";
    }

    @Override
    public boolean ReadOnly() {
        return false;
    }
}
