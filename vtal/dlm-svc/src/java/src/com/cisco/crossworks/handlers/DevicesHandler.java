package com.cisco.crossworks.handlers;

import com.cisco.nso.dao.model.ssh.cluster.Remote_node;
import com.cisco.nso.dao.model.ssh.devices.DeviceList;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceList;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import org.apache.log4j.Logger;

import java.io.IOException;

public class DevicesHandler implements Handler {
    private static final Logger log = Logger.getLogger(DevicesHandler.class);

    @Override
    public Object handle(NsoMessage message, MaapiSession ms) throws IOException, ConfException {
        DeviceList deviceList = ms.getRoot().getSsh().getDevices().getDevice();
        NsoDeviceList.Builder resultList = NsoDeviceList.newBuilder();

        if (deviceList != null) {
            for (com.cisco.nso.dao.model.ssh.devices.Device device : deviceList) {
                NsoDeviceData.Builder nsoDevice = NsoDeviceData.newBuilder();
                nsoDevice.setName(device.getName());
                String ip = device.getAddress();
                if (ip != null) {
                    nsoDevice.setIp(ip);
                } else {
                    //this is an RFS, fetch the ip from the cluster config
                    Remote_node remoteNode = ms.getRoot().getSsh().getCluster().getRemote_node().get(device.getName());
                    if (remoteNode != null) {
                        log.info("RFS node " + remoteNode.getName() + " was found!");
                        ip = remoteNode.getAddress();
                        if (ip != null) {
                            nsoDevice.setIp(ip);
                        } else {
                            log.error("RFS " + remoteNode.getName() + " has no ip");
                        }
                    }
                }
                try {
                    nsoDevice.setPort(device.getPort());
                } catch (Exception err) {
                    //Does not matter if there is no port
                }
                /*
                if (device.getService_list() != null) {
                    List<String> list = device.getService_list();
                    try {
                        for (int i = 0; i < list.size(); i++) {
                            String service = list.get(i);
                            if (service != null) {
                                nsoDevice.addServices(service);
                            }
                        }
                    } catch (Exception err) {
                        log.error("Failed to add services:", err);
                    }
                }
                */

                String ned = "";
                if (device.getDevice_type() != null &&
                        device.getDevice_type().getCli() != null &&
                        device.getDevice_type().getCli().getNed_id() != null) {
                    ned = device.getDevice_type().getCli().getNed_id();
                } else if (device.getDevice_type() != null &&
                        device.getDevice_type().getNetconf() != null &&
                        device.getDevice_type().getNetconf().getNed_id() != null) {
                    ned = device.getDevice_type().getNetconf().getNed_id();
                }
                nsoDevice.setNed(ned);
                nsoDevice.setProfile(device.getAuthgroup());
                resultList.addDeviceList(nsoDevice.build());
            }
        }
        return resultList.build();
    }

    @Override
    public String command() {
        return "devices";
    }

    @Override
    public boolean ReadOnly() {
        return true;
    }
}

