package com.cisco.crossworks.handlers;

import com.cisco.nso.dao.model.ssh.devices.authgroups.GroupList;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoCredentialList;
import com.cisco.robot.proto.nso_sp_api.NsoCredentialsData;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import com.tailf.maapi.MaapiCrypto;
import com.tailf.maapi.MaapiException;
import org.apache.log4j.Logger;

import java.io.IOException;

public class CredentialsHandler implements Handler {
    private static final Logger log = Logger.getLogger(CredentialsHandler.class);

    @Override
    public Object handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        MaapiCrypto mc = null;
        try {
            mc = new MaapiCrypto(ms.getMaapi());
        } catch (MaapiException e) {
            log.error("Failed to create Maapi Crypto", e);
        }
        GroupList groupList = ms.getRoot().getSsh().getDevices().getAuthgroups().getGroup();
        NsoCredentialList.Builder result = NsoCredentialList.newBuilder();

        if (groupList != null) {
            for (com.cisco.nso.dao.model.ssh.devices.authgroups.Group group : groupList) {
                String username = group.getDefault_map().getRemote_name();
                if (username != null) {
                    NsoCredentialsData.Builder cred = NsoCredentialsData.newBuilder();
                    cred.setName(group.getName());
                    cred.setUser(group.getDefault_map().getRemote_name());
                    if (mc != null) {
                        cred.setPass(mc.decrypt(group.getDefault_map().getRemote_password()));
                    } else {
                        cred.setPass(group.getDefault_map().getRemote_password());
                    }
                    result.addCredentials(cred.build());
                }
            }
        }
        return result.build();
    }

    @Override
    public String command() {
        return "credentials";
    }

    @Override
    public boolean ReadOnly() {
        return true;
    }
}