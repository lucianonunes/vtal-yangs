package com.cisco.crossworks.handlers;

import com.cisco.crossworks.encoder.Encoder;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoCredentialList;
import com.cisco.robot.proto.nso_sp_api.NsoCredentialsData;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.tailf.conf.ConfException;
import com.tailf.navu.NavuContainer;
import org.apache.log4j.Logger;

import java.io.IOException;

public class OnboardCredentialsHandler implements Handler {
    private static final Logger log = Logger.getLogger(OnboardCredentialsHandler.class);

    @Override
    public String handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        log.info("Authgroups Onboarding...");
        NsoCredentialList credList = (NsoCredentialList) Encoder.decode(msg.getData().toByteArray());
        com.cisco.nso.dao.model.ssh.devices.authgroups.GroupList groupList = ms.getRoot().getSsh().getDevices().getAuthgroups().getGroup();
        for (NsoCredentialsData credData : credList.getCredentialsList()) {
            com.cisco.nso.dao.model.ssh.devices.authgroups.Group newGroup = groupList.add(credData.getName());
            try {
                NavuContainer cont = (NavuContainer) newGroup.getDefault_map().getNavuNode();
                cont.create();
            } catch (Exception err) {
                log.info("Error Creating container");
            }
            newGroup.getDefault_map().setRemote_name(credData.getUser());
            newGroup.getDefault_map().setRemote_password(credData.getPass());
        }
        log.info("" + credList.getCredentialsList().size() + " credentials were onboarded, committing transaction...");
        ms.commitSession();
        log.info("Transaction committed");
        return Handler.OK;
    }

    @Override
    public String command() {
        return "onboardCredentials";
    }

    @Override
    public boolean ReadOnly() {
        return false;
    }
}