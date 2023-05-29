package com.cisco.crossworks.handlers;

import com.cisco.nso.dao.model.ssh.packages.PackageList;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.NsoMessage;
import com.cisco.robot.proto.nso_sp_api.NsoPackageData;
import com.cisco.robot.proto.nso_sp_api.NsoPackageList;
import com.tailf.conf.ConfException;

import java.io.IOException;

public class PackagesHandler implements Handler {
    @Override
    public Object handle(NsoMessage msg, MaapiSession ms) throws IOException, ConfException {
        PackageList packages = ms.getRoot().getSsh().getPackages().getPackage();
        NsoPackageList.Builder build = NsoPackageList.newBuilder();
        if (packages != null) {
            for (com.cisco.nso.dao.model.ssh.packages.Package p : packages) {
                NsoPackageData.Builder pkData = NsoPackageData.newBuilder();
                pkData.setPackageName(p.getName());
                build.addPackageData(pkData.build());
            }
        }
        return build.build();
    }

    @Override
    public String command() {
        return "packages";
    }

    @Override
    public boolean ReadOnly() {
        return true;
    }
}
