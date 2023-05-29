package com.cisco.corefpcommon.upgrade;

import java.io.IOException;
import java.net.Socket;
import java.util.EnumSet;
import com.cisco.corefpcommon.namespaces.coreFpCommon;
import com.tailf.cdb.Cdb;
import com.tailf.cdb.CdbDBType;
import com.tailf.cdb.CdbLockType;
import com.tailf.cdb.CdbUpgradeSession;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfCdbUpgradePath;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfValue;
import com.tailf.maapi.Maapi;
import com.tailf.ncs.NcsMain;
import java.net.InetAddress;

/**
 * @author abhatta
 *
 */
public class GlobalVarUpgrade {
    private static final String PS = "/";
    private static final String GLOBAL_VAR_OLD_PATH = PS + coreFpCommon.prefix + ":"
        + coreFpCommon._global_var_;
    private static final String GLOBAL_VAR_OLD_ITEM = GLOBAL_VAR_OLD_PATH + "[%d]" + PS;
    private static final String GLOBAL_VAR_NEW_PATH = GLOBAL_VAR_OLD_PATH + PS + coreFpCommon._var_
        + "{%s}";
    private static final String GLOBAL_VAR_NEW_ITEM = GLOBAL_VAR_NEW_PATH + PS;

    public GlobalVarUpgrade() {
        /*
         * Empty Constructor. Comments added for SONAR
         */ 
    }

    public static void main(String[] args) throws Exception {
        System.out.println("***********Upgrading global-var**************");
        Socket s1 = null;
        Socket s2 = null;
        try{ // NOSONAR
            String host = InetAddress.getByName("localhost").getHostAddress();
            int port = Integer.parseInt(System.getenv("NCS_IPC_PORT"));
            s1 = new Socket(host, port);            
            Cdb cdb = new Cdb("global-var-upgrade", s1);
            cdb.setUseForCdbUpgrade();
            CdbUpgradeSession cdbsess = cdb.startUpgradeSession(CdbDBType.CDB_RUNNING, EnumSet.of(
                CdbLockType.LOCK_SESSION, CdbLockType.LOCK_WAIT));
            int instanceNo = getInstanceCount(cdbsess);

            if (instanceNo > 0) {
                s2 = new Socket(host, port);
                Maapi maapi = new Maapi(s2);
                int th = maapi.attachInit();
                for (int i = 0; i < instanceNo; i++) {
                    Integer offset = Integer.valueOf(i);
                    ConfValue varName = cdbsess.getElem(GLOBAL_VAR_OLD_ITEM + coreFpCommon._name_,
                        offset);
                    maapi.safeCreate(th, GLOBAL_VAR_NEW_PATH, varName.toString());
                    ConfValue value = cdbsess.getElem(GLOBAL_VAR_OLD_ITEM + coreFpCommon._value_,
                        offset);
                    if (null != value) {
                        maapi.setElem(th, value, GLOBAL_VAR_NEW_ITEM + coreFpCommon._value_, varName
                            .toString());
                    } else {
                        ConfValue encryptValue = cdbsess.getElem(GLOBAL_VAR_OLD_ITEM
                            + coreFpCommon._encrypted_value_, offset);
                        maapi.setElem(th, encryptValue, GLOBAL_VAR_NEW_ITEM
                            + coreFpCommon._encrypted_value_, varName.toString());
                    }
                }
            }
        } catch (Exception e) { // NOSONAR
            System.err.println(e.getMessage()); // NOSONAR
        }finally{
            closeSocket(s1);
            closeSocket(s2);
        }
    }
    

    private static void closeSocket(Socket s) {
        try {
            if (null != s) {
                s.close();
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

    private static int getInstanceCount(CdbUpgradeSession cdbsess) {
        int count = 0;
        try {
            count = cdbsess.getNumberOfInstances(new ConfCdbUpgradePath(
                GLOBAL_VAR_OLD_PATH));
        } catch (ConfException | IOException e) {
            System.out.println("No Upgrade for " + GLOBAL_VAR_OLD_PATH);
        }
        System.out.println(GLOBAL_VAR_OLD_PATH + " instance count: " + count);
        return count;
    }

}
