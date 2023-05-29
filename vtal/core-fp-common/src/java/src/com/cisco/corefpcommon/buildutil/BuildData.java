package com.cisco.corefpcommon.buildutil;

import java.io.IOException;
import java.net.Socket;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.Conf;
import com.tailf.conf.ConfException;
import com.tailf.conf.ConfKey;
import com.tailf.maapi.Maapi;
import com.tailf.maapi.MaapiCursor;
import com.tailf.maapi.MaapiUserSessionFlag;
import com.tailf.ncs.ApplicationComponent;
import com.tailf.ncs.annotations.Resource;
import com.tailf.ncs.annotations.ResourceType;
import com.tailf.ncs.annotations.Scope;
import com.tailf.ncs.NcsMain;


public class BuildData implements ApplicationComponent {

    @Resource(type = ResourceType.MAAPI, scope = Scope.INSTANCE)
    private Maapi maapi = null;
    private static final Logger LOGGER = LogManager.getLogger(BuildData.class);
    private int th;

    @Override
    public void init() throws Exception {
        Socket socket = new Socket(NcsMain.getInstance().getNcsHost(), NcsMain.getInstance()
            .getNcsPort());
        maapi = new Maapi(socket);
        maapi.startUserSession("", maapi.getSocket().getInetAddress(), "system", new String[] {""},
            MaapiUserSessionFlag.PROTO_TCP);
        th = maapi.startTrans(Conf.DB_RUNNING, Conf.MODE_READ);
    }

    @Override
    public void run() {
        String path = "/core-fp-release/release/function-pack-store";
        String version = null;
        try {
            MaapiCursor c = maapi.newCursor(th, path);
            ConfKey k = maapi.getNext(c);
            while (k != null) {
                String key = k.elementAt(0).toString();
                String versionPath = "/core-fp-release/release/function-pack-store{" + key
                    + "}/version";
                version = maapi.getElem(th, versionPath).toString();
                if (LOGGER.isInfoEnabled()) {
                    LOGGER.info("Version: " + version);
                }
                k = maapi.getNext(c);
            }
        } catch (IOException | ConfException e) {
            LOGGER.error("Error in BuildData", e);
        }
    }

    @Override
    public void finish() throws Exception {
        try {
            if (maapi != null && th != 0) {
                maapi.finishTrans(th);
            }
        } catch (IOException | ConfException e) {
            LOGGER.warn("Failed to finish transaction in BuildData" + e);
        }
        try {
            if (maapi != null) {
                maapi.getSocket().close();
            }
        } catch (IOException e) {
            LOGGER.error("Failed to close socket due to error", e);
        }
    }
}
