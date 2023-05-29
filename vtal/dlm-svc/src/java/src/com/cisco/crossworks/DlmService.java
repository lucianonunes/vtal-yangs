package com.cisco.crossworks;

import com.cisco.nso.dao.impl.DAOFactory;
import com.tailf.dp.DpCallbackException;
import com.tailf.dp.annotations.ServiceCallback;
import com.tailf.dp.proto.ServiceCBType;
import com.tailf.dp.services.ServiceContext;
import com.tailf.maapi.MaapiCrypto;
import com.tailf.maapi.MaapiException;
import com.tailf.navu.NavuNode;
import org.apache.log4j.Logger;

import java.net.BindException;
import java.net.Socket;
import java.util.Properties;

public class DlmService {
    private static final Logger log = Logger.getLogger(DlmService.class);
    private static DlmServer server = null;

    @ServiceCallback(servicePoint = "dlm-port-config", callType = ServiceCBType.CREATE)
    public Properties create(ServiceContext context, NavuNode service, NavuNode ncsRoot, Properties opaque) throws DpCallbackException {
        log.info("***** Executing the DLM Service Pack *****");
        if (server == null) {
            startServer();
        }
        com.cisco.nso.dao.model.cisco_dlm.Crosswork_vm conn = (com.cisco.nso.dao.model.cisco_dlm.Crosswork_vm) DAOFactory.create(service);
        try {
            MaapiCrypto mc = new MaapiCrypto(ncsRoot.context().getMaapi());
            DLMConnection.registerVM(conn.getUuid(), mc.decrypt(conn.getKey()), conn.getUsername(), mc.decrypt(conn.getPassword()));
        } catch (MaapiException e) {
            log.error(e);
        }

        return opaque;
    }

    public void startServer() throws DpCallbackException {
        synchronized (this.getClass()) {
            if (server == null) {
                try {
                    log.info("***** Starting Server *****");
                    server = new DlmServer();
                } catch (BindException e) {
                    try {
                        new Socket("127.0.0.1", 20244);
                    } catch (Exception err) {
                        log.error(err, err);
                    }

                    try {
                        Thread.sleep(1000);
                        server = new DlmServer();
                    } catch (Exception err) {
                        throw new DpCallbackException(err);
                    }
                } catch (Exception e) {
                    throw new DpCallbackException(e);
                }
            }
        }

    }

    @ServiceCallback(servicePoint = "dlm-sessions", callType = ServiceCBType.CREATE)
    public Properties clean(ServiceContext context, NavuNode service, NavuNode ncsRoot, Properties opaque) throws DpCallbackException {
        log.info("***** Executing the DLM Session Clean *****");
        DLMConnection.cleanMaapiSessions();
        return opaque;
    }
}
