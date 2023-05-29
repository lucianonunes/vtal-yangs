package com.cisco.crossworks;

import org.apache.log4j.Logger;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class DlmServer implements Runnable {
    private static final Logger log = Logger.getLogger(DlmService.class);
    private ServerSocket server = null;
    private ServerSocket control = null;
    private boolean running = true;

    public DlmServer() throws IOException {
        this.server = new ServerSocket(20243);
        this.control = new ServerSocket(20244);
        Runnable runthis = new Runnable() {
            @Override
            public void run() {
                control();
            }
        };
        new Thread(runthis).start();
        new Thread(this).start();
    }

    public void run() {
        try {
            while (running) {
                log.info("Waiting for connection....");
                Socket socket = server.accept();
                new DLMConnection(socket);
            }
        } catch (IOException e) {
            log.info("DLM Server was shutdown");
        }
        log.info("Server was closed");
    }

    public void control() {
        try {
            log.info("wating on controll");
            this.control.accept();
            log.info("Finalize");
            this.finalize();
        } catch (Throwable e) {
        }
    }

    public void finalize() throws Throwable {
        this.running = false;
        log.info("**** Finalize ****");
        if (this.server != null) {
            this.server.close();
        }
        if (this.control != null) {
            this.control.close();
        }
    }
}
