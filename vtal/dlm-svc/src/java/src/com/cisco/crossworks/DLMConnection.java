package com.cisco.crossworks;

import com.cisco.crossworks.encoder.Encoder;
import com.cisco.crossworks.encoder._Proto;
import com.cisco.crossworks.handlers.*;
import com.cisco.nso.dao.utils.MaapiSession;
import com.cisco.robot.proto.nso_sp_api.*;
import com.google.protobuf.ByteString;
import org.apache.log4j.Logger;

import java.io.*;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class DLMConnection implements Runnable {
    private static final Logger log = Logger.getLogger(DlmService.class);
    private Socket socket = null;
    private DataInputStream in = null;
    private DataOutputStream out = null;
    private boolean running = true;
    private String key = "";
    private String user = "";
    private String password = "";
    private String uuid = "";

    private final LinkedList<byte[]> incoming = new LinkedList<>();

    private static final Map<String, String> uuid2key = new HashMap<>();
    private static final Map<String, String> uuid2user = new HashMap<>();
    private static final Map<String, String> uuid2password = new HashMap<>();
    private static final Map<String, Handler> handlers = new HashMap<>();
    private static final Map<String, Map<Integer, MaapiSession>> uuid2Maapi = new HashMap<>();
    private static Integer nextMaapiId = 1;

    protected static final void registerVM(String host, String key, String user, String password) {
        synchronized (uuid2key) {
            uuid2key.put(host, key);
            uuid2password.put(host, password);
            uuid2user.put(host, user);
        }
    }

    protected static final String getKey(String host) {
        synchronized (uuid2key) {
            return uuid2key.get(host);
        }
    }

    protected static final String getUser(String host) {
        synchronized (uuid2key) {
            return uuid2user.get(host);
        }
    }

    protected static final String getPassword(String host) {
        synchronized (uuid2key) {
            return uuid2password.get(host);
        }
    }

    public DLMConnection(Socket socket) throws IOException {
        this.socket = socket;
        in = new DataInputStream(new BufferedInputStream(this.socket.getInputStream()));
        out = new DataOutputStream(new BufferedOutputStream(this.socket.getOutputStream()));
        Runnable worker = new Runnable() {
            @Override
            public void run() {
                processQueue();
            }
        };

        addHandler(new DevicesHandler());
        addHandler(new OnboardDeviceHandler());
        addHandler(new PackagesHandler());
        addHandler(new CredentialsHandler());
        addHandler(new OnboardCredentialsHandler());
        addHandler(new PingHandler());
        addHandler(new FetchSshKeysHandler());
        addHandler(new ConnectHandler());
        addHandler(new SyncFromHandler());
        addHandler(new SyncToHandler());
        addHandler(new TMTCCleanHandler());
        addHandler(new TMTCDeployHandler());
        addHandler(new CheckSyncHandler());
        addHandler(new CompareConfigHandler());

        _Proto.protoTypes.register(NsoDeviceList.newBuilder().build().getClass());
        _Proto.protoTypes.register(NsoDeviceData.newBuilder().build().getClass());
        _Proto.protoTypes.register(NsoCredentialList.newBuilder().build().getClass());
        _Proto.protoTypes.register(NsoCredentialsData.newBuilder().build().getClass());
        _Proto.protoTypes.register(NsoPackageData.newBuilder().build().getClass());
        _Proto.protoTypes.register(NsoPackageList.newBuilder().build().getClass());

        new Thread(worker).start();
        new Thread(this).start();
    }

    private void addHandler(Handler h) {
        handlers.put(h.command(), h);
    }

    private void executeConnectProtocol() throws IOException {
        byte[] sizeBytes = new byte[8];
        in.read(sizeBytes);
        long size = bytes2Long(sizeBytes);
        byte[] data = new byte[(int) size];
        in.read(data);

        String uuid = new String(data);
        String key = DLMConnection.getKey(uuid);
        String user = DLMConnection.getUser(uuid);
        String pass = DLMConnection.getPassword(uuid);
        if (key == null) {
            throw new IOException("Cannot find a key for uuid: " + uuid + ", rejecting connection.");
        }

        this.key = key;
        this.user = user;
        this.password = pass;
        this.uuid = uuid;

        sizeBytes = new byte[8];
        in.read(sizeBytes);
        size = bytes2Long(sizeBytes);
        data = new byte[(int) size];
        in.read(data);
        String encUuid = new String(data);

        String decUuid = "";
        try {
            decUuid = new String(Security.decrypt(encUuid, this.key));
        } catch (Exception e) {
            log.error(e, e);
        }
        if (!decUuid.equals(uuid)) {
            throw new IOException("Unable to decrypt Uuid:" + decUuid);
        }

        try {
            String encString = Security.encrypt(decUuid.getBytes(), this.key);
            sizeBytes = long2Bytes(encString.length());
            out.write(sizeBytes);
            out.write(encString.getBytes());
            out.flush();
        } catch (Exception e) {
            throw new IOException("Unable to encrypt Data");
        }
        data = new byte[1];
        in.read(data);
        if (data[0] != 97) {
            throw new IOException("Something wrong on the other side");
        }
        log.info("Connected from " + this.socket.getInetAddress().getHostAddress() + "!");
    }

    public void run() {
        try {
            executeConnectProtocol();
            while (running) {
                byte[] sizeBytes = new byte[8];
                in.readFully(sizeBytes);
                long size = bytes2Long(sizeBytes);
                byte[] data = new byte[(int) size];
                in.readFully(data);
                synchronized (this.incoming) {
                    this.incoming.add(data);
                    this.incoming.notifyAll();
                }
            }
        } catch (IOException e) {
            if (e instanceof EOFException) {
                log.info("DLM has closed the session.");
            } else {
                log.error(e, e);
            }
        } finally {
            try {
                if (this.in != null) {
                    this.in.close();
                }
                if (this.out != null) {
                    this.out.close();
                }
                if (this.socket != null) {
                    this.socket.close();
                }
            } catch (Exception err) {
            }
            synchronized (this) {
                running = false;
                this.notifyAll();
                socket = null;
                this.in = null;
                this.out = null;
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                    log.error(e);
                }
                this.incoming.clear();
                log.info("DLM Session has been cleared");
            }
        }
    }

    public void processQueue() {
        while (running) {
            byte[] data = null;
            synchronized (this) {
                if (this.incoming.size() == 0) {
                    try {
                        this.wait(2000);
                    } catch (InterruptedException e) {
                        log.error(e);
                    }
                }
                if (this.incoming.size() > 0) {
                    data = this.incoming.removeFirst();
                }
            }
            if (data != null) {
                boolean success = this.processCommand(data);
                if (!success) {
                    log.error("Failed to process command!!!");
                }
            }
        }
        log.info("End Processing Queue, Maapi cache size:" + uuid2Maapi.size());
        for (Map.Entry<String, Map<Integer, MaapiSession>> entry : uuid2Maapi.entrySet()) {
            log.info("Stat:" + entry.getKey() + " size:" + entry.getValue().size());
        }
    }

    public boolean processCommand(byte[] data) {
        log.info("Processing Command");
        NsoMessage msg = null;
        try {
            byte[] msgData = Security.decrypt(new String(data), this.key);
            synchronized (this) {
                msg = NsoMessage.parseFrom(msgData);
            }
            log.info("Command:" + msg.getTopic());
        } catch (Exception e) {
            log.error("Failed to unmarshal message", e);
            return false;
        }

        try {
            Handler handler = handlers.get(msg.getTopic());
            MaapiSession.setNcsUsername(this.user);
            MaapiSession.setNcsPassword(this.password);

            NsoMessage.Builder reply = NsoMessage.newBuilder();
            reply.setId(msg.getId());
            reply.setTopic(msg.getTopic());

            if (handler == null) {
                log.error("Unknown Command : " + msg.getTopic());
                reply.setError("Unknown Command : " + msg.getTopic());
            } else {
                MaapiSession ms = null;
                try {
                    if (handler.ReadOnly()) {
                        ms = MaapiSession.newReadOnlyMaapiSession();
                    } else {
                        ms = MaapiSession.newReadWriteMaapiSession();
                    }
                    if (ms.Closed()) {
                        if (handler.ReadOnly()) {
                            log.error("DLM FP failed to start a Read transaction");
                        } else {
                            log.error("DLM FP failed to start a Read/Write transaction");
                        }
                        return false;
                    }
                    Integer id = DLMConnection.addMaapi(this.uuid, ms);
                    Object response = handler.handle(msg, ms);
                    DLMConnection.delMaapi(this.uuid, id);
                    byte[] replyData = Encoder.encode(response);
                    ByteString b = ByteString.copyFrom(replyData);
                    reply.setData(b);
                } finally {
                    if (ms != null) {
                        ms.closeSession();
                    }
                }
            }

            log.info("Replying to command:" + msg.getTopic());

            String encString = Security.encrypt(reply.build().toByteArray(), this.key);
            byte[] sizeBytes = long2Bytes(encString.length());
            synchronized (this) {
                out.write(sizeBytes);
                out.flush();
                out.write(encString.getBytes());
                out.flush();
            }
            return true;
        } catch (Exception e) {
            if (msg != null) {
                log.error("Failed to reply to command:" + msg.getTopic(), e);
            } else {
                log.error("Unable to reply to command", e);
            }
            return false;
        }
    }

    public static final byte[] long2Bytes(long value) {
        byte[] data = new byte[8];
        data[0] = (byte) ((value >> 56) & 0xff);
        data[1] = (byte) ((value >> 48) & 0xff);
        data[2] = (byte) ((value >> 40) & 0xff);
        data[3] = (byte) ((value >> 32) & 0xff);
        data[4] = (byte) ((value >> 24) & 0xff);
        data[5] = (byte) ((value >> 16) & 0xff);
        data[6] = (byte) ((value >> 8) & 0xff);
        data[7] = (byte) ((value) & 0xff);
        return data;
    }

    public static final long bytes2Long(byte[] data) {
        return (long) (0xff & data[0]) << 56
                | (long) (0xff & data[1]) << 48
                | (long) (0xff & data[2]) << 40
                | (long) (0xff & data[3]) << 32
                | (long) (0xff & data[4]) << 24
                | (long) (0xff & data[5]) << 16
                | (long) (0xff & data[6]) << 8
                | (long) (0xff & data[7]);
    }

    public static void main(String args[]) {
        long l1 = System.currentTimeMillis();
        byte[] data = long2Bytes(l1);
        long l2 = bytes2Long(data);
        if (l1 == l2) {
            System.out.println("OK");
        }
    }

    public synchronized static Integer addMaapi(String uuid, MaapiSession ms) {
        nextMaapiId++;
        Integer result = nextMaapiId;
        Map<Integer, MaapiSession> map = uuid2Maapi.get(uuid);
        if (map == null) {
            map = new HashMap<>();
            uuid2Maapi.put(uuid, map);
        }
        map.put(nextMaapiId, ms);
        return result;
    }

    public synchronized static void delMaapi(String uuid, Integer id) {
        Map<Integer, MaapiSession> map = uuid2Maapi.get(uuid);
        if (map != null) {
            map.remove(id);
        }
    }

    public synchronized static void cleanMaapiSessions() {
        for (Map.Entry<String, Map<Integer, MaapiSession>> uuidEntry : uuid2Maapi.entrySet()) {
            for (Map.Entry<Integer, MaapiSession> maapiEntry : uuidEntry.getValue().entrySet()) {
                maapiEntry.getValue().closeSession(true);
            }
        }
    }
}

