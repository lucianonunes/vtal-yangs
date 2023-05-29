package com.cisco.crossworks.encoder;

import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;
import com.google.protobuf.GeneratedMessageV3;
import org.apache.log4j.Logger;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

public class _Proto implements Encoder.TypeEncoder {
    private static final Logger log = Logger.getLogger(_Proto.class);
    public static final ProtoTypes protoTypes = new ProtoTypes();

    public static final class ProtoTypes {
        private Map<String, Class> protos = new HashMap();

        public synchronized void register(Class cls) {
            this.protos.put(cls.getSimpleName(), cls);
        }

        public synchronized Class get(String name) {
            return this.protos.get(name);
        }
    }

    @Override
    public byte[] add(Object object, Encoder.Location location) {
        if (object == null) {
            return Encoder.int32Encoder.add(-1, location);
        }
        String name = object.getClass().getSimpleName();
        Class protoClass = protoTypes.get(name);
        if (protoClass == null) {
            log.error("Unable to find class for " + name);
            return null;
        }

        byte[] typeNameData = Encoder.stringEncoder.add(name, location);
        GeneratedMessageV3 proto = (GeneratedMessageV3) object;
        byte[] protoData = proto.toByteArray();
        byte[] protoSize = Encoder.int32Encoder.add(protoData.length, location);

        byte[] data = Encoder.addBytes(typeNameData, protoSize);
        data = Encoder.addBytes(data, protoData);

        location.location += protoData.length;
        return data;
    }

    @Override
    public Object get(byte[] data, Encoder.Location location) {
        int locStart = location.location;
        String name = (String) Encoder.stringEncoder.get(data, location);
        int size = (int) Encoder.int32Encoder.get(data, location);

        Class protoClass = protoTypes.get(name);
        if (protoClass == null) {
            log.error("Unable to find class for " + name);
            return null;
        }

        try {
            Method m = protoClass.getMethod("parseFrom", new Class[]{byte[].class});
            byte[] protoData = new byte[size];
            System.arraycopy(data, location.location, protoData, 0, size);
            return m.invoke(null, protoData);
        } catch (Exception err) {
            err.printStackTrace();
        }
        return null;
    }

    @Override
    public int kind() {
        return 22;
    }
}