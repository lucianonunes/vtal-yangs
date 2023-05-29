package com.cisco.crossworks.encoder;

import com.google.protobuf.GeneratedMessageV3;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.Map;

public class Encoder {
    private static final Logger log = Logger.getLogger(Encoder.class);
    private static final Map<String, TypeEncoder> encoders = new HashMap();
    private static final Map<Integer, TypeEncoder> encodersKind = new HashMap();
    private byte[] data;
    private Location location;
    protected static final _Int32 int32Encoder = new _Int32();
    protected static final _String stringEncoder = new _String();

    static {
        init();
    }

    private static final void init() {
        registerEncoder(Integer.class.getSimpleName(), new _Int32());
        registerEncoder(String.class.getSimpleName(), new _String());
        registerEncoder("slice", new _Slice());
        registerEncoder("proto", new _Proto());
    }

    private static final void registerEncoder(String name, TypeEncoder enc) {
        encoders.put(name, enc);
        encodersKind.put(enc.kind(), enc);
    }

    protected static interface TypeEncoder {
        public byte[] add(Object object, Location location);

        public Object get(byte[] data, Location location);

        public int kind();
    }

    protected static class Location {
        protected int location;
    }

    public Encoder() {
        this.data = new byte[0];
        this.location = new Location();
    }

    public Encoder(byte[] data, int location) {
        this.data = data;
        this.location = new Location();
        this.location.location = location;
    }

    public byte[] getData() {
        return this.data;
    }

    public Location getLocation() {
        return this.location;
    }

    public void reset() {
        this.location = new Location();
    }

    public final void Add(Object obj) {
        String name = obj.getClass().getSimpleName();
        if (obj.getClass().isArray()) {
            name = "slice";
        } else if (GeneratedMessageV3.class.isAssignableFrom(obj.getClass())) {
            name = "proto";
        }
        TypeEncoder enc = encoders.get(name);
        if (enc != null) {
            this.data = addBytes(this.data, int32Encoder.add(enc.kind(), this.location));
            this.data = addBytes(this.data, enc.add(obj, this.location));
        } else {
            log.error("Did not find any encoder for " + name);
        }
    }

    public final Object Get() {
        int k = (int) int32Encoder.get(this.data, this.location);
        TypeEncoder enc = encodersKind.get(k);
        if (enc != null) {
            return enc.get(this.data, this.location);
        } else {
            log.error("Did not find any encoder for kind " + k);
        }
        return null;
    }

    protected static byte[] addBytes(byte[] data, byte[] added) {
        byte[] result = new byte[data.length + added.length];
        System.arraycopy(data, 0, result, 0, data.length);
        System.arraycopy(added, 0, result, data.length, added.length);
        return result;
    }

    protected static final Class getClass(Object obj) {
        Class cls = obj.getClass();
        if (cls.equals(Integer.class)) {
            cls = int.class;
        }
        return cls;
    }

    public static final Object decode(byte[] data) {
        Encoder enc = new Encoder(data, 0);
        return enc.Get();
    }

    public static final byte[] encode(Object obj) {
        Encoder enc = new Encoder();
        enc.Add(obj);
        return enc.getData();
    }
}
