package com.cisco.crossworks.encoder;

public class _Int32 implements Encoder.TypeEncoder {

    @Override
    public byte[] add(Object object, Encoder.Location location) {
        byte[] data = new byte[4];
        int value = (int) object;
        data[3] = (byte) ((value >> 24) & 0xff);
        data[2] = (byte) ((value >> 16) & 0xff);
        data[1] = (byte) ((value >> 8) & 0xff);
        data[0] = (byte) ((value) & 0xff);
        location.location += 4;
        return data;
    }

    @Override
    public Object get(byte[] data, Encoder.Location location) {
        int value = 0;
        value = (0xff & data[3 + location.location]) << 24
                | (0xff & data[2 + location.location]) << 16
                | (0xff & data[1 + location.location]) << 8
                | (0xff & data[location.location]);
        location.location += 4;
        return value;
    }

    @Override
    public int kind() {
        return 5;
    }
}
