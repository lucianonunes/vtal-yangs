package com.cisco.crossworks.encoder;

public class _String implements Encoder.TypeEncoder {

    @Override
    public byte[] add(Object object, Encoder.Location location) {
        byte[] strBytes = ((String) object).getBytes();
        byte[] data = Encoder.int32Encoder.add(strBytes.length, location);
        data = Encoder.addBytes(data, strBytes);
        location.location += strBytes.length;
        return data;
    }

    @Override
    public Object get(byte[] data, Encoder.Location location) {
        int strSize = (int) Encoder.int32Encoder.get(data, location);
        byte[] strData = new byte[strSize];
        System.arraycopy(data, location.location, strData, 0, strSize);
        location.location += strData.length;
        return new String(strData);
    }

    @Override
    public int kind() {
        return 24;
    }
}
