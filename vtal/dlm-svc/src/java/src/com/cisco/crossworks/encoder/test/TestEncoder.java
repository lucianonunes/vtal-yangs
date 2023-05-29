package com.cisco.crossworks.encoder.test;

import com.cisco.crossworks.encoder.Encoder;
import com.cisco.crossworks.encoder._Proto;
import com.cisco.robot.proto.nso_sp_api.NsoDeviceData;

public class TestEncoder {
    public static void main(String args[]) {
        testObject(-56);
        testObject("Hello World");
        int[] val = new int[3];
        val[0] = 1;
        val[1] = 2;
        val[2] = 3;
        testObject(val);

        NsoDeviceData.Builder deviceData = NsoDeviceData.newBuilder();
        deviceData.setName("Hello World");
        _Proto.protoTypes.register(NsoDeviceData.class);
        testObject(deviceData.build());

    }

    public static final void testObject(Object o) {
        Encoder encoder = new Encoder();
        encoder.Add(o);
        encoder.reset();
        Object val = encoder.Get();
        if (val.getClass().isArray()) {
            int[] arr = (int[]) val;
            System.out.println(arr.length);
            System.out.println(arr[0]);
            System.out.println(arr[1]);
            System.out.println(arr[2]);
        } else {
            System.out.println(val);
        }
    }
}
