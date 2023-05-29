package com.cisco.crossworks.encoder;

import java.lang.reflect.Array;

public class _Slice implements Encoder.TypeEncoder {
    @Override
    public byte[] add(Object object, Encoder.Location location) {
        if (object == null) {
            return Encoder.int32Encoder.add(-1, location);
        }

        int arrSize = Array.getLength(object);

        if (arrSize == 0) {
            return Encoder.int32Encoder.add(-1, location);
        }

        byte[] data = Encoder.int32Encoder.add(arrSize, location);
        for (int i = 0; i < arrSize; i++) {
            Encoder enc = new Encoder();
            Object o = Array.get(object, i);
            enc.Add(o);
            data = Encoder.addBytes(data, enc.getData());
            location.location += enc.getLocation().location;
        }
        return data;
    }

    @Override
    public Object get(byte[] data, Encoder.Location location) {
        int sizeArr = (int) Encoder.int32Encoder.get(data, location);
        if (sizeArr == -1) {
            return null;
        }
        Object arr = null;
        for (int i = 0; i < sizeArr; i++) {
            Encoder enc = new Encoder(data, location.location);
            Object obj = enc.Get();
            if (arr == null) {
                arr = Array.newInstance(Encoder.getClass(obj), sizeArr);
            }
            Array.set(arr, i, obj);
            location.location = enc.getLocation().location;
        }
        return arr;
    }

    @Override
    public int kind() {
        return 23;
    }
}
