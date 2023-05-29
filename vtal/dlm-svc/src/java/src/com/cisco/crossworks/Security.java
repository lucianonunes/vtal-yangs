package com.cisco.crossworks;

import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

public class Security {
    //Encrypt a byte array using the given key.
    protected static String encrypt(byte[] dataToEncode, String key) throws NoSuchPaddingException, NoSuchAlgorithmException, InvalidAlgorithmParameterException, InvalidKeyException, IOException {
        byte[] iv = new byte[16];
        IvParameterSpec ivspec = new IvParameterSpec(iv);
        SecretKey secretKey = new SecretKeySpec(key.getBytes(), 0, key.length(), "AES");

        Cipher cr = Cipher.getInstance("AES/CFB/NoPadding");
        cr.init(Cipher.ENCRYPT_MODE, secretKey, ivspec);
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        CipherOutputStream out = new CipherOutputStream(bout, cr);
        out.write(iv);
        out.write(dataToEncode);
        out.close();
        byte[] data = bout.toByteArray();
        return Base64.getEncoder().encodeToString(data);
    }

    public static byte[] decrypt(String stringToDecode, String key) {
        try {
            byte[] dataWithIv = Base64.getDecoder().decode(stringToDecode);
            byte[] iv = new byte[16];
            byte[] dataToDecrypt = new byte[dataWithIv.length - iv.length];
            System.arraycopy(dataWithIv, 0, iv, 0, 16);
            System.arraycopy(dataWithIv, 16, dataToDecrypt, 0, dataWithIv.length - iv.length);

            IvParameterSpec ivspec = new IvParameterSpec(iv);
            SecretKey secretKey = new SecretKeySpec(key.getBytes(), 0, key.length(), "AES");

            Cipher cipher = Cipher.getInstance("AES/CFB/NoPadding");
            cipher.init(Cipher.DECRYPT_MODE, secretKey, ivspec);
            return cipher.doFinal(dataToDecrypt);
        } catch (Exception e) {
            System.out.println("Error while decrypting: " + e.toString());
        }
        return null;
    }
}
