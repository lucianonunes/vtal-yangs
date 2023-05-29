package com.cisco.corefpcommon.planutils;

public class CoreFpNameUtil {
    
    private CoreFpNameUtil() {
    }

    public static String getImageName(String vnfdName, String vduName) {
        String image = "image_" + vnfdName + "_" + vduName;
        return image;
    }

    public static String getFlavorName(String vnfdName, String vduName) {
        String flavorName = "flavor_" + vnfdName + "_" + vduName;
        return flavorName;
    }

    public static String getDeployment(String dcName, String vnfName) {
        return dcName + "_" + vnfName;
    }
}
