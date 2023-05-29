package com.cisco.corefpcommon.templateutils;

import java.util.HashMap;

public class Tuple {
    private String name;
    private boolean turnOffFlag;
    private HashMap<String, String> templateMapping = new HashMap<String, String>();
    private HashMap<String, Tuple> children = new HashMap<String, Tuple>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setTurnOffFlag(boolean flag) {
        turnOffFlag = flag;
    }

    public boolean isTurnedOff() {
        return turnOffFlag;
    }

    public boolean hasTemplateMapping(String key) {
        return templateMapping.containsKey(key);
    }

    public String getTemplateMapping(String key) {
        return templateMapping.get(key);
    }

    public void setGlobalTemplateMapping(String serviceTemplate, String overrideTemplate) {
        this.templateMapping.put(serviceTemplate, overrideTemplate);
    }

    public void removeGlobalTemplateMapping(String serviceTemplate) {
        this.templateMapping.remove(serviceTemplate);
    }

    public boolean hasChildTuple(String key) {
        return this.children.containsKey(key);
    }

    public Tuple getChildTuple(String key) {
        return this.children.get(key);
    }

    public void setChildTuple(String key, Tuple tuple) {
        this.children.put(key, tuple);
    }

    public void removeChildTuple(String key) {
        this.children.remove(key);
    }

    @Override
    public String toString() {
        return "Tuple [name=" + name + ", turnOffFlag=" + turnOffFlag + ", templateMapping="
                + templateMapping + ", children=" + children + "]";
    }
}
