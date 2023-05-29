package com.cisco.corefpcommon.templateutils;

public enum TemplateMapperData {
    INSTANCE;

    private Tuple templateData = new Tuple();

    TemplateMapperData() {
    }

    public Tuple getTemplateTuple() {
        return templateData;
    }
}
