package com.cisco.corefpcommon.templateutils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.cisco.corefpcommon.namespaces.coreFpCommonTemplateMapper;
import com.tailf.conf.ConfBool;
import com.tailf.conf.ConfException;
import com.tailf.conf.DiffIterateOperFlag;
import com.tailf.maapi.Maapi;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuLeaf;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ns.Ncs;
import com.tailf.ncs.template.Template;

public class TemplateFinderFunction {
    private static final Logger LOGGER = LogManager.getLogger(TemplateFinderFunction.class);
    private static TemplateFinderFunction INSTANCE;

    public static TemplateFinderFunction init() {
        if (INSTANCE == null) {
            INSTANCE = new TemplateFinderFunction();
        }
        return INSTANCE;
    }

    private TemplateFinderFunction() {}

    public String findMappingTemplate(Maapi maapi, String provider, String tenant,
        String originalTemplate) {
        Tuple templateDataTuple = TemplateMapperData.INSTANCE.getTemplateTuple();
        String mappingTemp = originalTemplate;

        if ((templateDataTuple.hasChildTuple(provider) && templateDataTuple.getChildTuple(provider)
            .hasChildTuple(tenant) && templateDataTuple.getChildTuple(provider).getChildTuple(
               tenant).isTurnedOff())){
                LOGGER.debug("Override setting is turned off, returning " + originalTemplate);
                return mappingTemp;
        }

        if ((templateDataTuple.hasChildTuple(provider) && templateDataTuple.getChildTuple(provider)
            .isTurnedOff()) || templateDataTuple.isTurnedOff()){
                LOGGER.debug("Override setting is turned off, returning " + originalTemplate);
                return mappingTemp;
        }

        if (templateDataTuple.hasTemplateMapping(originalTemplate)) {
            mappingTemp = templateDataTuple.getTemplateMapping(originalTemplate);
        }
        if (templateDataTuple.hasChildTuple(provider)) {
            if (templateDataTuple.getChildTuple(provider).hasTemplateMapping(originalTemplate)) {
                mappingTemp = templateDataTuple.getChildTuple(provider).getTemplateMapping(
                    originalTemplate);
            }
            if (templateDataTuple.getChildTuple(provider).hasChildTuple(tenant) && templateDataTuple
                .getChildTuple(provider).getChildTuple(tenant).hasTemplateMapping(
                    originalTemplate)) {
                mappingTemp = templateDataTuple.getChildTuple(provider).getChildTuple(tenant)
                    .getTemplateMapping(originalTemplate);
            }
        }

        mappingTemp = mappingTemp == null ? originalTemplate : mappingTemp;

        try {
            if (!Template.exists(maapi, mappingTemp)) {
                LOGGER.warn("Mapping template " + mappingTemp + " for " + originalTemplate
                    + " does not exist");
                mappingTemp = originalTemplate;
            }
        } catch (ConfException e) {
            mappingTemp = originalTemplate;
        }
        LOGGER.debug((mappingTemp.equals(originalTemplate)
            ? "No mapping is found for original template " + originalTemplate
            : "Template mapping for " + originalTemplate + " is " + mappingTemp));
        return mappingTemp;
    }

    public void loadTemplateMappingData(Maapi maapi, int tid) throws NavuException {
        NavuContainer ncsRoot = new NavuContainer(maapi, tid, Ncs.hash);
        NavuNode mapping = ncsRoot.getParent().container(coreFpCommonTemplateMapper.hash).container(
            coreFpCommonTemplateMapper._override_template_);
        Tuple templateDataTuple = TemplateMapperData.INSTANCE.getTemplateTuple();
        templateDataTuple.setTurnOffFlag(((ConfBool) mapping.leaf(
            coreFpCommonTemplateMapper._ignore_override_settings_).value()).booleanValue());
        readMapping(mapping, templateDataTuple);
        for (NavuNode provider : mapping.list(coreFpCommonTemplateMapper._per_provider_)) {
            String name = provider.leaf(coreFpCommonTemplateMapper._provider_).valueAsString();
            templateDataTuple.setChildTuple(name, readProviderData(name, provider));
        }
        LOGGER.debug(templateDataTuple);
    }

    public void readTemplateMappingChange(TemplateMapperSub.Type type, String key,
        DiffIterateOperFlag opr, NavuNode node) throws NavuException {
        Tuple templateDataTuple = TemplateMapperData.INSTANCE.getTemplateTuple();

        switch (type) {
            case TURN_OFF_FLAG:
                templateDataTuple.setTurnOffFlag(((ConfBool) ((NavuLeaf) node).value())
                    .booleanValue());
                break;
            case GLOBAL:
                switch (opr) {
                    case MOP_CREATED:
                        templateDataTuple.setGlobalTemplateMapping(key, node.leaf(
                            coreFpCommonTemplateMapper._override_template_).valueAsString());
                        break;

                    case MOP_DELETED:
                        templateDataTuple.removeGlobalTemplateMapping(key);
                        break;

                    default:
                        break;
                }
                break;
            case PROVIDER:
                switch (opr) {
                    case MOP_CREATED:
                        templateDataTuple.setChildTuple(key, readProviderData(key, node));
                        break;

                    case MOP_DELETED:
                        templateDataTuple.removeChildTuple(key);
                        break;

                    default:
                        break;
                }
                break;
            default:
                break;
        }
        LOGGER.debug(templateDataTuple);
    }

    private Tuple readProviderData(String key, NavuNode provider) throws NavuException {
        Tuple providerTuple = new Tuple();
        providerTuple.setName(key);
        providerTuple.setTurnOffFlag(((ConfBool) provider.leaf(
            coreFpCommonTemplateMapper._ignore_override_settings_).value()).booleanValue());
        readMapping(provider, providerTuple);
        for (NavuNode tenant : provider.list(coreFpCommonTemplateMapper._per_tenant_)) {
            Tuple tenantTuple = new Tuple();
            tenantTuple.setName(tenant.leaf(coreFpCommonTemplateMapper._tenant_).valueAsString());
            tenantTuple.setTurnOffFlag(((ConfBool) tenant.leaf(
                coreFpCommonTemplateMapper._ignore_override_settings_).value()).booleanValue());
            readMapping(tenant, tenantTuple);
            providerTuple.setChildTuple(tenantTuple.getName(), tenantTuple);
        }
        return providerTuple;
    }

    private void readMapping(NavuNode node, Tuple dataTuple) throws NavuException {
        for (NavuNode mapping : node.list(coreFpCommonTemplateMapper._service_template_mapper_)) {
            dataTuple.setGlobalTemplateMapping(mapping.leaf(
                coreFpCommonTemplateMapper._service_template_).valueAsString(), mapping.leaf(
                    coreFpCommonTemplateMapper._override_template_).valueAsString());
        }
    }
}
