/*
 ***************************************************
 * Copyright (c) 2019 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.corefpcommon.statuscodes;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import com.cisco.corefpcommon.namespaces.coreFpCommonStatusCodes;
import com.tailf.conf.ConfException;
import com.tailf.dp.DpCallbackException;
import com.tailf.maapi.Maapi;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.ns.Ncs;

/**
 * Utility class for Status Codes
 *
 * @author abhatta
 */
public class StatusCodeUtil {
    private static final Logger LOGGER = LogManager.getLogger(StatusCodeUtil.class);

    /**
     * Validate status codes for all core function packs.
     *
     * @param maapi
     * @param th
     * @throws DpCallbackException
     */
    public static void validateStatusCodes(Maapi maapi, int th) throws DpCallbackException {
        try {
            NavuNode ncsRoot = new NavuContainer(maapi, th, Ncs.hash);
            NavuList cfpNodes = ncsRoot.getParent().container(coreFpCommonStatusCodes.uri)
                .container(coreFpCommonStatusCodes._status_codes_).list(
                    coreFpCommonStatusCodes._core_function_pack_);
            for (NavuNode cfpNode : cfpNodes) {
                if (isJavaEnum(cfpNode)) {
                    validateCfpStatusCodes(cfpNode);
                }
            }
        } catch (DpCallbackException e) {
            throw e;
        } catch (ConfException e) {
            LOGGER.error("Unexpected error: ", e);
        }
    }

    /**
     * Validate status codes of a specific core function pack
     *
     * @param cfpNode: core-function-pack NavuNode
     * @throws DpCallbackException
     */
    public static void validateCfpStatusCodes(NavuNode cfpNode) throws DpCallbackException {
        try {
            StringBuilder err = new StringBuilder();
            List<String> cfpStatusCodes = readCfpStatusCodes(cfpNode);
            List<String> statusCodes = cfpNode.list(coreFpCommonStatusCodes._status_code_).keySet()
                .stream().map(item -> item.toString().replaceAll("[{}]", "")).collect(Collectors
                    .toList());
            if (!statusCodes.isEmpty()) {
                cfpStatusCodes.removeIf(item -> statusCodes.contains(item));
            }
            if (!cfpStatusCodes.isEmpty()) {
                err.append("Following status codes of core function pack: " + cfpNode.leaf(
                    coreFpCommonStatusCodes._name_).valueAsString() + ", are not set: ");
                cfpStatusCodes.forEach(item -> err.append(item).append(", "));
            }
            if (err.length() > 0) {
                err.delete(err.toString().lastIndexOf(","), err.length());
                throw new DpCallbackException(err.toString());
            }
        } catch (DpCallbackException e) {
            throw e;
        } catch (NavuException e) {
            LOGGER.error("", e);
            throw new DpCallbackException(e.getMessage());
        }
    }

    /**
     * Generate status codes xml payload from core function pack enum class at compile time. Core
     * function pack's responsibility is to load merge the generated xml payload to NCS as a
     * bootstrap data.
     *
     * @param enumClass
     * @param cfpName
     */
    public static void generatePayload(Class<?> enumClass, String cfpName) {
        try {
            DocumentBuilder docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();

            // root element
            Document doc = docBuilder.newDocument();
            Node disableEscaping = doc.createProcessingInstruction(
                StreamResult.PI_DISABLE_OUTPUT_ESCAPING, "");
            doc.appendChild(disableEscaping);
            Element rootElement = doc.createElementNS("http://tail-f.com/ns/config/1.0",
                Ncs._config_);
            doc.appendChild(rootElement);

            // status-code-cfp elements
            Element statusCodeCfp = createXmlElement(doc, rootElement, coreFpCommonStatusCodes.uri,
                coreFpCommonStatusCodes._status_code_cfp_);

            setXmlElementValue(doc, statusCodeCfp, coreFpCommonStatusCodes._name_, cfpName);

            // status-codes elements
            Element statusCodes = createXmlElement(doc, rootElement, coreFpCommonStatusCodes.uri,
                coreFpCommonStatusCodes._status_codes_);

            // core-function-pack elements
            Element cfp = createXmlElement(doc, statusCodes, null,
                coreFpCommonStatusCodes._core_function_pack_);

            setXmlElementValue(doc, cfp, coreFpCommonStatusCodes._name_, cfpName);
            setXmlElementValue(doc, cfp, coreFpCommonStatusCodes._status_code_enum_path_, enumClass
                .getCanonicalName());

            List<Field> fields = Arrays.stream(enumClass.getDeclaredFields()).filter(item -> !item
                .isEnumConstant() && !Modifier.isStatic(item.getModifiers())).collect(Collectors
                    .toList());

            for (Object obj : enumClass.getEnumConstants()) {
                // status-code elements
                Element statusCode = createXmlElement(doc, cfp, null,
                    coreFpCommonStatusCodes._status_code_);

                for (int i = 0; i < fields.size(); i++) {
                    Field field = fields.get(i);
                    field.setAccessible(true);
                    String value = field.get(obj).toString();
                    if (i == 0) {
                        setXmlElementValue(doc, statusCode, coreFpCommonStatusCodes._code_, value);
                    } else if (i == 1) {
                        setXmlElementValue(doc, statusCode, coreFpCommonStatusCodes._reason_,
                            value);
                    } else if (i == 2) {
                        setXmlElementValue(doc, statusCode, coreFpCommonStatusCodes._category_,
                            value);
                    } else if (i == 3) {
                        setXmlElementValue(doc, statusCode, coreFpCommonStatusCodes._severity_,
                            value);
                    } else {
                        setXmlElementValue(doc, statusCode,
                            coreFpCommonStatusCodes._recommended_actions_, value);
                    }
                }
            }
            Node enableEscaping = doc.createProcessingInstruction(
                StreamResult.PI_ENABLE_OUTPUT_ESCAPING, "");
            doc.appendChild(enableEscaping);

            // write the content into xml file
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperties(getTransformerProperties());
            DOMSource source = new DOMSource(doc);
            StreamResult result = new StreamResult(new File(cfpName + "-status-codes.xml"));
            transformer.transform(source, result);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static boolean isJavaEnum(NavuNode cfpNode) throws NavuException {
        return cfpNode.leaf(coreFpCommonStatusCodes._status_code_enum_path_).valueAsString()
            .contains(".");
    }

    private static List<String> readCfpStatusCodes(NavuNode cfpNode) throws DpCallbackException {
        List<String> cfpStatusCodes = new ArrayList<>();
        try {
            String enumPath = cfpNode.leaf(coreFpCommonStatusCodes._status_code_enum_path_)
                .valueAsString();
            Class<?> statusCodeClass = Class.forName(enumPath);
            LOGGER.debug("Status Enum Class: " + statusCodeClass);
            if (statusCodeClass.isEnum()) {
                Field field = Arrays.stream(statusCodeClass.getDeclaredFields()).filter(
                    item -> !item.isEnumConstant() && !Modifier.isStatic(item.getModifiers()))
                    .findFirst().get();
                field.setAccessible(true);
                for (Object obj : statusCodeClass.getEnumConstants()) {
                    cfpStatusCodes.add(field.get(obj).toString());
                }
            } else {
                throw new DpCallbackException("Provided "
                    + coreFpCommonStatusCodes._status_code_enum_path_ + " is not an Enum class.");
            }
        } catch (ClassNotFoundException e) {
            throw new DpCallbackException(e.getMessage() + " class not found.");
        } catch (NavuException | IllegalArgumentException | IllegalAccessException e) {
            throw new DpCallbackException(e.getMessage());
        }
        return cfpStatusCodes;
    }

    private static Element createXmlElement(Document doc, Element rootElement, String namespace,
        String elementName) {
        Element newElem = null;
        if (null != namespace) {
            newElem = doc.createElementNS(namespace, elementName);
        } else {
            newElem = doc.createElement(elementName);
        }
        rootElement.appendChild(newElem);
        return newElem;
    }

    private static void setXmlElementValue(Document doc, Element rootElement, String elementName,
        String value) {
        if (null != value && !value.isEmpty()) {
            Element newElem = doc.createElement(elementName);
            newElem.appendChild(doc.createTextNode(value));
            rootElement.appendChild(newElem);
        }
    }

    private static Properties getTransformerProperties() {
        Properties props = new Properties();
        props.setProperty(OutputKeys.METHOD, "xml");
        props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        props.setProperty(OutputKeys.INDENT, "yes");
        props.setProperty("{http://xml.apache.org/xslt}indent-amount", "2");
        return props;
    }
}
