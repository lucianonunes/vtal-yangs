/*
 ***************************************************
 * Copyright (c) 2020 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.nso.common.ctutils;

import java.util.Map;
import com.tailf.conf.ConfException;
import com.tailf.dp.services.ServiceContext;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;

/**
 *
 * @author abhatta
 *
 *         The interface which provides the api methods, to integrate with core function packs.
 *
 */
public interface CustomTemplateUtils {
  /**
   * Apply multiple custom templates to a device with extra variable map.
   *
   * @param context {@link ServiceContext} Mandatory parameter must be passed from where this method
   *        is called.
   * @param deviceName {@link String} Mandatory parameter name of the device to which custom
   *        templates will be applied.
   * @param keyPath {@link String} Mandatory parameter keyPath till {@link NavuList} custom-template
   *        in yang model.
   * @param extraVariables {@link Map} of variable name value pair, which will be used for all
   *        iterations. Pass {@link null}, if there is no extra variables to be used while applying
   *        templates.User provided template variables will overwrite the variables present in this
   *        map if variable name matches.
   * @return false if apply-custom-template flag is false, else true.
   * @throws ConfException
   */
  static boolean applyCustomTemplates(ServiceContext context, String deviceName, String keyPath,
      Map<String, String> extraVariables) throws ConfException {
    return CTUtilsFunction.applyCustomTemplates(context, deviceName, keyPath, extraVariables);
  }

  /**
   * Apply a single custom template to a device with extra variable map.
   *
   * @param context {@link ServiceContext} Mandatory parameter must be passed from where this method
   *        is called.
   * @param deviceName {@link String} Mandatory parameter name of the device to which custom
   *        template will be applied.
   * @param keyPath {@link String} Mandatory parameter keyPath till {@link NavuNode}
   *        custom-template{%s} in yang model.
   * @param extraVariables {@link Map} of variable name value pair, which will be used for all
   *        iterations. Pass {@link null}, if there is no extra variables to be used while applying
   *        templates.User provided template variables will overwrite the variables present in this
   *        map if variable name matches.
   * @return false if apply-custom-template flag is false, else true.
   * @throws ConfException
   */
  static boolean applyCustomTemplate(ServiceContext context, String deviceName, String keyPath,
      Map<String, String> extraVariables) throws ConfException {
    return CTUtilsFunction.applyCustomTemplate(context, deviceName, keyPath, extraVariables);
  }

  /**
   * Apply any template to a device with provided variable map only one time.
   *
   * @param context {@link ServiceContext} Mandatory parameter must be passed from where this method
   *        is called.
   * @param deviceName {@link String} Mandatory parameter name of the device to which custom
   *        templates are applied.
   * @param templateName {@link String} Mandatory parameter name of the template to be applied.
   * @param variables {@link Map} of variable name value pair to be used while applying the
   *        template.
   * @return false if apply-custom-template flag is false, else true.
   * @throws ConfException
   */
  static boolean applyTemplate(ServiceContext context, String deviceName, String templateName,
      Map<String, String> variables) throws ConfException {
    return CTUtilsFunction.applyTemplate(context, deviceName, templateName, variables);
  }
}
