# custom-template-utils

## Overview

The package ***custom-template-utils*** inside this project adds additional functionality on top of the already awesome NSO Service Templates feature (see "Mapping using Service Templates" in the NSO Developers guide).

**See the custom-template-project with custom-template-utils package and ct-utils-test package [here](https://wwwin-github.cisco.com/nso-contributions/custom-template-project "custom-template-project")**

## Features

### Cutom templates naming restriction
> **Custom template either it is a service template or device template, must starts with "ct-" or "CT-".**

### Apply multiple custom templates to a device
The package ***custom-template-utils*** can be integrated with other core function packs to enable custom template feature in core function packs. This packages handles and applies the templates to the device, irresepective of whether the templates reside inside templates directory of any nso package or as device templates loaded into nso. The sequence of template apply is in the order of template defined in custom template model.

### Apply custom template multiple times
A template can have multiple **iteration** to indicate that it is going to be applied multiple times. Every iteration will have same variables with different values, which are taken into consideration while applying the template for that iteration.

### Template variable naming restrictions
> The device/controller name variable in every service config templates must be either ***DEVICE\_NAME / DEVICE / CONTROLLER\_NAME***. These three variable names are reserved only for device/controller name in templates and hence must not be used for anything else in a template.

* The variable name should be such that it should be self understandable.

	**For example**, if a template is applied to configure Ethernet interfaces for ASA device. Suppose **Ethernet 0/0** is configured as **Private** interface along with it's IP address and **Ethernet 0/1** is configured as **Public** interface along with it's IP address. In the template if ip addresses are variables, then the variable name should be ***VFW\_PRIVATE\_INTF\_IP*** and ***VFW\_PUBLIC\_INTF\_IP*** instead of ***VFW\_ETH0\_IP*** and ***VFW\_ETH1\_IP***. The former variable names are more understandable than the later ones because the former variable names suggest that these variables contain Public and Private interfaces IP addresses of ASA (virtual firewall) and variable values should be of IP address type. But the later variables does not convey which variable is for which interface. If a field engineer does not know that Ethernet 0/0 should be configured as Private and Ethernet 0/1 as Public interface, then by looking at the variable names like VFW_ETH0_IP and VFW_ETH1_IP, he may get confused on assigning proper interface ip values, which results into ASA being wrongly configured.
	
	* 	Variable name should be suffix with type of the variable like **IP** address type variable should siffix with ***\_IP***. Similarly **String** type variable should suffix with ***\_STR***.
	
		**Example** of different variable suffix can be **\_IP**, **\_IPV4**, **\_IPV6**, **\_STR**, **\_INT** etc..

### Turn ON/OFF custom template feature
***apply-custom-template*** boolean flag is provided to turn on/off custom template feature in yang model. Default value is true means custom template feature is enabled by default. To disable custom template feature, set the flag to false.

```
admin@ncs% set apply-custom-template false
[ok][2016-09-22 11:10:46]

[edit]
admin@ncs% commit
Commit complete.
[ok]
```
> Disabling custom template feature does not rollback the configs pushed by the custom templates before disabling the feature. To rollback configs pushed by custom templates, ***re-deploy*** the parent core function pack.

### Show template and variable suggestion in cli
***custom-template-utils*** provide custom-template names as well as variables of a custom template as cli suggestions.
> It does not shows the reserved variable names in a cutom-template like **CONTROLLER\_NAME** / **DEVICE\_NAME** / **DEVICE**.

#### Example
```
admin@ncs% set ct-utils-test srv1 device cpe0 custom-template ?
Possible completions:
  CT-asa_exec          CT-asa_interface     CT-asa_login         CT-asa_motd               CT-exec                   CT-login
  CT-motd              ct-hosttemplate_asa  ct-hosttemplate_cpe  ct-interfacetemplate_asa  ct-interfacetemplate_cpe  ct-usertemplate_asa
  ct-usertemplate_cpe
```
```
admin@ncs% set ct-utils-test srv1 device cpe0 custom-template CT-asa_interface variable ?
Possible completions:
  ASA_INTF_DESCRIPTION  ASA_INTF_ID  ASA_INTF_IP  ASA_INTF_NAME
```
```
admin@ncs% set ct-utils-test srv1 device cpe0 custom-template CT-asa_interface iteration 1 variable ?
Possible completions:
  ASA_INTF_DESCRIPTION  ASA_INTF_ID  ASA_INTF_IP  ASA_INTF_NAME
```

### Check list of applied custom templates
> **This feature is only available if integrated though custom-template-utils stack service.**

 ***This feature is to check which templates are applied on which device successfully.***

> **To check detailed custom-template apply status, please check customization noitification.**

#### Example
```
admin@ncs> show applied-custom-templates
NAME              DEVICE  OWNER
-------------------------------------------------------------------------
CT-asa_interface  asa1    /ct-utils-test:ct-utils-test{srv2}
CT-asa_login      asa1    /ct-utils-test:ct-utils-test{srv1}
ct-vlan           cpe1    srv1
ct-vlan           cpe0    /ct-utils-test:ct-utils-test{srv1}/vlan{cpe0}
ct-vlan           cpe0    /ct-utils-test:ct-utils-test{srv2}/vlan{cpe0}
CT-asa_interface  asa0    service-1
CT-asa_interface  asa1    service-1
[ok]
```
In the above example,

* **NAME:** Custom-template name.
* **DEVICE:** Device on which custom-template is applied.
* **OWNER:** Owner of the custom-template stack service. It can be any string, even the service context path to uniquely identify which service applied the custom-template, **if core function pack facilitates to apply same custom-template on same device at different parts of it's payload**.

### Send customization notifications
> **This feature is only available if integrated though custom-template-utils stack service.**

***custom-template-utils*** sends notification for each template **APPLIED/UPDATED/DELETED/FAILED/NO_OPERATION** events.

#### Status
* **APPLIED:** When a custom template is applied to device.
* **UPDATED:** When a custom template is updated. It can be a actual update to the template or just a service re-deploy.
* **DELETED:** When a custom template is deleted. Custom template can also be deleted when service is re-deployed after setting **apply-custom-template** flag to **false**.
* **FAILED:** When a custom template is failed to apply to a device, may be due to missing variable value or incompatible value or wrong template etc..
* **NO_OPERATION:** There will be no operation on template, if there is any *CREATE/UPDATE/DELETE* operation on template when **apply-custom-template** flag is **false**. This notification will be sent for all each custom template separately.

#### Example
```
admin@ncs> show notification stream customization-notif
notification {
    eventTime 2018-06-26T21:08:26.975+00:00
    customization-notif {
        name ct-hosttemplate_asa
        device asa0
        owner srv1
        status DELETED
        message apply-custom-template flag is false
    }
}
notification {
    eventTime 2018-06-26T21:10:23.271+00:00
    customization-notif {
        name ct-interfacetemplate_cpe
        device cpe0
        owner srv1
        status APPLIED
        message APPLY SUCCESS
    }
}
notification {
    eventTime 2018-06-26T21:21:14.579+00:00
    customization-notif {
        name ct-usertemplate_asa
        device cpe1
        owner srv1
        status FAILED
        message Error applying device template: ct-usertemplate_asa
    }
}
notification {
    eventTime 2018-06-26T21:23:28.823+00:00
    customization-notif {
        name CT-asa_login
        device asa0
        owner srv1
        status APPLIED
        message UPDATE SUCCESS
    }
}
notification {
    eventTime 2018-06-26T21:23:29.718+00:00
    customization-notif {
        name CT-login
        device cpe0
        owner srv1
        status DELETED
        message DELETE SUCCESS
    }
}
notification {
    eventTime 2020-02-06T21:57:47.568+00:00
    customization-notif {
        name ct-vlan
        device cpe1
        owner "/ct-utils-test:ct-utils-test{srv1}"
        status DELETED
        message apply-custom-template flag is false
    }
}
notification {
    eventTime 2018-06-29T19:50:17.848+00:00
    customization-notif {
        name CT-asa_interface
        device asa0
        owner service-1
        status NO_OPERATION
        message apply-custom-template flag is false
    }
}
```

## Integration with core function packs
There are 3 ways we can integrate with custom-template-utils, i.e., through

* Java APIs
* Python APIs
* cusrom-template-utils stack service.

But there are some common integration steps which need to be carried out before carrying out steps for specific way.
### Common Steps
#### Step-1: package-meta-data
Below section must be added to ***package-meta-data*** file of core function pack package, to which custom-template-utils is integrated.

```
<required-package>
  <name>custom-template-utils</name>
</required-package>
```
#### Step-2: Makefile
Please look for below section and update as mentioned below to ***Makefile*** of core function pack package, to which custom-template-utils is integrated:

```
## Uncomment and patch the line below if you have a dependency to a NED
## or to other YANG files
YANGPATH += ../../custom-template-utils/src/yang
```
Please refer [Makefile](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/src/Makefile "Makefile").
#### Step-3: Yang model
***custom-template-utils*** package introduces model that exposes custom template functionality.

###### model: custom-template-hook.yang
```
module: custom-template-hook
  +--rw apply-custom-template?   boolean
  grouping template-hook
    +---- custom-template* [name]
       +---- name?        string
       +---- variable* [name]
       |  +---- name?    string
       |  +---- value    string
       +---- iteration* [number]
          +---- number?     uint16
          +---- variable* [name]
             +---- name?    string
             +---- value    string
```
***grouping template-hook*** can be used in function pack models to support for custom templates from north bound. Look for `uses ct-hook:template-hook;` in [ct-utils-test.yang](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/src/yang/ct-utils-test.yang "ct-utils-test.yang").

### Java APIs
> **Java APIs do not send any customization notification or maintain custom-template-status. The core function pack which uses Java APIs should handle these.**

All below steps are specific to integration through JAVA APIs.
#### Step-1: build.xml
Please look for below section and update as mentioned below to ***build.xml*** of core function pack package, to which custom-template-utils is integrated:

```
<project name="package" basedir="." default="all">
  .
  .
  <property name="ctu.dir" value="../../../custom-template-utils/shared-jar"/>
  .
  .
  <path id="ctu-libs">
    <fileset dir="${ctu.dir}">
      <include name="*.jar"/>
    </fileset>
  </path>
  .
  .
  <target name="compile">
    .
    .
    <javac>
      .
      .
      <classpath refid="ctu-libs"/>
    </javac>
  </target>
```
Please refer [build.xml](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/src/java/build.xml "").
#### Step-2: Code
***custom-template-utils*** package exposes an utility interface [CustomTemplateUtils.java](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/custom-template-utils/src/java/src/com/cisco/nso/common/ctutils/CustomTemplateUtils.java "CustomTemplateUtils.java") with static methods.
Refer [ctutilstestRFS.java](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/src/java/src/com/example/ctutilstest/ctutilstestRFS.java "ctutilstestRFS.java") to know about usage of java api methods.

##### METHOD-1: applyCustomTemplates
Apply all custom templates to a device defined by list custom-template yang model, which is identified by it's keyPath.
```
boolean applyCustomTemplates(ServiceContext context, String deviceName, String keyPath,
      Map<String, String> extraVariables) throws ConfException
```
###### Parameters
* **context:** The `ServiceContext` object.
* **deviceName:** The name of the device to which custom templates will be applied.
* **keyPath:** The keypath of `NavuList` ***custom-template***, which can be get by `NavuList.getKeyPath()` NSO Java API. This represents list of custom-template in yang model.
* **extraVariables:** The `Map<String, String>` of variable name and value pairs. As the name suggests, these are extra variables if core function pack want to pass, otherwise pass `null`. These variables will be used while applying the template one time or multiple times. User provided template variables will overwrite the variables present in this map if variable name matches.

###### Returns
`false` if `apply-custom-template` flag is `false`, or else returns `true`.

###### Throws
`com.tailf.conf.ConfException`

##### METHOD-2: applyCustomTemplate
This method is similar to the method mentioned above but it is used to apply a single custom template to a device defined by a node of list custom-template yang model, which is identified by it's keyPath.

```
boolean applyCustomTemplate(ServiceContext context, String deviceName, String keyPath,
      Map<String, String> extraVariables) throws ConfException
```
###### Parameters
* **context:** The `ServiceContext` object.
* **deviceName:** The name of the device to which custom templates will be applied.
* **keyPath:** The keypath of `NavuNode` ***custom-template{%s}***, which can be get by `NavuNode.getKeyPath()` NSO Java API. This represents a single custom-template node in yang model.
* **extraVariables:** The `Map<String, String>` of variable name and value pairs. As the name suggests, these are extra variables if core function pack want to pass, otherwise pass `null`. These variables will be used while applying the template one time or multiple times. User provided template variables will overwrite the variables present in this map if variable name matches.

###### Returns
`false` if `apply-custom-template` flag is `false`, or else returns `true`.

###### Throws
`com.tailf.conf.ConfException`

##### METHOD-3: applyTemplate
Apply any template to a device using provided variable map only for **one time**.

```
boolean applyTemplate(ServiceContext context, String deviceName, String templateName,
      Map<String, String> variables) throws ConfException
```
###### Parameters
* **context:** The `ServiceContext` object.
* **deviceName:** The name of the device to which custom template will be applied.
* **templateName:** The name of the custom template.
* **variables:** `Map<String, String>` object to hold the variable name and value pair which will be used to apply the custom template for **one time**.

###### Returns
`false` if `apply-custom-template` flag is `false`, or else returns `true`.

###### Throws
`com.tailf.conf.ConfException`

### Python APIs
> **Python APIs do not send any customization notification or maintain custom-template-status. The core function pack which uses Python APIs should handle these.**

All below steps are specific to integration through Python APIs.

#### Step-1: Makefile
The python api files need to be copied to the core function package python package directory, during compilation. To achieve this, following steps need to be carried out.

* To create and delete a sub-package **custom-template-utils** under core function pack python package directory on `make clean all`, add `../python/<cfp-package-name>/custom_template_utils` to **DIRS** target in Makefile.
* Add a new target **ct-copy** in Makefile to copy python files from `custom-template-utils/python/custom_template_utils` to current package's `python/<cfp-package-name>/custom_template_utils`.
* Append the **ct-copy** target to **all** target in Makefile.

Please refer [Makefile](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/src/Makefile "Makefile").

#### Step-2: Code
***custom-template-utils*** package exposes an python module [custom_template_utils.py](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/custom-template-utils/python/custom_template_utils/custom_template_utils.py "custom_template_utils.py") with functions.
Refer [main.py](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/python/ct_utils_test/main.py "main.py") to know about usage of python api functions.

##### FUNCTION-1: applyCustomTemplates
Apply all custom templates to a device defined by list custom-template yang model, which is identified by it's nso maagic List node.
```
apply_custom_templates(self, root, service, device_name, ct_list_node, extra_vars=None)
```
###### Parameters
* **self:** `ServiceCallbacks` instance.
* **root:** root `nso.maagic.Node` node.
* **service:** service `nso.maagic.Node` node and all the custom-templates will be applied on this context.
* **device\_name:** Name of the device of type `String` on which all the custom-templates will be applied.
* **ct\_list\_node:** cutom_template `nso.maagic.List` node.
* **extra\_vars:** Dictionary `{variable_name:variable_value}` of extra variables. These variables will be over-written by the custom-template variables if variable name matches.

###### Returns
`false` if `apply-custom-template` flag is `false`, or else returns `true`.

###### Throws
`Error` if any error happens.

##### METHOD-2: applyCustomTemplate
This method is similar to the method mentioned above but it is used to apply a single custom template to a device defined by a node of list custom-template yang model, which is identified by it's nos maagic node.

```
apply_custom_template(self, root, service, device_name, ct_node, extra_vars=None)
```
###### Parameters
* **self:** `ServiceCallbacks` instance.
* **root:** root `nso.maagic.Node` node.
* **service:** service `nso.maagic.Node` node and all the custom-templates will be applied on this context.
* **device\_name:** Name of the device of type `String` on which all the custom-templates will be applied.
* **ct\_node:** a single cutom_template `nso.maagic.Node` node.
* **extra\_vars:** Dictionary `{variable_name:variable_value}` of extra variables. These variables will be over-written by the custom-template variables if variable name matches.

###### Returns
`false` if `apply-custom-template` flag is `false`, or else returns `true`.

###### Throws
`Error` if any error happens.

##### METHOD-3: applyTemplate
Apply any template to a device using provided variable map only for **one time**.

```
apply_template(self, root, service, device_name, template_name, variables=None)
```
###### Parameters
* **self:** `ServiceCallbacks` instance.
* **root:** root `nso.maagic.Node` node.
* **service:** service `nso.maagic.Node` node and all the custom-templates will be applied on this context.
* **device\_name:** Name of the device of type `String` on which all the custom-templates will be applied.
* **template\_name:** Name of the cutom-template of type `String` which will be applied.
* **variables:** Dictionary `{variable_name:variable_value}` of variables in the custom-template.

###### Returns
`false` if `apply-custom-template` flag is `false`, or else returns `true`.

###### Throws
`Error` if any error happens.

### Stack Service
> **cutom-templates applied through stack service supports customization notification and maintain custom-template-status.**

All below steps are specific to integration through custom-template stack service.
#### Step-1: ncs.conf (If customization notification required)
***customization-notif*** stream must be added to ***ncs.conf*** file under ***notifications/event-streams*** along with other default and core function pack specific notification streams.

```
<notifications>
  <event-streams>
  .
  .
  .
    <stream>
      <name>customization-notif</name>
      <description>customization notifications</description>
      <replay-support>true</replay-support>
      <builtin-replay-store>
        <enabled>true</enabled>
        <dir>./state</dir>
        <max-size>S10M</max-size>
        <max-files>50</max-files>
      </builtin-replay-store>
    </stream>
  </event-streams>
</notifications>
```

#### Step-2: Code
***custom-template-utils*** package exposes **three** templates, which can be applied to create custom-template stack service to apply custom templates, i.e.,

* [custom-template](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/custom-template-utils/templates/custom-template.xml "custom-template.xml") to apply custom-template with it's variable populated in. core-function-pack code, where this template is applied.
* [custom-template-advance](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/custom-template-utils/templates/custom-template-advance.xml "custom-template-advance.xml") to apply custom-template by applying this template in core function pack code. But the variables are directly accessed from core function pack service yang through xpath by NSO template engine.
* [custom-template-extra-vars](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/custom-template-utils/templates/custom-template-extra-vars.xml "custom-template-extra-vars") to pass extra variables generated by core function pack. **Note:** This template must be additionally applied along with any one of the above mentioned template if there are extra variables need to be passed.

Refer [ctutilstestRFS.java](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/packages/ct-utils-test/src/java/src/com/example/ctutilstest/ctutilstestRFS.java "ctutilstestRFS.java") to know about how to apply stack service templates.

##### Model
```
module: custom-template
  +--rw custom-template* [name device owner]
     +--rw name                        string
     +--rw device                      -> /ncs:devices/device/name
     +--rw owner                       string
     +--rw (apply-times)?
     |  +--:(single)
     |  |  +--rw variable* [name]
     |  |     +--rw name     string
     |  |     +--rw value    string
     |  +--:(multiple)
     |     +--rw iteration* [number]
     |        +--rw number      uint16
     |        +--rw variable* [name]
     |           +--rw name     string
     |           +--rw value    string
     +--rw ctx-path?                   string
     +--rw extra-var* [name]
        +--rw name     string
        +--rw value    string
```  

## Create Templates
### Pre-requisite
Create a netsim device and start ncs and netsim

```
ncs-netsim create-network cisco-ios 1 cpe
ncs-netsim start
ncs
ncs_cli -u admin
request devices sync-from
exit
```

### Create feature template
* Apply configuration to a netsim device through NED

	```
	$ ncs_cli -u admin -C
	admin@ncs# config
	admin@ncs(config)# devices device cpe0 config
	admin@ncs(config-config)# ios:username abhatta password abhatta privilege 0
	```
* Get template contents

	```
	admin@ncs(config-config)# commit dry-run outformat xml
	result-xml <devices xmlns="http://tail-f.com/ns/ncs">
	             <device>
	               <name>cpe0</name>
	               <config>
	                 <username xmlns="urn:ios">
	                   <name>abhatta</name>
	                   <privilege>0</privilege>
	                   <password>
	                     <secret>abhatta</secret>
	                   </password>
	                 </username>
	               </config>
	             </device>
	           </devices>
	```

* Enclose the template within `<config-template xmlns="http://tail-f.com/ns/config/1.0">{%template}</config-template>` and add variables and tags.

	```
	<config-template xmlns="http://tail-f.com/ns/config/1.0">
	  <devices xmlns="http://tail-f.com/ns/ncs">
	    <device>
	      <name>{$DEVICE_NAME}</name>
	      <config>
		  	<username xmlns="urn:ios" tags="merge">
		      <name>{$CPE_USERNAME}</name>
		      <privilege>{$CPE_USER_PREVILEGE}</privilege>
		      <password>
		        <secret>{$CPE_PASSWORD}</secret>
		      </password>
		    </username>
		  </config>
	    </device>
	  </devices>
	</config-template>
	```
> **Refer** [Template variable naming restrictions](#template-variable-naming-restrictions "") section for variable naming convention.
> 
> **Refer** [Custom Template naming restriction](#cutom-templates-naming-restriction "") section for custom template naming convention.
> 
> **Refer** [here](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/tree/master/packages/ct-utils-test/templates "") for sample feature templates.
> 
> ***These templates have to reside in `templates` directory in ncs package.***

### Create device templates
* Create a device template under NSO device tree

	```
	$ ncs_cli -u admin
	admin@ncs> configure
	admin@ncs% set devices template ct-ios_user ned-id cisco-ios-cli-6.22 config ios:username abhatta privilege 0 password secret abhatta
	```
* Get template contents

	```
	admin@ncs% commit dry-run outformat xml
	result-xml {
	    local-node {
	        data <devices xmlns="http://tail-f.com/ns/ncs">
	               <template>
	                 <name>ct-ios_user</name>
	                 <ned-id>
	                   <id xmlns:cisco-ios-cli-6.22="http://tail-f.com/ns/ned-id/cisco-ios-cli-6.22">cisco-ios-cli-6.22:cisco-ios-cli-6.22</id>
	                   <config>
	                     <username xmlns="urn:ios">
	                       <name>abhatta</name>
	                       <privilege>0</privilege>
	                       <password>
	                         <secret>abhatta</secret>
	                       </password>
	                     </username>
	                   </config>
	                 </ned-id>
	               </template>
	             </devices>
	    }
	}
	```

* Save the devices section (content within `<devices></devices>` tag) to a file and add variables.

	```
	<devices xmlns="http://tail-f.com/ns/ncs">
	  <template>
	    <name>ct-ios_user</name>
	    <ned-id>
	      <id xmlns:cisco-ios-cli-6.22="http://tail-f.com/ns/ned-id/cisco-ios-cli-6.22">cisco-ios-cli-6.22:cisco-ios-cli-6.22</id>
	      <config>
	        <username xmlns="urn:ios">
	          <name>{$CPE_USERNAME}</name>
	          <privilege>{$CPE_USER_PREVILEGE}</privilege>
	          <password>
	            <secret>{$CPE_PASSWORD}</secret>
	          </password>
	        </username>
	      </config>
	    </ned-id>
	  </template>
	</devices>
	```
> **Refer** [Template variable naming restrictions](#template-variable-naming-restrictions "") section for variable naming convention.
> 
> **Refer** [Custom Template naming restriction](#cutom-templates-naming-restriction "") section for custom template naming convention.
> 
> **Refer** [here](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/global-templates/custom-template.xml "") for sample global device templates.
> 
> ***These templates have to be `load merge` in to nso.***

## Create templates using templatize feature on NSO 4.6 onwards
### Pre-requisites
Device on which template is to be generated, must present in ncs device tree and must be pre-configured and `sync-from` must has been executed.

***Note:*** templatize feature may not work to generate templates for all types of configs. This feature can only be used if device already has the configs for which template is to be generated.

```
admin@ncs> templatize devices device cpe0 config ios:interface GigabitEthernet
Found potential templates at:
  devices device cpe0 \ config \ ios:interface GigabitEthernet {$GigabitEthernet-name}

Template path:
  devices device cpe0 \ config \ ios:interface GigabitEthernet {$GigabitEthernet-name}
Variables in template:
  {$GigabitEthernet-name}

<config xmlns="http://tail-f.com/ns/config/1.0">
  <devices xmlns="http://tail-f.com/ns/ncs">
    <device>
      <name>cpe0</name>
      <config>
        <interface xmlns="urn:ios">
          <GigabitEthernet>
            <name>{$GigabitEthernet-name}</name>
            <ip>
              <no-address>
                <address>false</address>
              </no-address>
            </ip>
          </GigabitEthernet>
        </interface>
      </config>
    </device>
  </devices>
</config>
```

```
admin@ncs% set devices device cpe0 config ios:vlan vlan-list 1-5
[ok][2018-05-03 12:38:48]

[edit]
admin@ncs% commit
Commit complete.
[ok][2018-05-03 12:38:52]

[edit]
admin@ncs% exit
[ok][2018-05-03 12:38:54]
admin@ncs> templatize devices device cpe0 config ios:vlan
Found potential templates at:
  devices device cpe0 \ config \ ios:vlan vlan-list {$vlan-list-id}

Template path:
  devices device cpe0 \ config \ ios:vlan vlan-list {$vlan-list-id}
Variables in template:
  {$vlan-list-id}

<config xmlns="http://tail-f.com/ns/config/1.0">
  <devices xmlns="http://tail-f.com/ns/ncs">
    <device>
      <name>cpe0</name>
      <config>
        <vlan xmlns="urn:ios">
          <vlan-list>
            <id>{$vlan-list-id}</id>
          </vlan-list>
        </vlan>
      </config>
    </device>
  </devices>
</config>
```
### Create feature templates
Get the contents within `<devices></devices>` tag and enclose within `<config-template xmlns="http://tail-f.com/ns/config/1.0">{%template}>` and add variables and tags

## Lux Test
Lux test covering all custom-template-utils feature tests is located [here](https://wwwin-github.cisco.com/nso-contributions/custom-template-project/blob/master/test/internal/lux/basic/run.lux "run.lux").

## Build Job
custom-template-utils pipeline build job is located [here](https://engci-private-sjc.cisco.com/jenkins/nso/job/NSO-Function-Pack/job/custom-template-utils/ "").

## Artifacts
custom-template-utils artifacts are pushed by build job to [here](http://engci-maven-master.cisco.com/artifactory/nso-release/nso/package-library/components/custom-template-utils/ "").

In above link go inside latest nso version directory to get the latest artifact by date.


FOR LSA

1. To update the packages

   make pkg-update

2. For LSA Setup

   cd test/internal/lux/ct-utils-lsa
   sh setup.sh

3. To run lux script for LSA

   cd test/internal/lux
   lux -v ct-lsa.lux

CUSTOM ACTION TO SYNC RFS AND CFS CUSTOM-TEMPLATE-INFO.

Usecase:
  This custom action is used to sync RFS and CFS custom-template-infos.

  ```
  request custom-template-actions sync-custom-templates
  success true
  detail Successfully sync'd custom template info.
  ```

  Check both RFS and CFS cusomt-template-info.
