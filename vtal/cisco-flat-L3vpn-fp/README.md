---------------------------------------------------------
KNOWN ISSUES
---------------------------------------------------------

---------------------------------------------------------
Service Cleanup Feature for L3VPN
---------------------------------------------------------
If l3vpn service is deleted but fails to delete completely and plan is stuck, following action can be used to cleanup service-data.
Scenarios like a device not reachable during deletion can cause service deletion to fail.

  1. Cleanup l3vpn service

    request flat-L3vpn-actions cleanup service <service-name> device <endpoint-to-clean> no-networking <true/false>

      service-name : This is the name of the service. If no other params are given, entire service will be cleaned up.
      device: This is the endpoint which needs to be cleaned up. If provided only this endpoint will be cleaned from service.
      no-networking : When this is set to false, cleanup action will try to remove config from device. This flag is a mandatory field. In scenarios where device is not-reachable during delete, this flag should be set to true.

  2. Auto cleanup

      There is a auto-cleanup flag which is 'false' by default, can be turned 'true'. This means if the device is down, CFP will auto cleanup the device realted config from NSO. It is upto the user to then cleanup actual device when it comes back up. A sync-to from NSO will help in bringing both NSO and device in sync.

---------------------------------------------------------
Get-Modifications actions for L3VPN
---------------------------------------------------------
1. SERVICE Get-modifications

  Get the data this service created. In CLI and XML format.

    request flat-L3vpn <service-name> action get-modifications
      Sample output:
                  message  devices {
                    device P-0 {
                      ...
                  }
    OR
    request flat-L3vpn <service-name> action get-modifications outformat cli
      Sample output:
                  message  devices {
                    device P-0 {
                      ...
                  }
    OR
    request flat-L3vpn <service-name> action get-modifications outformat xml
      Sample output:
                  message  <devices xmlns="http://tail-f.com/ns/ncs">
                            <device>
                                 ...
                            </device>
                           </devices>

2. Get-modifications per device

    request flat-L3vpn <service-name> action get-modifications endpoint <device-name>
    Sample output:
                  device device_name {
                      ...
                  }
    request flat-L3vpn <service-name> action get-modifications endpoint <device-name> outformat cli
        Sample output:
                  device device_name {
                      ...
                  }
    request flat-L3vpn <service-name> action get-modifications endpoint <device-name> outformat xml
          Sample output:
                  message <device>
                            <name>device_name</name>
                                 ...
                            </device>


---------------------------------------------------------
Custom Actions for L3VPN
---------------------------------------------------------
1. Endpoint REDEPLOY

    If for some reason an endpoint is down and plan is failed, now the device comes back up, we can run redeploy on the endpoint to push the config again.

        request flat-L3vpn <service-name> endpoint <endpoint-name> action redeploy

2. L3VPN SERVICE REDEPLOY

    Service level redeploy is also possible to redeploy config on all endpoints again.

        request flat-L3vpn <service-name> action redeploy
        
3. REDEPLOY RECONCILE
  
    Data that is part of service config but not owned by service can be kept or discarded using reconcile option.
    "redeploy reconcile" shares same behavior and input structure as native "re-deploy reconcile".
  
        request flat-L3vpn <service-name> action redeploy reconcile { discard-non-service-config }
        OR
        request flat-L3vpn <service-name> action redeploy reconcile { keep-non-service-config }
        OR 
        (default reconcile choice is keep-non-service-config)
        request flat-L3vpn <service-name> action redeploy reconcile { }
        OR
        (endpoint redeploy has same input as service redeploy)
        request flat-L3vpn <service-name> endpoint <endpoint-name> action redeploy reconcile { }


---------------------------------------------------------
    Custom Templates
---------------------------------------------------------
  Custom templates can be used to apply configs directly on devices
  custom-template added under flat-L3vpn and flat-L3vpn -> endpoint

  Name of custom templates must start with either "ct-" or "CT-"

  a. Enable/Disable Custom Templates

    If apply-custom-template leaf is set to 'true', then only custom-templates will get applied
    By default apply-custom-template is set to 'true'

    admin@ncs% show apply-custom-template
    apply-custom-template true;

  b. Sample custom templates
      <devices xmlns="http://tail-f.com/ns/ncs">
        <template>
          <name>CT-CLI-banner</name> ----------------------> CUSTOM TEMPLATE NAME
          <ned-id>
            <id xmlns:cisco-iosxr-cli-7.22="http://tail-f.com/ns/ned-id/cisco-iosxr-cli-7.22">
                    cisco-iosxr-cli-7.22:cisco-iosxr-cli-7.22</id>
            <config>
              <logging xmlns="http://tail-f.com/ned/cisco-ios-xr">
                <history>informational</history>
              </logging>
              <banner xmlns="http://tail-f.com/ned/cisco-ios-xr">
                <login>
                  <start-marker>'</start-marker>
                  <message>{$BANNER_TEXT}</message>  ------------------> VARIABLE
                  <end-marker>'</end-marker>
                </login>
              </banner>
            </config>
          </ned-id>
        </template>
        <template>
          <name>CT-CLI-logging</name> ----------------------> CUSTOM TEMPLATE NAME
            <ned-id>
              <id xmlns:cisco-iosxr-cli-7.22="http://tail-f.com/ns/ned-id/cisco-iosxr-cli-7.22">
                    cisco-iosxr-cli-7.22:cisco-iosxr-cli-7.22</id>
                <config>
                  <logging xmlns="http://tail-f.com/ned/cisco-ios-xr">
                    <history>informational</history>
                  </logging>
                </config>
            </ned-id>
          </template>
        </devices>

    CT-CLI-banner template will apply banner config on device
    CT-CLI-logging template will set logging config on device

    c. Sample Payloads
            Previous Payload sample:
            <config xmlns="http://tail-f.com/ns/config/1.0">
                <flat-L3vpn xmlns="http://cisco.com/ns/nso/fp/examples/cisco-tsdn-flat-L3vpn">
                    <name>L3</name>
                    <endpoint>
                        <endpoint-name>cli-1</endpoint-name>
                        <access-pe>PIOSXR-0</access-pe>
                    </endpoint>
                </flat-L3vpn>
            </config>

            Current Payload sample:
            <config xmlns="http://tail-f.com/ns/config/1.0">
              <flat-L3vpn xmlns="http://cisco.com/ns/nso/fp/examples/cisco-tsdn-flat-L3vpn">
                <name>L3</name>
                <custom-template>
                  <name>CT-CLI-logging</name>
                </custom-template>
                <endpoint>
                  <endpoint-name>cli-1</endpoint-name>
                  <access-pe>PIOSXR-0</access-pe>
                  <custom-template>
                    <name>CT-CLI-banner</name>
                    <variable>
                      <name>BANNER_TEXT</name>
                      <value>Welcome</value>
                    </variable>
                  </custom-template>
                </endpoint>
              </flat-L3vpn>
            </config>

    d. Verify configs on device:

        Configs pushed by CT-CLI-banner template:
          admin@ncs% show devices device PIOSXR-0 config cisco-ios-xr:banner login message
          message Welcome;

        Configs pushed by CT-CLI-logging template:
          admin@ncs% show devices device PIOSXR-0 config cisco-ios-xr:logging history
          history informational;

---------------------------------------------------------
Enabling stacked-service approach for TSDN demo
---------------------------------------------------------

Enabling this setting will create services based on stacked-service concept. For the end-user this means being able to use commit dry-run, get-modifications and other features from NSO platform.

Also note, this approach can be used only if subscriber based services are not present in the system, meaning, only one approach can be used at a time & there should be no services of the other approach in the system.

To enable stacked service, this flag needs to be set:

    set cisco-flat-L3vpn-fp:cfp-configurations stacked-service-enabled
    commit