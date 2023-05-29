---------------------------------------------------------
KNOWN ISSUES
---------------------------------------------------------

----------------------------------------------------------------------------
Get-Modifications actions for SR-TE-POLICY, SR-ODN
----------------------------------------------------------------------------
1. SERVICE Get-modifications SR-TE-POLICIES

  Get the data this service created. In CLI and XML format.

    request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action get-modifications
      Sample output:
                  message  devices {
                    device P-0 {
                      ...
                  }
    OR
    request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action get-modifications outformat cli
      Sample output:
                  message  devices {
                    device P-0 {
                      ...
                  }
    OR
    request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action get-modifications outformat xml
      Sample output:
                  message  <devices xmlns="http://tail-f.com/ns/ncs">
                            <device>
                                 ...
                            </device>
                           </devices>

2. SERVICE Get-modifications SR-ODN

  Get the data this service created. In CLI and XML format.

    request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action get-modifications
      Sample output:
                  message  devices {
                    device P-0 {
                      ...
                  }
    OR
    request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action get-modifications outformat cli
      Sample output:
                  message  devices {
                    device P-0 {
                      ...
                  }
    OR
    request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action get-modifications outformat xml
      Sample output:
                  message  <devices xmlns="http://tail-f.com/ns/ncs">
                            <device>
                                 ...
                            </device>
                           </devices>

3. SR-ODN Get-modifications per device

    request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action get-modifications device <device-name>
    Sample output:
                  device device_name {
                      ...
                  }
    request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action get-modifications device <device-name> outformat cli
        Sample output:
                  device device_name {
                      ...
                  }
    request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action get-modifications device <device-name> outformat xml
          Sample output:
                  message <device>
                            <name>device_name</name>
                                 ...
                            </device>

---------------------------------------------------------
Custom Actions for SR-TE Policy
---------------------------------------------------------
1. Policy SERVICE REDEPLOY

    Service level redeploy is also possible to redeploy config again.

        request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action redeploy
        
2. Policy SERVICE REDEPLOY RECONCILE

    Data that is part of service config but not owned by service can be kept or discarded using reconcile option.
    "redeploy reconcile" shares same behavior and input structure as native "re-deploy reconcile".
  
        request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action redeploy reconcile { discard-non-service-config }
        OR
        request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action redeploy reconcile { keep-non-service-config }
        OR 
        (default reconcile choice is keep-non-service-config)
        request sr-te cisco-sr-te-cfp-sr-policies:policies policy <service-name> action redeploy reconcile { }



---------------------------------------------------------
Custom Actions for SR ODN TEMPLATE
---------------------------------------------------------
1. ODN Head-End REDEPLOY

    If for some reason a head-end is down and plan is failed, now the device comes back up, we can run redeploy on the head-end to push the config again.

        request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> head-end <head-end> action redeploy

2. ODN SERVICE REDEPLOY

    Service level redeploy is also possible to redeploy config again.

        request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action redeploy
    
3. ODN SERVICE REDEPLOY RECONCILE

    Data that is part of service config but not owned by service can be kept or discarded using reconcile option.
    "redeploy reconcile" shares same behavior and input structure as native "re-deploy reconcile".
  
        request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action redeploy reconcile { discard-non-service-config }
        OR
        request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action redeploy reconcile { keep-non-service-config }
        OR 
        (default reconcile choice is keep-non-service-config)
        request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> action redeploy reconcile { }
        OR
        (head-end redeploy has same input as service redeploy)
        request sr-te cisco-sr-te-cfp-sr-odn:odn odn-template <service-name> head-end <head-end> action redeploy reconcile { }

---------------------------------------------------------
Service Cleanup Action for SR-TE services
---------------------------------------------------------

If sr-te service is deleted but fails to delete completely and plan is stuck, following action can be used to cleanup service-data.
Scenarios like a device not reachable during deletion can cause service deletion to fail.

  1. Cleanup sr-policy service

    request sr-te cleanup service <service-name> service-type <service-type> device <device-name> no-networking <true/false>

      service-name : This is the name of the service
      service-type : This can be one of sr-policy, or sr-odn.
      device: If this is provided only this device related data will be cleaned up.
      no-networking : When this is set to false, cleanup action will try to remove config from device. This flag is a mandatory field. In scenarios where device is not-reachable during delete, this flag should be set to true.

  2. Auto cleanup

      There is a auto-cleanup flag which is 'false' by default, can be turned 'true'. This means if the device is down, CFP will auto cleanup the device realted config from NSO. It is upto the user to then cleanup actual device when it comes back up. A sync-to from NSO will help in bringing both NSO and device in sync.

---------------------------------------------------------
Enabling stacked-service approach for TSDN demo
---------------------------------------------------------

Enabling this setting will create services based on stacked-service concept. For the end-user this means being able to use commit dry-run, get-modifications and other features from NSO platform.

Also note, this approach can be used only if subscriber based services are not present in the system, meaning, only one approach can be used at a time & there should be no services of the other approach in the system.

To enable stacked service, this flag needs to be set:

    set cisco-sr-te-cfp:cfp-configurations stacked-service-enabled
    commit

