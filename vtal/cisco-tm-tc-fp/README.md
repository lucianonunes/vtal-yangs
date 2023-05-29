---------------------------------------------------------
KNOWN ISSUES
---------------------------------------------------------

---------------------------------------------------------
Service Cleanup Feature for TM TC
---------------------------------------------------------
If tm-tc service is deleted but fails to delete completely and plan is stuck, you can use the clean up action to remove any remaining service data.
Scenarios such as "device not reachable" during deletion can cause service deletion to fail.

  1. Cleanup tm-tc service

    Remove a specific service

      request tm-tc-actions cleanup service <service-name>

    You may also specify a specific node to remove from the service

      request tm-tc-actions cleanup service <service-name>

---------------------------------------------------------
Get-Modifications actions for TM TC
---------------------------------------------------------
  1. SERVICE get-modifications

    Get service data in either CLI or XML format.

      request tm-tc <service-name> action get-modifications

      OR

      request tm-tc <service-name> action get-modifications outformat cli

      OR

      request tm-tc <service-name> action get-modifications outformat xml

  2. Get-Modifications per device

    Get service data for a specific node/device in either CLI or XML format.

      request tm-tc <service-name> action get-modifications device <node-name>

      OR

      request tm-tc <service-name> action get-modifications device <node-name> outformat cli

      OR

      request tm-tc <service-name> action get-modifications device <node-name> outformat xml

---------------------------------------------------------
Custom Actions for TM TC
---------------------------------------------------------
  1. Node REDEPLOY

    Redeploy on an available node in case of a failed plan.

      request tm-tc <service-name> node <node-name> action redeploy

  2. TM TC SERVICE REDEPLOY

    Service level redeploy to push config on all nodes.

      request tm-tc <service-name> action redeploy
