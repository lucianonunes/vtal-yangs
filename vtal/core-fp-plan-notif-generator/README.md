core-fp-plan-notif-generator v1.0.0
===================================

This package contains common code and yang models to send plan deletion notification to be
used by Core Function Packs.

USAGE:

1. To generate a plan deletion notification on 'service-state-changes' stream,
following config should be set in ncs.conf:

    ```
    <stream>
      <name>service-state-changes</name>
      <description>Plan state transitions according to
      tailf-ncs-plan.yang</description>
      <replay-support>true</replay-support>
      <builtin-replay-store>
        <enabled>true</enabled>
        <dir>./state</dir>
        <max-size>S10M</max-size>
        <max-files>50</max-files>
      </builtin-replay-store>
    </stream>

2. Set the plan notification paths as shown below:

    ```
    set plan-path-for-notification /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/policy-plan
    service-path /cisco-sr-te-cfp:sr-te/cisco-sr-te-cfp-sr-policies:policies/policy
    service-key-elements [ name ]

3. On deletion of the plan on above path, notification will be generated as below:

    ```
    admin@ncs> show notification stream service-state-changes last 1
      notification {
          eventTime 2020-02-05T15:34:21.377904+00:00
          plan-state-change {
              service /sr-te/cisco-sr-te-cfp-sr-policies:policies/policy[name='SR-CLI-DYNAMIC']
              operation deleted
          }
      }