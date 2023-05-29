# IETF-L3NM

**This is a sample IETF-L3NM function pack**

This Function Package implements the following IETF model: 
https://tools.ietf.org/html/draft-ietf-opsawg-l3sm-l3nm-03
and uses L3VPN as a internal stacked service for service implementation.

## IETF-L3NM Yang Tree Implementation Coverage
```
========== Service Model Tree ==========
module: ietf-l3vpn-ntw
  +--rw l3vpn-ntw
  |  +--rw vpn-profiles
  |  |  +--rw valid-provider-identifiers
  |  |     +--rw routing-profile-identifier* [id]
  |  |        +--rw id    string
  |  +--rw vpn-services
  |     +--rw vpn-service* [vpn-id]
  |     |  +--rw vpn-id                      l3vpn-svc:svc-id
  |     |  +--rw ie-profiles
  |     |  |  +--rw ie-profile* [ie-profile-id]
  |     |  |     +--rw ie-profile-id    string
  |     |  |     +--rw rd?              rt-types:route-distinguisher
  |     |  |     +--rw vpn-targets
  |     |  |        +--rw vpn-target* [id]
  |     |  |        |  +--rw id                   int8
  |     |  |        |  +--rw route-targets* [route-target]
  |     |  |        |  |  +--rw route-target    rt-types:route-target
  |     |  |        |  +--rw route-target-type    rt-types:route-target-type
  |     |  |        +--rw vpn-policies
  |     |  |           +--rw import-policy?   -> /l3vpn-ntw/vpn-profiles/valid-provider-identifiers/routing-profile-identifier/id
  |     |  |           +--rw export-policy?   -> /l3vpn-ntw/vpn-profiles/valid-provider-identifiers/routing-profile-identifier/id
  |     |  +--rw vpn-nodes
  |     |  |  +--rw vpn-node* [ne-id]
  |     |  |     +---x error-recovery
  |     |  |     |  +---w input
  |     |  |     |  |  +---w vpn-network-access-id    string
  |     |  |     |  |  +---w sync-direction           enumeration
  |     |  |     |  +--ro output
  |     |  |     |     +--ro success    boolean
  |     |  |     |     +--ro detail?    string
  |     |  |     +--rw local-autonomous-system?   inet:as-number
  |     |  |     +--rw ne-id                      string
  |     |  |     +--rw rd?                        rt-types:route-distinguisher
  |     |  |     +--rw vpn-targets
  |     |  |     |  +--rw vpn-target* [id]
  |     |  |     |  |  +--rw id                   int8
  |     |  |     |  |  +--rw route-targets* [route-target]
  |     |  |     |  |  |  +--rw route-target    rt-types:route-target
  |     |  |     |  |  +--rw route-target-type    rt-types:route-target-type
  |     |  |     |  +--rw vpn-policies
  |     |  |     |     +--rw import-policy?   -> /l3vpn-ntw/vpn-profiles/valid-provider-identifiers/routing-profile-identifier/id
  |     |  |     |     +--rw export-policy?   -> /l3vpn-ntw/vpn-profiles/valid-provider-identifiers/routing-profile-identifier/id
  |     |  |     +--rw vpn-network-accesses
  |     |  |     |  +--rw vpn-network-access* [id]
  |     |  |     |     +--rw id                   l3vpn-svc:svc-id
  |     |  |     |     +--rw port-id?             l3vpn-svc:svc-id
  |     |  |     |     +--rw connection
  |     |  |     |     |  +--rw encapsulation-type?   identityref
  |     |  |     |     |  +--rw tagged-interface
  |     |  |     |     |     +--rw type?                identityref
  |     |  |     |     |     +--rw dot1q-vlan-tagged {dot1q}?
  |     |  |     |     |        +--rw cvlan-id?   uint16
  |     |  |     |     |        +--rw BDI?        uint16
  |     |  |     |     +--rw ip-connection
  |     |  |     |     |  +--rw ipv4 {l3vpn-svc:ipv4}?
  |     |  |     |     |  |  +--rw address-allocation-type?   identityref
  |     |  |     |     |  |  +--rw static-addresses
  |     |  |     |     |  |     +--rw primary-address?   -> /l3vpn-ntw/vpn-services/vpn-service/vpn-nodes/vpn-node/vpn-network-accesses/vpn-network-access/ip-connection/ipv4/static-addresses/address/address-id
  |     |  |     |     |  |     +--rw address* [address-id]
  |     |  |     |     |  |        +--rw address-id          string
  |     |  |     |     |  |        +--rw provider-address?   inet:ipv4-address
  |     |  |     |     |  |        +--rw prefix-length?      uint8
  |     |  |     |     |  +--rw ipv6 {l3vpn-svc:ipv6}?
  |     |  |     |     |     +--rw address-allocation-type?   identityref
  |     |  |     |     |     +--rw static-addresses
  |     |  |     |     |        +--rw primary-address?   -> /l3vpn-ntw/vpn-services/vpn-service/vpn-nodes/vpn-node/vpn-network-accesses/vpn-network-access/ip-connection/ipv6/static-addresses/address/address-id
  |     |  |     |     |        +--rw address* [address-id]
  |     |  |     |     |           +--rw address-id          string
  |     |  |     |     |           +--rw provider-address?   inet:ipv6-address
  |     |  |     |     |           +--rw prefix-length?      uint8
  |     |  |     |     +--rw routing-protocols
  |     |  |     |        +--rw routing-protocol* [id]
  |     |  |     |           +--rw id      string
  |     |  |     |           +--rw type?   identityref
  |     |  |     |           +--rw bgp {l3vpn-svc:rtg-bgp}?
  |     |  |     |              +--rw peer-autonomous-system            inet:as-number
  |     |  |     |              +--rw local-autonomous-system?          inet:as-number
  |     |  |     |              +--rw address-family*                   l3vpn-svc:address-family
  |     |  |     |              +--rw redistribute-connected-ipv4-af!
  |     |  |     |              |  +--rw metric?   uint32
  |     |  |     |              +--rw redistribute-connected-ipv6-af!
  |     |  |     |              |  +--rw metric?   uint32
  |     |  |     |              +--rw update-source!
  |     |  |     |              |  +--rw if-type      enumeration
  |     |  |     |              |  +--rw if-id        string
  |     |  |     |              |  +--rw sub-if-id?   int32
  |     |  |     |              +--rw mpls-deactivation?                boolean
  |     |  |     |              +--rw neighbor*                         inet:ip-address
  |     |  |     |              +--rw multihop?                         uint8
  |     |  |     +--rw node-ie-profile?           -> /l3vpn-ntw/vpn-services/vpn-service/ie-profiles/ie-profile/ie-profile-id
  |     |  +--rw custom-template* [name]
  |     |  +---x self-test
  |     |  |  +--ro output
  |     |  |     +--ro status?    string
  |     |  |     +--ro message?   string
  |     |  +---x error-recovery
  |     |     +---w input
  |     |     |  +---w sync-direction    enumeration
  |     |     +--ro output
  |     |        +--ro success    boolean
  |     |        +--ro detail?    string
  +--rw l3nm-actions
     +---x cleanup
     |  +---w input
     |  |  +---w service                  string
     |  |  +---w vpn-node?                string
     |  |  +---w vpn-network-access-id    string
     |  |  +---w no-networking            boolean
     |  +--ro output
     |     +--ro success    boolean
     |     +--ro detail?    string
     +---x internal-plan-change-handler
     |  +---w input
     |     +---w kicker-id?   string
     |     +---w path?        tailf:node-instance-identifier
     |     +---w tid?         uint32
     +---x error-recovery
        +---w input
        |  +---w service                  string
        |  +---w vpn-node?                string
        |  +---w vpn-network-access-id    string
        |  +---w sync-direction           enumeration
        +--ro output
           +--ro success    boolean
           +--ro detail?    string

========== Service Plan Model Tree ==========
module: ietf-l3vpn-ntw
  +--rw l3vpn-ntw
     +--rw vpn-services
        +--ro vpn-service-plan* [vpn-id]
           +--ro vpn-id        string
           +--ro plan
           |  +--ro component* [type name]
           |  |  +--ro name                string
           |  |  +--ro type                plan-component-type-t
           |  |  +--ro state* [name]
           |  |  |  +--ro name                   plan-state-name-t
           |  |  |  +--ro status?                plan-state-status-t
           |  |  |  +--ro when?                  yang:date-and-time
           |  +--ro failed?               empty
           |  +--ro error-info!
           |  |  +--ro message?     string
           |  |  +--ro log-entry?   instance-identifier
```

**NSO Modifications in IETF-L3NM Yang Model**

There are serveral modifications in IETF-L3NM model to allow compatibility with L3VPN

  *vpn-id* : Added pattern restriction
    
    OLD : type string
    NEW : type string, pattern "[a-zA-Z0-9\\-_]+"
    Notes : vpn-id is used as a key for internal service. This means we must match the pattern 
            restrictions imposed by internal service.

  *svc-input-bandwidth* : mandatory true -> false

  *svc-output-bandwidth* : mandatory true -> false
  
  *svc-mtu* : mandatory true -> false
  
  *rd* : Type change
  
    OLD : type union (empty, rt-types:route-distinguisher)
    NEW : type leaf (rt-types:route-distinguisher)
    Notes : COMPILE ERROR: NSO does not support YANG 1.1 type union with empty leaves

  *ne-id* : Type change
        OLD : type string
        NEW : type leafref /core-fp-common:dispatch-map/core-fp-common:device
  
**NSO Additions in IETF-L3NM Yang Model**

There are a few additions to IETF-L3NM model to allow compatibility with L3VPN

  *redistribute-connected-ipv4-af* : Empty leaf
  
  *redistribute-connected-ipv6-af* : Empty leaf

**NSO Constraints in IETF-L3NM Model**

There are several elements in IETF-L3NM model that have additional constraints to be compatible with
L3VPN service model

  *routing-profile-identifier:id* : Reference constraint
  
    id must reference valid l3vpn-route-policy element
    
  *vpn-network-access* : Type constraint
  
    L3NM (vpn-network-access) : type list
    Notes : Due to L3VPN model structure, we must have at least a single vpn-network-access entry to be mapped 
    
  *routing-protocols* : Type constraint
  
    L3NM (routing-protocols) : type list
    L3VPN (routing -> e-bgp) : List Max Size (1), case bgp
    Notes : Due to L3VPN model structure, we can only allow a single bgp routing protocol to be mapped
    

## Configuration Examples:

**Sample L3NM Service w/ BGP**
```
NSO Service Input:
=================
vpn-profiles {
    valid-provider-identifiers {
        routing-profile-identifier TEST_POLICY;
    }
}
vpn-services {
    vpn-service 0-65008740 {
        ie-profiles {
            ie-profile ie_00 {
                rd 0:65100:87400024;
                vpn-targets {
                    vpn-target 1 {
                        route-targets 0:65010:17401;
                        route-targets 0:65010:17402;
                        route-target-type both;
                    }
                    vpn-target 2 {
                        route-targets 0:65010:17403;
                        route-target-type import;
                    }
                    vpn-target 3 {
                        route-targets 0:65010:17404;
                        route-target-type export;
                    }
                    vpn-policies {
                        export-policy TEST_POLICY;
                    }
                }
            }
        }
        vpn-nodes {
            vpn-node PIOSXR-0 {
                local-autonomous-system 65001;
                vpn-targets {
                    vpn-target 1 {
                        route-targets 0:65010:17405;
                        route-targets 0:65010:17406;
                        route-target-type both;
                    }
                }
                vpn-network-accesses {
                    vpn-network-access 23 {
                        port-id GigabitEthernet1/1/1/1;
                        connection {
                            encapsulation-type tagged-int;
                            tagged-interface {
                                type dot1q;
                                dot1q-vlan-tagged {
                                    cvlan-id 1234;
                                }
                            }
                        }
                        ip-connection {
                            ipv6 {
                                address-allocation-type static-address;
                                static-addresses {
                                    primary-address test-ipv6-address;
                                    address test-ipv6-address {
                                        provider-address 2001:db8::1;
                                        prefix-length    32;
                                    }
                                }
                            }
                        }
                        routing-protocols {
                            routing-protocol TEST_PROTO {
                                type bgp;
                                bgp {
                                    peer-autonomous-system 65003;
                                    address-family         [ ipv6 ];
                                    neighbor               [ 2001:db8::2 ];
                                    multihop               12;
                                }
                            }
                        }
                    }
                }
                node-ie-profile         ie_00;
            }
            vpn-node PIOSXR-1 {
                local-autonomous-system 65001;
                vpn-network-accesses {
                    vpn-network-access 23 {
                        port-id GigabitEthernet1/1/1/1;
                        connection {
                            encapsulation-type tagged-int;
                            tagged-interface {
                                type dot1q;
                                dot1q-vlan-tagged {
                                    cvlan-id 1234;
                                }
                            }
                        }
                        ip-connection {
                            ipv4 {
                                address-allocation-type static-address;
                                static-addresses {
                                    primary-address test-ipv4-address;
                                    address test-ipv4-address {
                                        provider-address 10.1.1.1;
                                        prefix-length    24;
                                    }
                                }
                            }
                        }
                        routing-protocols {
                            routing-protocol TEST_PROTO {
                                type bgp;
                                bgp {
                                    peer-autonomous-system  65003;
                                    local-autonomous-system 65002;
                                    address-family          [ ipv4 ];
                                    neighbor                [ 10.1.1.1 ];
                                    multihop                11;
                                }
                            }
                        }
                    }
                }
                node-ie-profile         ie_00;
            }
        }
    }
}

NSO Device Output (PIOSXR-0):
============================
vrf {
    vrf-list 0-65008740 {
        address-family {
            ipv6 {
                unicast {
                    import {
                        route-target {
                            address-list 65010:17405;
                            address-list 65010:17406;
                        }
                    }
                    export {
                        route-target {
                            address-list 65010:17405;
                            address-list 65010:17406;
                        }
                    }
                }
            }
        }
    }
}
logging {
    history informational;
}
interface {
    GigabitEthernet 1/1/1/1 {
        description "T-SDN Interface";
    }
    GigabitEthernet-subinterface {
        GigabitEthernet 1/1/1/1.1234 {
            description "T-SDN Interface";
            encapsulation {
                dot1q {
                    vlan-id [ 1234 ];
                }
            }
            vrf         0-65008740;
            ipv6 {
                address {
                    prefix-list 2001:db8::1/32;
                }
            }
        }
    }
}
extcommunity-set {
    opaque COLOR_100 {
        set 100;
    }
    opaque COLOR_101 {
        set 101;
    }
}
route-policy PASS_ALL {
    value pass;
}
route-policy SET_COLORv4_TEST_POLICY {
    value "  if destination in (1.1.1.1/32, 1.1.1.2/32) then\r\n    set extcommunity color COLOR_100\r\n  endif\r\n  if destination in (2.1.1.1/32, 2.1.1.2/32) then\r\n    set extcommunity color COLOR_101\r\n  endif\r\n";
}
router {
    bgp {
        bgp-no-instance 65001 {
            vrf 0-65008740 {
                rd 65100:87400024;
                address-family {
                    ipv6 {
                        unicast;
                    }
                }
                neighbor 2001:db8::2 {
                    remote-as 65003;
                    ebgp-multihop {
                        ttl-value 12;
                    }
                    address-family {
                        ipv6 {
                            unicast {
                                route-policy in {
                                    name PASS_ALL;
                                }
                                route-policy out {
                                    name PASS_ALL;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

NSO Device Output (PIOSXR-1):
============================
vrf {
    vrf-list 0-65008740 {
        address-family {
            ipv4 {
                unicast {
                    import {
                        route-target {
                            address-list 65010:17401;
                            address-list 65010:17402;
                            address-list 65010:17403;
                        }
                    }
                    export {
                        route-policy SET_COLORv4_TEST_POLICY;
                        route-target {
                            address-list 65010:17401;
                            address-list 65010:17402;
                            address-list 65010:17404;
                        }
                    }
                }
            }
        }
    }
}
logging {
    history informational;
}
interface {
    GigabitEthernet 1/1/1/1 {
        description "T-SDN Interface";
    }
    GigabitEthernet-subinterface {
        GigabitEthernet 1/1/1/1.1234 {
            description "T-SDN Interface";
            encapsulation {
                dot1q {
                    vlan-id [ 1234 ];
                }
            }
            vrf         0-65008740;
            ipv4 {
                address {
                    ip   10.1.1.1;
                    mask 255.255.255.0;
                }
            }
        }
    }
}
extcommunity-set {
    opaque COLOR_100 {
        set 100;
    }
    opaque COLOR_101 {
        set 101;
    }
}
route-policy PASS_ALL {
    value pass;
}
route-policy SET_COLORv4_TEST_POLICY {
    value "  if destination in (1.1.1.1/32, 1.1.1.2/32) then\r\n    set extcommunity color COLOR_100\r\n  endif\r\n  if destination in (2.1.1.1/32, 2.1.1.2/32) then\r\n    set extcommunity color COLOR_101\r\n  endif\r\n";
}
router {
    bgp {
        bgp-no-instance 65002 {
            vrf 0-65008740 {
                rd 65100:87400024;
                address-family {
                    ipv4 {
                        unicast;
                    }
                }
                neighbor 10.1.1.1 {
                    remote-as 65003;
                    ebgp-multihop {
                        ttl-value 11;
                    }
                    address-family {
                        ipv4 {
                            unicast {
                                route-policy in {
                                    name PASS_ALL;
                                }
                                route-policy out {
                                    name PASS_ALL;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
```

## L3NM -> L3VPN MAPPING
```
  L3NM: vpn-service | Path: l3vpn-ntw -> vpn-services -> vpn-service
  =====================
  vpn-service -> vpn-id     =>     flat-L3vpn -> name
  vpn-service -> vpn-id     =>     flat-L3vpn -> endpoint -> vrf -> vrf-definition


  L3NM: vpn-node | Path: l3vpn-ntw -> vpn-services -> vpn-service -> vpn-nodes -> vpn-node
  =====================
  vpn-node -> ne-id                                  =>      flat-L3vpn -> endpoint -> endpoint-name
  vpn-node -> ne-id                                  =>      flat-L3vpn -> endpoint -> access-pe (mandatory)
  vpn-node -> rd                                     =>      flat-L3vpn -> endpoint -> vrf -> route-distinguisher
  vpn-node -> local-autonomous-system (fallback)     =>      flat-L3vpn -> endpoint -> as-no


  L3NM: vpn-targets | Path: l3vpn-ntw -> vpn-services -> vpn-service -> vpn-nodes -> vpn-node -> vpn-targets
  =====================
  vpn-targets -> vpn-target -> route-targets -> route-target (substring)     =>     flat-L3vpn -> endpoint -> vrf -> address-family -> vpn-target -> rt-value
  vpn-targets -> vpn-target -> route-target-type                             =>     flat-L3vpn -> endpoint -> vrf -> address-family -> vpn-target -> rt-type
  vpn-targets -> vpn-policies -> export-policy                               =>     flat-L3vpn -> endpoint -> sr-te -> export-route-policy (mandatory if sr-te defined) 
  vpn-targets -> vpn-policies -> import-policy                               =>     flat-L3vpn -> endpoint -> sr-te -> import-route-policy (mandatory if sr-te defined)


  L3NM: vpn-network-access | Path: l3vpn-ntw -> vpn-services -> vpn-service -> vpn-nodes -> vpn-node -> vpn-network-accesses -> vpn-network-access
  =====================
  vpn-network-access -> port-id (substring)                                                                          =>     flat-L3vpn -> endpoint -> if-type (mandatory)
  vpn-network-access -> port-id (substring)                                                                          =>     flat-L3vpn -> endpoint -> if-id (mandatory)
  vpn-network-access -> ip-connection -> ipv4 -> static-addresses -> address -> provider-address + prefix-length     =>     flat-L3vpn -> endpoint -> pe-ip-addr (pe-ip-addr or pe-ipv6-addr mandatory)
  vpn-network-access -> ip-connection -> ipv6 -> static-addresses -> address -> provider-address + prefix-length     =>     flat-L3vpn -> endpoint -> pe-ipv6-addr (pe-ip-addr or pe-ipv6-addr mandatory)
  vpn-network-access -> connection -> tagged-interface -> dot1q-vlan-tagged -> cvlan-id                              =>     flat-L3vpn -> endpoint -> vlan-id (when if-type != Loopback or BVI)


  L3NM: bgp | Path: l3vpn-ntw -> vpn-services -> vpn-service -> vpn-nodes -> vpn-node -> vpn-network-accesses -> vpn-network-access -> routing-protocols -> routing-protocol -> bgp
  =====================
  bgp -> local-autonomous-system     =>     flat-L3vpn -> endpoint -> as-no
  bgp -> peer-autonomous-system      =>     flat-L3vpn -> endpoint -> ce-pe-prot (when as-no) -> e-bgp -> remote-as-ipv4 or remote-as-ipv6 (mandatory if neighbor is defined)
  bgp -> neighbor                    =>     flat-L3vpn -> endpoint -> ce-pe-prot (when as-no) -> e-bgp -> neighbor-ipv4 or neighbor-ipv6
  bgp -> multihop                    =>     flat-L3vpn -> endpoint -> ce-pe-prot (when as-no) -> e-bgp -> ebgp-multihop (presence container) -> ttl-value (mandatory true)
  bgp -> address-family              =>     flat-L3vpn -> endpoint -> vrf -> address-family -> address-family (key)


  L3NM: ie-profile | Path: l3vpn-ntw -> vpn-services -> ie-profiles -> ie-profile
  =====================
  ie-profile -> rd (fallback)                                                           =>     flat-L3vpn -> endpoint -> vrf -> route-distinguisher
  ie-profile -> vpn-targets (fallback) -> route-targets -> route-target (substring)     =>     flat-L3vpn -> endpoint -> vrf -> address-family -> vpn-target -> rt-value
  ie-profile -> vpn-targets (fallback) -> vpn-target -> route-target-type               =>     flat-L3vpn -> endpoint -> vrf -> address-family -> vpn-target -> rt-type
  ie-profile -> vpn-targets (fallback) -> vpn-policies -> export-policy                 =>     flat-L3vpn -> endpoint -> sr-te -> export-route-policy (mandatory if sr-te defined)
  ie-profile -> vpn-targets (fallback) -> vpn-policies -> import-policy                 =>     flat-L3vpn -> endpoint -> sr-te -> import-route-policy (mandatory if sr-te defined)
```