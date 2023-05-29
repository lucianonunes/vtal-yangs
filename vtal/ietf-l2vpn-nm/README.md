# IETF-L2NM

**This is a sample IETF-L2NM function pack**

This Function Package implements the following IETF model:
https://tools.ietf.org/html/draft-barguil-opsawg-l2sm-l2nm-02
https://github.com/IETF-OPSAWG-WG/l2nm/blob/master/Yang/ietf-l2vpn-ntw.yang
and uses L2VPN as a internal stacked service for service implementation.

## IETF-L2NM Yang Tree Implementation Coverage
```
module: ietf-l2vpn-ntw
  +--rw l2vpn-ntw
  |  +--rw id-pools!
  |  |  +--rw evi-id-pool-name?              -> /ralloc:resource-pools/idalloc:id-pool/name
  |  |  +--rw evi-source-target-pool-name?   -> /ralloc:resource-pools/idalloc:id-pool/name
  |  +--rw vpn-services
  |  |  +--rw vpn-service* [vpn-id]
  |  |  |  +--rw vpn-id                                       string
  |  |  |  +--ro evi-allocation-data
  |  |  |  |  +--ro evi-id?       string
  |  |  |  |  +--ro evi-source?   string
  |  |  |  |  +--ro evi-target?   string
  |  |  |  +--rw custom-template* [name]
  |  |  |  |  +--rw name         -> /ct-info:custom-template-info/template-name
  |  |  |  |  +--rw variable* [name]
  |  |  |  |  |  +--rw name     -> deref(../../name)/../ct-info:variables
  |  |  |  |  |  +--rw value    string
  |  |  |  |  +--rw iteration* [number]
  |  |  |  |     +--rw number      uint16
  |  |  |  |     +--rw variable* [name]
  |  |  |  |        +--rw name     -> deref(../../../name)/../ct-info:variables
  |  |  |  |        +--rw value    string
  |  |  |  +--rw (evi-id-choice)?
  |  |  |  |  +--:(auto-evi-id)
  |  |  |  |  |  +--rw auto-evi-id?                           empty
  |  |  |  |  +--:(evi-id)
  |  |  |  |     +--rw evi-id                                 uint16
  |  |  |  +--rw (evi-source-choice)?
  |  |  |  |  +--:(auto-evi-source)
  |  |  |  |  |  +--rw auto-evi-source?                       empty
  |  |  |  |  +--:(evi-source)
  |  |  |  |     +--rw evi-source                             uint32
  |  |  |  +--rw (evi-target-choice)?
  |  |  |  |  +--:(auto-evi-target)
  |  |  |  |  |  +--rw auto-evi-target?                       empty
  |  |  |  |  +--:(evi-target)
  |  |  |  |     +--rw evi-target                             uint32
  |  |  |  +---x self-test
  |  |  |  |  +--ro output
  |  |  |  |     +--ro status?    string
  |  |  |  |     +--ro message?   string
  |  |  |  +--rw control-word?                                enumeration
  |  |  |  +--rw vpn-svc-type?                                enumeration
  |  |  |  +--rw svc-topo?                                    identityref
  |  |  |  +--rw vpn-target
  |  |  |  |  +--rw hub-rt-value      rt-types:route-target
  |  |  |  |  +--rw spoke-rt-value    rt-types:route-target
  |  |  |  +--rw status
  |  |  |  |  +--ro oper-status
  |  |  |  |     +--ro status?      identityref
  |  |  |  |     +--ro timestamp?   yang:date-and-time
  |  |  |  +--rw vpn-nodes
  |  |  |  |  +--rw vpn-node* [vpn-node-id ne-id]
  |  |  |  |     +--rw vpn-node-id                                     -> /ncs:devices/lsa-utils:lsa/dispatch-map/device/name
  |  |  |  |     +--rw custom-template* [name]
  |  |  |  |     |  +--rw name         -> /ct-info:custom-template-info/template-name
  |  |  |  |     |  +--rw variable* [name]
  |  |  |  |     |  |  +--rw name     -> deref(../../name)/../ct-info:variables
  |  |  |  |     |  |  +--rw value    string
  |  |  |  |     |  +--rw iteration* [number]
  |  |  |  |     |     +--rw number      uint16
  |  |  |  |     |     +--rw variable* [name]
  |  |  |  |     |        +--rw name     -> deref(../../../name)/../ct-info:variables
  |  |  |  |     |        +--rw value    string
  |  |  |  |     +---x error-recovery
  |  |  |  |     |  +---w input
  |  |  |  |     |  |  +---w sync-direction    enumeration
  |  |  |  |     |  +--ro output
  |  |  |  |     |     +--ro success    boolean
  |  |  |  |     |     +--ro detail?    string
  |  |  |  |     +--rw multi-home!
  |  |  |  |     |  +--rw esi-value    string
  |  |  |  |     +--rw te-service-mapping
  |  |  |  |     |  +--rw te-mapping
  |  |  |  |     |  |  +--rw (te)?
  |  |  |  |     |  |     +--:(sr-policy)
  |  |  |  |     |  |     |  +--rw sr-policy!
  |  |  |  |     |  |     |     +--rw policy      string
  |  |  |  |     |  |     |     +--rw fallback?   enumeration
  |  |  |  |     |  |     +--:(te-tunnel-list)
  |  |  |  |     |  |     |  +--rw te-tunnel-list!
  |  |  |  |     |  |     |     +--rw (tunnel-te-id-source)
  |  |  |  |     |  |     |     |  +--:(te-tunnel-id)
  |  |  |  |     |  |     |     |  |  +--rw te-tunnel-id?      uint16
  |  |  |  |     |  |     |     |  +--:(ietf-te-service)
  |  |  |  |     |  |     |     |     +--rw ietf-te-service?   string
  |  |  |  |     |  |     |     +--rw fallback?                enumeration
  |  |  |  |     |  |     +--:(odn)
  |  |  |  |     |  |        +--rw odn!
  |  |  |  |     |  |           +--rw route-policy    -> /cisco-flat-L2vpn-fp:l2vpn-route-policy/name
  |  |  |  |     |  |           +--rw attach-point
  |  |  |  |     |  |              +--rw (parent-rr-route-policy-choice)?
  |  |  |  |     |  |                 +--:(parent-rr-route-policy)
  |  |  |  |     |  |                    +--rw parent-rr-route-policy?   string
  |  |  |  |     |  +--rw l2vpn-ntw-augmentations:srv6!
  |  |  |  |     |     +--rw l2vpn-ntw-augmentations:locator?   string
  |  |  |  |     +--rw node-role?                                      enumeration
  |  |  |  |     +--rw ne-id                                           -> ../vpn-node-id
  |  |  |  |     +--rw signaling-options* [type]
  |  |  |  |     |  +--rw type         enumeration
  |  |  |  |     |  +--rw evpn-bgp!
  |  |  |  |     |  |  +--rw type?                enumeration
  |  |  |  |     |  |  +--rw mac-learning-mode?   identityref
  |  |  |  |     |  |  +--rw vpn-target* [id]
  |  |  |  |     |  |  |  +--rw id                   int8
  |  |  |  |     |  |  |  +--rw route-targets* [route-target]
  |  |  |  |     |  |  |  |  +--rw route-target    rt-types:route-target
  |  |  |  |     |  |  |  +--rw route-target-type    rt-types:route-target-type
  |  |  |  |     |  |  +--rw vpn-policies
  |  |  |  |     |  |     +--rw import-policy?   -> ../../../../te-service-mapping/te-mapping/odn/route-policy
  |  |  |  |     |  |     +--rw export-policy?   -> ../../../../te-service-mapping/te-mapping/odn/route-policy
  |  |  |  |     |  +--rw t-ldp-pwe!
  |  |  |  |     |     +--rw ac-pw-list* [peer-addr vc-id]
  |  |  |  |     |        +--rw peer-addr     inet:ip-address
  |  |  |  |     |        +--rw vc-id         vpn-common:svc-id
  |  |  |  |     |        +--rw mpls-label?   uint32
  |  |  |  |     +--rw vpn-network-accesses
  |  |  |  |     |  +--rw vpn-network-access* [id]
  |  |  |  |     |     +--rw id                      vpn-common:svc-id
  |  |  |  |     |     +--rw Interface-mtu?          uint16
  |  |  |  |     |     +--rw connection
  |  |  |  |     |     |  +--rw encapsulation-type?   enumeration
  |  |  |  |     |     |  +--rw dot1q-interface!
  |  |  |  |     |     |  |  +--rw l2-access-type?   enumeration
  |  |  |  |     |     |  |  +--rw dot1q! {dot1q}?
  |  |  |  |     |     |  |     +--rw physical-inf?   string
  |  |  |  |     |     |  |     +--rw c-vlan-id?      uint32
  |  |  |  |     |     |  |     +--rw rewrite!
  |  |  |  |     |     |  |        +--rw ingress!
  |  |  |  |     |     |  |           +--rw (tag-choice)?
  |  |  |  |     |     |  |           |  +--:(pop)
  |  |  |  |     |     |  |           |  |  +--rw pop          enumeration
  |  |  |  |     |     |  |           |  +--:(push)
  |  |  |  |     |     |  |           |  |  +--rw push         empty
  |  |  |  |     |     |  |           |  +--:(translate)
  |  |  |  |     |     |  |           |     +--rw translate    enumeration
  |  |  |  |     |     |  |           +--rw dot1q              uint16
  |  |  |  |     |     |  |           +--rw mode?              enumeration
  |  |  |  |     |     |  +--rw untagged-interface!
  |  |  |  |     |     |  |  +--rw l2-access-type?   enumeration
  |  |  |  |     |     |  |  +--rw untagged!
  |  |  |  |     |     |  |     +--rw physical-inf?   string
  |  |  |  |     |     |  |     +--rw sub-if-id?      uint32
  |  |  |  |     |     |  |     +--rw rewrite!
  |  |  |  |     |     |  |        +--rw ingress!
  |  |  |  |     |     |  |           +--rw (tag-choice)?
  |  |  |  |     |     |  |           |  +--:(pop)
  |  |  |  |     |     |  |           |  |  +--rw pop          enumeration
  |  |  |  |     |     |  |           |  +--:(push)
  |  |  |  |     |     |  |           |  |  +--rw push         empty
  |  |  |  |     |     |  |           |  +--:(translate)
  |  |  |  |     |     |  |           |     +--rw translate    enumeration
  |  |  |  |     |     |  |           +--rw dot1q              uint16
  |  |  |  |     |     |  |           +--rw mode?              enumeration
  |  |  |  |     |     |  +--rw split-horizon!
  |  |  |  |     |     +--rw ethernet-service-oam
  |  |  |  |     |        +--rw md-name?    string
  |  |  |  |     |        +--rw md-level?   uint8
  |  |  |  |     |        +--rw y-1731* [maid]
  |  |  |  |     |           +--rw maid                                               string
  |  |  |  |     |           +--rw mep-id                                             uint16
  |  |  |  |     |           +--rw message-period?                                    string
  |  |  |  |     |           +--rw l2vpn-ntw-augmentations:id-type?                   enumeration
  |  |  |  |     |           +--ro l2vpn-ntw-augmentations:sman-id-allocation-data
  |  |  |  |     |           |  +--ro l2vpn-ntw-augmentations:icc-based-id?   string
  |  |  |  |     |           |  +--ro l2vpn-ntw-augmentations:number-id?      string
  |  |  |  |     |           +--rw l2vpn-ntw-augmentations:y-1731-profile* [name]
  |  |  |  |     |              +--rw l2vpn-ntw-augmentations:schedule
  |  |  |  |     |              |  +--rw l2vpn-ntw-augmentations:interval?   uint8
  |  |  |  |     |              |  +--rw l2vpn-ntw-augmentations:duration?   union
  |  |  |  |     |              +--rw l2vpn-ntw-augmentations:name?       -> /l2vpn-ntw/l2vpn-ntw-augmentations:y-1731-profile/name
  |  |  |  |     +--rw l2vpn-ntw-augmentations:control-word-disable?   boolean
  |  |  |  +--rw l2vpn-ntw-augmentations:service-assurance!
  |  |  |  |  +--rw l2vpn-ntw-augmentations:monitoring-state?   aa-monitoring-state
  |  |  |  |  +--rw l2vpn-ntw-augmentations:profile-name        string
  |  |  |  |  +--rw l2vpn-ntw-augmentations:rule-name           string
  |  |  |  +--rw l2vpn-ntw-augmentations:bridge-group?        string
  +--rw l2nm-actions
     +---x cleanup
     |  +---w input
     |  |  +---w service          string
     |  |  +---w no-networking    boolean
     |  |  +---w vpn-node?        -> /ncs:devices/lsa-utils:lsa/dispatch-map/device/name
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
        |  +---w service           string
        |  +---w vpn-node?         string
        |  +---w sync-direction    enumeration
        +--ro output
           +--ro success    boolean
           +--ro detail?    string
```

**NSO Modifications in IETF-L2NM Yang Model**

Modifications in IETF-L2NM model to allow compatibility with L2VPN

  *vpn-node-id* : Type change
        OLD : type vpn-common:svc-id
        NEW : type leafref /core-fp-common:dispatch-map/core-fp-common:device

  *ne-id* : Type change
        OLD : type string
        NEW : type leafref ../l2vpn-ntw:vpn-node-id

  *vpn-svc-type* : Type change
        OLD : type identityref { base vpn-common:vpn-signaling-type }
        NEW : type enumeration { enum vpn-common:t-ldp; enum vpn-common:evpn-bgp; }

  *vpn-node* : added min-elements 2 and max-elements 2

  *signaling-options* : added max-elements 1

  *ac-pw-list* : added max-elements 1

  *signaling-options/type* : Type change
        OLD : type vpn-common:vpn-signaling-type
        NEW : type enumeration { enum vpn-common:t-ldp; enum vpn-common:evpn-bgp; }

  *signaling-options/evpn-bgp/type* : Type change
        OLD : type identityref { base evpn-type; } 
        NEW : type enumeration { enum evpn-vpws; }

  *vpn-service* : Type change
        OLD : type vpn-common:svc-id
        NEW : type enumeration { enum vpn-common:t-ldp; enum vpn-common:evpn-bgp; }

  *vpn-network-access* : added max-elements 1

  *encapsulation-type* : Type change
        OLD : type vpn-common:encapsulation-type
        NEW : type enumeration { enum vpn-common:dot1q; enum vpn-common:untagged-int; }

  *l2-access-type* : Type change
        OLD : type identityref { base vpn-common:encapsulation-type; }
        NEW : type enumeration { enum vpn-common:untagged-int; }

  *Interface-mtu* : Type change
        OLD : type uint32;
        NEW : type uint16 { range "64..65535"; }

Augmented te-service-mapping/te-mapping/te choice to IETF-L2NM model
  vpn-svc-type = t-ldp supports
        choice te : sr-policy and te-tunnel-list
  vpn-svc-type = evpn-bgp supports
        choice te : sr-policy

Augmented leaf mpls-label to IETF-L2NM model
  leaf mpls-label under /l2vpn-ntw/vpn-services/vpn-service/vpn-nodes/vpn-node/signaling-options/t-ldp-pwe/ac-pw-list

Augmented leaf control-word to IETF-L2NM model
  leaf control-word under /l2vpn-ntw/vpn-services/vpn-service/
  when vpn-svc-type = 'vpn-common:t-ldp'

Augmented container multi-home to IETF-L2NM model
  container multi-home under /l2vpn-ntw/vpn-services/vpn-service/vpn-nodes/vpn-node
  when vpn-svc-type = 'vpn-common:evpn-bgp'

Augmented container rewrite to IETF-L2NM model
  container rewrite under /l2vpn-ntw/vpn-services/vpn-service/vpn-nodes/vpn-node/vpn-network-accesses/vpn-network-access/connection/dot1q-interface/dot1q

Augmented container evi-allocation-data to IETF-L2NM model
  container evi-allocation-data under /l2vpn-ntw/vpn-services/vpn-service
  This is operational data and not config data i.e. container set with "config false;" in the yang

Modifications in dependant models

ietf-vpn-common.yang
  *rd* : Type change

    OLD : type union (empty, rt-types:route-distinguisher)
    NEW : type leaf (rt-types:route-distinguisher)
    Notes : COMPILE ERROR: NSO does not support YANG 1.1 type union with empty leaves

ietf-routing-types@2017-02-19.yang
  *typedef router-id* : Commented
  Notes : COMPILE ERROR: typedef 'dotted-quad' in module 'ietf-yang-types' not found

## Configuration Examples:

**Sample L2NM Service vpn-svc-type = t-ldp**
```
NSO Service Input:
=================
vpn-services {
    vpn-service l2nm-p2p {
        custom-template CT-CLI-banner {
            variable BANNER_TEXT {
                value Welcome_A;
            }
        }
        vpn-svc-type vpn-common:t-ldp;
        vpn-nodes {
            vpn-node PIOSXR-0 PIOSXR-0 {
                custom-template CT-CLI-banner {
                    variable BANNER_TEXT {
                        value Welcome_B;
                    }
                }
                signaling-options vpn-common:t-ldp {
                    t-ldp-pwe {
                        ac-pw-list 198.18.1.5 1001 {
                            mpls-label 101;
                        }
                    }
                }
                vpn-network-accesses {
                    vpn-network-access l2vpn-p2p-ac1 {
                        connection {
                            encapsulation-type vpn-common:dot1q;
                            dot1q-interface {
                                l2-access-type vpn-common:dot1q;
                                dot1q {
                                    physical-inf GigabitEthernet0/0/0/1;
                                    c-vlan-id    601;
                                }
                            }
                        }
                    }
                }
            }
            vpn-node PIOSXR-1 PIOSXR-1 {
                signaling-options vpn-common:t-ldp {
                    t-ldp-pwe {
                        ac-pw-list 198.18.1.4 1001 {
                            mpls-label 102;
                        }
                    }
                }
                vpn-network-accesses {
                    vpn-network-access l2vpn-p2p-ac1 {
                        connection {
                            encapsulation-type vpn-common:dot1q;
                            dot1q-interface {
                                l2-access-type vpn-common:dot1q;
                                dot1q {
                                    physical-inf GigabitEthernet0/0/0/1;
                                    c-vlan-id    601;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

NSO Device Output (PIOSXR-0):
============================
banner {
    incoming {
        start-marker ';
        message      Welcome_B;
        end-marker   ';
    }
    motd {
        start-marker ';
        message      Welcome_B;
        end-marker   ';
    }
    login {
        start-marker ';
        message      Welcome_B;
        end-marker   ';
    }
}
logging {
    history informational;
}
interface {
    GigabitEthernet 0/0/0/1 {
        description "T-SDN Interface";
    }
    GigabitEthernet-subinterface {
        GigabitEthernet 0/0/0/1.601 {
            mode        l2transport;
            description "T-SDN Interface";
            encapsulation {
                dot1q {
                    vlan-id [ 601 ];
                }
            }
        }
    }
}
l2vpn {
    pw-class pw-class-l2vpn-p2p-ac1 {
        encapsulation {
            mpls;
        }
    }
    xconnect {
        group l2vpn-evpn-ac1 {
            p2p l2vpn-evpn-ac1 {
                interface GigabitEthernet0/0/0/1.601;
                neighbor-evpn {
                    neighbor {
                        evpn evi 1 target 2 source 1;
                    }
                }
            }
        }
        group l2vpn-p2p-ac1 {
            p2p l2vpn-p2p-ac1 {
                interface GigabitEthernet0/0/0/1.601;
                neighbor 198.18.1.4 pw-id 1001 {
                    ip-version ipv4;
                    mpls {
                        static {
                            label {
                                local  101;
                                remote 102;
                            }
                        }
                    }
                    pw-class   pw-class-l2vpn-p2p-ac1;
                }
            }
        }
    }
}

NSO Device Output (PIOSXR-1):
============================
banner {
    incoming {
        start-marker ';
        message      Welcome_A;
        end-marker   ';
    }
    motd {
        start-marker ';
        message      Welcome_A;
        end-marker   ';
    }
    login {
        start-marker ';
        message      Welcome_A;
        end-marker   ';
    }
}
logging {
    history informational;
}
interface {
    GigabitEthernet 0/0/0/1 {
        description "T-SDN Interface";
    }
    GigabitEthernet-subinterface {
        GigabitEthernet 0/0/0/1.601 {
            mode        l2transport;
            description "T-SDN Interface";
            encapsulation {
                dot1q {
                    vlan-id [ 601 ];
                }
            }
        }
    }
}
l2vpn {
    pw-class pw-class-l2vpn-p2p-ac1 {
        encapsulation {
            mpls;
        }
    }
    xconnect {
        group l2vpn-evpn-ac1 {
            p2p l2vpn-evpn-ac1 {
                interface GigabitEthernet0/0/0/1.601;
                neighbor-evpn {
                    neighbor {
                        evpn evi 1 target 1 source 2;
                    }
                }
            }
        }
        group l2vpn-p2p-ac1 {
            p2p l2vpn-p2p-ac1 {
                interface GigabitEthernet0/0/0/1.601;
                neighbor 198.18.1.5 pw-id 1001 {
                    ip-version ipv4;
                    mpls {
                        static {
                            label {
                                local  102;
                                remote 101;
                            }
                        }
                    }
                    pw-class   pw-class-l2vpn-p2p-ac1;
                }
            }
        }
    }
}
```
**Sample L2NM Service vpn-svc-type = evpn-bgp**
Pre-requisite to create evpn-bgp type of service is create id-pools and associate them with l2vpn-ntw id-pools. These id-pools will be used for evi-id, evi-source and evi-target configs.

```
NSO resource-manager id-pools:
=============================
resource-pools {
    id-pool evi-id-pool {
        range {
            start 1;
            end   4000;
        }
    }
    id-pool evi-source-target-pool {
        range {
            start 1;
            end   400;
        }
    }
}

NSO resource-pools:
==================
l2vpn-ntw {
    id-pools {
        evi-id-pool-name            evi-id-pool;
        evi-source-target-pool-name evi-source-target-pool;
    }
}
```

```
NSO Service Input:
=================
l2vpn-ntw {
    vpn-services {
        vpn-service l2nm-evpn {
            vpn-svc-type vpn-common:evpn-bgp;
            vpn-nodes {
                vpn-node PIOSXR-0 PIOSXR-0 {
                    signaling-options vpn-common:evpn-bgp {
                        evpn-bgp {
                            type evpn-vpws;
                        }
                    }
                    vpn-network-accesses {
                        vpn-network-access l2vpn-evpn-ac1 {
                            connection {
                                encapsulation-type vpn-common:dot1q;
                                dot1q-interface {
                                    l2-access-type vpn-common:dot1q;
                                    dot1q {
                                        physical-inf GigabitEthernet0/0/0/1;
                                        c-vlan-id    601;
                                    }
                                }
                            }
                        }
                    }
                }
                vpn-node PIOSXR-1 PIOSXR-1 {
                    signaling-options vpn-common:evpn-bgp {
                        evpn-bgp {
                            type evpn-vpws;
                        }
                    }
                    vpn-network-accesses {
                        vpn-network-access l2vpn-evpn-ac1 {
                            connection {
                                encapsulation-type vpn-common:dot1q;
                                dot1q-interface {
                                    l2-access-type vpn-common:dot1q;
                                    dot1q {
                                        physical-inf GigabitEthernet0/0/0/1;
                                        c-vlan-id    601;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

NSO Device Output (PIOSXR-0):
============================
interface {
    GigabitEthernet 0/0/0/1 {
        description "T-SDN Interface";
    }
    GigabitEthernet-subinterface {
        GigabitEthernet 0/0/0/1.601 {
            mode        l2transport;
            description "T-SDN Interface";
            encapsulation {
                dot1q {
                    vlan-id [ 601 ];
                }
            }
        }
    }
}
l2vpn {
    xconnect {
        group l2vpn-evpn-ac1 {
            p2p l2vpn-evpn-ac1 {
                interface GigabitEthernet0/0/0/1.601;
                neighbor-evpn {
                    neighbor {
                        evpn evi 1 target 2 source 1;
                    }
                }
            }
        }
    }
}

NSO Device Output (PIOSXR-1):
============================
interface {
    GigabitEthernet 0/0/0/1 {
        description "T-SDN Interface";
    }
    GigabitEthernet-subinterface {
        GigabitEthernet 0/0/0/1.601 {
            mode        l2transport;
            description "T-SDN Interface";
            encapsulation {
                dot1q {
                    vlan-id [ 601 ];
                }
            }
        }
    }
}
l2vpn {
    xconnect {
        group l2vpn-evpn-ac1 {
            p2p l2vpn-evpn-ac1 {
                interface GigabitEthernet0/0/0/1.601;
                neighbor-evpn {
                    neighbor {
                        evpn evi 1 target 1 source 2;
                    }
                }
            }
        }
    }
}
```
