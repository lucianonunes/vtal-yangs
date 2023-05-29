# IETF-TE 

**This is a sample IETF-TE RSVP TE function pack** 

This Function Package implements the https://tools.ietf.org/id/draft-ietf-teas-yang-te-21.txt 

and depends on cisco-rsvp-te-fp function pack

## IETF-Yang Tree Implementation coverage
```
module: ietf-te
  +--rw te!
     +--rw tunnels
     |  +--rw tunnel* [name]
     |  |  +--rw name                        string
     |  |  +--rw identifier?                 uint16
     |  |  +--rw description?                string
     |  |  +--rw signaling-type?             identityref
     |  |  +--rw setup-priority?                  uint8
     |  |  +--rw hold-priority?                   uint8
     |  |  +--rw source                      te-types:te-node-id
     |  |  +--rw head-end                    -> /core-fp-common:dispatch-map/device
     |  |  +--rw destination                 te-types:te-node-id
     |  |  +--rw tail-end?                   -> /core-fp-common:dispatch-map/device
     |  |  +--rw bidirectional?              boolean
     |  |  +--rw te-bandwidth
     |  |  |  +--rw (technology)?
     |  |  |     +--:(generic)
     |  |  |        +--rw generic?   te-bandwidth
     |  |  +--rw p2p-primary-paths
     |  |  |  +--rw p2p-primary-path* [name]
     |  |  |     +--rw name                             string
     |  |  |     +--rw path-computation-method?         identityref
     |  |  |     +--rw preference?                      uint8
     |  |  |     +--rw explicit-route-objects-always
     |  |  |     |  +--rw route-object-include-exclude* [index]
     |  |  |     |     +--rw explicit-route-usage?        identityref
     |  |  |     |     +--rw index                        uint32
     |  |  |     |     +--rw (type)?
     |  |  |     |        +--:(numbered-node-hop)
     |  |  |     |        |  +--rw numbered-node-hop
     |  |  |     |        |     +--rw node-id     te-node-id
     |  |  |     |        |     +--rw hop-type?   te-hop-type
     |  |  |     |        +--:(label)
     |  |  |     |        |  +--rw label-hop
     |  |  |     |        |     +--rw te-label
     |  |  |     |        |        +--rw (technology)?
     |  |  |     |        |        |  +--:(generic)
     |  |  |     |        |        |     +--rw generic?   rt-types:generalized-label
     |  |  |     +--rw optimizations
     |  |  |     |  +--rw (algorithm)?
     |  |  |     |     +--:(metric) {path-optimization-metric}?
     |  |  |     |     |  +--rw optimization-metric* [metric-type]
     |  |  |     |     |  |  +--rw metric-type
     |  |  |     |     |  |  |       identityref
     |  |  +--rw traffic-steering
     |  |     +--rw (steering-choice)?
     |  |        +--:(autoroute)
     |  |        |  +--rw autoroute
     |  |        |     +--rw announce
     |  |        |     |  +--rw enable?   boolean
     |  |        |     |  +--rw metric!
     |  |        |     |     +--rw (metric-choice)?
     |  |        |     |        +--:(constant)
     |  |        |     |        |  +--rw constant?   uint32
     |  |        |     |        +--:(absolute)
     |  |        |     |        |  +--rw absolute?   uint32
     |  |        |     |        +--:(relative)
     |  |        |     |           +--rw relative?   int8
     |  |        |     +--rw destination* [address]
     |  |        |        +--rw address    inet:ipv4-address
     |  |        +--:(forwarding-adjacency)
     |  |           +--rw forwarding-adjacency!
     |  |              +--rw holdtime?       uint16
     |  |              +--rw include-ipv6?   empty

```

**Modification in IETF-TE Yang model**

There are the following modifications in the IETF-TE ietf-te model

  *name*: Tunnel name 
  ```
    Change type from string to string with pattern since this is the key of service
  ```

  *head-end*, *tail-end*: Tunnel source and destination devices
  ```
    Augmented head-end and tail-end leaf elements for devices to be used as source and destination.
  ```

  *traffic-steering*: Steer traffic traffic through the tunnel
  ```
    Augmented traffic-steering grouping that provides the option to configure autoroute announce (default) or forwarding adjacency.
  ```

## Configuration Examples :

**Explicit Path With Next Hop Include and Exclude**
```
NSO Service Input:
=================
tunnel IETF-RSVP-TE {
    identifier     1234;
    description    RSVP_TE;
    source         111.1.1.1;
    head-end       PIOSXR-0;
    destination    222.2.2.2;
    te-bandwidth {
        generic 94967295;
    }
    signaling-type path-setup-rsvp;
    p2p-primary-paths {
        p2p-primary-path PATH-1 {
            path-computation-method path-explicitly-defined;
            preference              1;
            explicit-route-objects-always {
                route-object-include-exclude 1 {
                    numbered-node-hop {
                        node-id  1.1.1.1;
                        hop-type loose;
                    }
                }
                route-object-include-exclude 3 {
                    explicit-route-usage route-exclude-object;
                    numbered-node-hop {
                        node-id 10.10.10.10;
                    }
                }
            }
        }
    }
}

NSO Device Output:
=================
explicit-path name IETF-RSVP-TE-PATH-1
 index 1 next-address loose ipv4 unicast 1.1.1.1
 index 3 exclude-address ipv4 unicast 10.10.10.10
!
interface tunnel-te 1234
 description "RSVP_TE"
 ipv4 unnumbered Loopback0
 no shutdown
 signalled-name IETF-RSVP-TE
 signalled-bandwidth 94967295
 autoroute announce
 exit
 destination 222.2.2.2
 path-option 1 explicit name IETF-RSVP-TE-PATH-1
exit
```

**Explicit Path With Label Hop**
```
NSO Service Input:
=================
tunnel IETF-RSVP-TE {
    identifier     1234;
    description    RSVP_TE;
    source         111.1.1.1;
    head-end       PIOSXR-0;
    destination    222.2.2.2;
    te-bandwidth {
        generic 94967295;
    }
    signaling-type path-setup-rsvp;
    p2p-primary-paths {
        p2p-primary-path PATH-1 {
            path-computation-method path-explicitly-defined;
            preference              1;
            explicit-route-objects-always {
                route-object-include-exclude 2 {
                    label-hop {
                        te-label {
                            generic Afw=;
                        }
                    }
                }
            }
        }
    }
}

NSO Device Output:
=================
explicit-path name IETF-RSVP-TE-PATH-1
 index 2 next-label 508
!
interface tunnel-te 1234
 description "RSVP_TE"
 ipv4 unnumbered Loopback0
 no shutdown
 signalled-name IETF-RSVP-TE
 signalled-bandwidth 94967295
 autoroute announce
 exit
 destination 222.2.2.2
 path-selection
  metric te
 exit
 path-option 1 explicit name IETF-RSVP-TE-PATH-1
exit
```

**Dynamic Path With Locally Computed and Metric TE**
```
NSO Service Input:
=================
tunnel IETF-RSVP-TE {
    identifier     1234;
    description    RSVP_TE;
    source         111.1.1.1;
    head-end       PIOSXR-0;
    destination    222.2.2.2;
    te-bandwidth {
        generic 94967295;
    }
    signaling-type path-setup-rsvp;
    p2p-primary-paths {
        p2p-primary-path PATH-2 {
            path-computation-method path-locally-computed;
            optimizations {
                optimization-metric path-metric-te;
            }
            preference              2;
        }
    }
}

NSO Device Output:
=================
interface tunnel-te 1234
 description "RSVP_TE"
 ipv4 unnumbered Loopback0
 no shutdown
 signalled-name IETF-RSVP-TE
 signalled-bandwidth 94967295
 autoroute announce
 exit
 destination 222.2.2.2
 path-selection
  metric te
 exit
 path-option 2 dynamic
exit
```

**Dynamic Path With External Query and Metric TE**
```
NSO Service Input:
=================
tunnel IETF-RSVP-TE {
    identifier     1234;
    description    RSVP_TE;
    source         111.1.1.1;
    head-end       PIOSXR-0;
    destination    222.2.2.2;
    te-bandwidth {
        generic 94967295;
    }
    signaling-type path-setup-rsvp;
    p2p-primary-paths {
        p2p-primary-path PATH-3 {
            path-computation-method path-externally-queried;
            optimizations {
                optimization-metric path-metric-te;
            }
            preference              3;
        }
    }
}

NSO Device Output:
=================
interface tunnel-te 1234
 description "RSVP_TE"
 ipv4 unnumbered Loopback0
 no shutdown
 signalled-name IETF-RSVP-TE
 signalled-bandwidth 94967295
 autoroute announce
 exit
 destination 222.2.2.2
 path-selection
  metric te
 exit
 path-option 3 dynamic pce
 pce
  delegation
exit
```

## Implementation limitations
* tunnel -> identifier is Mandatory
* tunnel -> source is Mandatory
* tunnel -> destination is Mandatory
* tunnel -> head-end is Mandatory
* tunnel -> tail-end is Mandatory if tunnel is bidirectional
* tunnel -> signaling-type of "path-setup-rsvp" is Mandatory 
 
