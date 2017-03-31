# Network-level design
ni instances serve as bridges on a vlan-aware packet-switched OSI L3 network
whose packets can be forwarded over TCP or UDP connections (in either case
supporting packet loss for saturated egress links). Instance addresses are
global, so there is no IP/MAC address distinction; because of this and other
differences, ni's networks don't use IPv4 or TCP, although they do have
analogous abstractions.

## Point-to-point links (L2)
ni supports a number of point-to-point connection strategies:

1. The SSH tunnel used to start an instance (secure vlan)
2. UDP connections (insecure vlan)
3. Arbitrary ni streams, e.g. reading a file (insecure vlan)

## Routed packet traffic (L3)

## Streams (L4)
