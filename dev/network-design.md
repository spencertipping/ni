# Network-level design
ni instances serve as bridges on a vlan-aware packet-switched OSI L3 network
whose packets can be forwarded over TCP or UDP connections (in either case
supporting packet loss for saturated egress links). Instance addresses are
global, so there is no IP/MAC address distinction; because of this and other
differences, ni's networks don't use IPv4 or TCP, although they do have some
analogous abstractions.

ni networks and Ethernet also differ in how they are set up: Ethernet uses
things like STP to build an understanding of the hardware topology, whereas ni
networks work in the other direction: they start with a configuration and
provision instances and linkages to converge to that state. So ni doesn't have
any particular obligation to discover existing linkages, beyond verifying that
they work at all.

## Point-to-point links (L2)
ni transmits level-2 frames over three types of channels:

1. SSH (used to create the instance in the first place)
2. Ad-hoc UDP (used for peer-to-peer communication)
3. Random async IO (arbitrary ni streams; this can be slow)

### VLANs, PSKs, and security
ni instances are booted over a protocol that's assumed to be secure, e.g. SSH.
This is typically a safe assumption: you wouldn't generally have a computing
environment where unauthenticated users could provision process resources, nor
one where unauthorized users could modify the traffic used to do so.

This assumption means that each newly-provisioned ni instance can be seeded
with one or more pre-shared keys. Public key encryption isn't a good solution
for unsessioned datagrams, so instead we use VLANs as symmetric encryption
domains: each vlan uses a separate symmetric encryption key, and ni instances'
vlan memberships are determined by the set of PSKs they're seeded with (or
later gain by configuration).

Packets are hashed + encrypted at the frame level:

```
[frame size] + [vlan ID] + [IV]
  + vlan-psk-encrypted([L3 packet data] + [SHA-256 of L3 packet data])
```

L2 security is provided by a JIT-compiled C binary that receives symmetric keys
using a private block of shared memory, or some initial FD transfers. We don't
want to bake the keys in (1) for security, and (2) because there's no guarantee
that any member of the vlan will have a C compiler -- so we might have to
compile elsewhere and transfer the binary.

## Routed packet traffic (L3)
L3 packets are designed a lot like IPv6, but they have different metadata
associated with them. We need to store the following header data:

- Source address
- Destination address
- Packet type (like the "protocol" field of IP)
- Accumulated expected global cost (drop cost)
- Hop count
- TTL (don't forward if blocked for longer than this amount of time)

### Routing
Each node chooses how to get incoming packets closer to their destination, and
in general nodes shouldn't assume much about any high-level routing strategy.
Nodes are theoretically at liberty to use stochastic or locally-optimal
algorithms, or to forward packets randomly. In practice nodes will minimize
expected global cost, which is informed by higher-level graph metadata.

Hop count and TTL prevent packets from accumulating arbitrarily high amounts of
forwarding value/priority by being forwarded excessively.

## Data and control messages (L4)
