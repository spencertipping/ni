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

### Binary formats
There are two data formats, one in plaintext and one that encrypts its traffic.
Every L2 link is set up to transmit only one of these two packet types,
depending on whether it's a secure link (SSH) or an insecure one (TCP or UDP).

#### Plaintext format
This format assumes that data will be transmitted securely and without errors.

```
frame_size: 16 bits, BE         # size of the whole frame in bytes
data:       <variable>
```

#### Encrypted format
This format hashes the data to detect errors. Any hash mismatch causes the
incoming packet to be silently dropped.

```
frame_size: 16 bits, BE         # size of the whole frame in bytes
vlan_id:    16 bits, BE         # index of vlan decryption key to use
iv:        128 bits
encrypted: <variable>           # aes256(data + sha256(data), iv, keys[vlan_id])
```

#### Format of the `data` field
```
packet_type: 8 bits
packet_data: <variable>
```

`packet_type` is one of the following:

- `0x01`: Link test ping (not data)
- `0x02`: Link test pong (not data)
- `0x03`: L3 packet

#### Link test packets
These are important, and ni uses them to monitor link reliability + latency.
During an active connection, ni will intermittently ping to make sure the link
hasn't silently failed. ni also uses test packets to detect cases where packets
are being fragmented, which causes their data to be destroyed.

A link test ping will contain an arbitrary amount of data that must be echoed
back using a link test pong. For example, here's a ping/pong cycle between two
ends of a plaintext L2 link, in this case using the single-byte test data `ff`:

```
A -> B: 00 04 01 ff
B -> A: 00 04 02 ff
```

The nodes can track timing themselves, or encode that data into the ping
request/reply.

## Routed data (L3)
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
forwarding value/priority by being forwarded excessively (not that this should
happen in the first place, but pathological routing situations might arise due
to race conditions while a graph update is being propagated).

### Binary format
```
source_address: 128 bits
dest_address:   128 bits
win:             32 bits, single float  # w
df_dt:           32 bits, single float  # f/s
dcost:           32 bits, single float  # f incurred by dropping the packet
data:         <variable>
```

### Intent and drop cost
Drop cost is measured in the amount of fail incurred if the packet has to be
retransmitted to its current location. A node is performing optimally if it
minimizes the total drop cost of the packets it drops.

See [intent-design.md](intent-design.md) for details.

## Reliable finite-message transport (L4)
ni communicates primarily using _messages_, not _streams_. This means that
TCP/IP is overkill; what we really need is a protocol that provides TCP's
durability for finite-length messages. The SYN/ACK handshake is unnecessary.

L4 messages are one-way, but they can refer to each other. There are three
types of packets:

- `piece(message_id, total_pieces, piece_id, data)`: send part of a message
- `ack(message_id, received_pieces_bits)`: indicate which parts of a message
  have been received
- `check(message_id)`: request an ack

The sender exists in one of two states:

1. Unconfirmed: the message needs to be stored so we can retransmit, and we're
   listening for `ack`s for the message.
2. Confirmed: the message can be deallocated, and any `ack` for the message can
   be ignored.

The receiver has three states with respect to a given `message_id`:

1. Ok to receive `message_id`: the message is not a retransmitted duplicate of
   something we've seen before.
2. Currently receiving `message_id`: send `ack` when (1) we haven't heard
   anything in a while, or (2) we've fully received `message_id`.
3. We're done receiving `message_id`: reply with a complete `ack` for all
   further packets for it.

### Message IDs
Message IDs are namespaced to receivers, which means they must be constructed
in a specific way to avoid collisions:

```
message_id = 96 bits of md5(sender instance ID + message data)
           + 32 bits of UNIX timestamp
```

**TODO:** clock discrepancies
