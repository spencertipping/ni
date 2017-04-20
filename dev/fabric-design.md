# ni fabric
This is the base layer, which manages stuff like self-installing, forwarding
data and RMI, and managing running instances. Internally, ni operates a
switched-packet network in which individual instances can forward traffic
across varying connections; RMI requests and replies are encrypted using the
recipient's pre-shared key.

## Design assumptions
1. Host network connections are unreliable and insecure.
2. The network topology must be able to change without rebooting ni instances.
3. Remote ni instances may be arbitrarily compromised at any point, which means
   all instances are aware of trust domains.
4. Connection unreliability should be invisible to running workflows (i.e. it
   should look like a slow/paused link, not generate SIGPIPE).
5. Classes can change while a ni instance is running.
6. The mechanism used to provision a ni instance is as secure as the remote
   it's running on (e.g. SSH). This assumption is required in order to use
   PSKs, and it's a reasonable design requirement.

Q: Is it worth using the fabric as a substrate to run jobs, but leave the jobs
   themselves generalized? So fabric is strictly a connectivity layer.

Handoff is "fabric node" that is itself a member of the network; it manages
state, sends/receives messages. Some messages are handled as RMI. Probably
atomic messages + IO, fabric nodes encapsulate all required retry logic.

Q: How to handle mixed-security connections? (secure provisioning connection
   like SSH, insecure data stuff). We need end-to-end encryption anyway.

Two security tiers:

1. "You know my VLAN key, so you can ask me to forward traffic for you"
   (link-level)
2. "You know my RMI key, so you can interact with objects I own"
   (connection-level)

Two base networking levels: link-layer (L2) and network-layer (L3). Connections
managed by the fabric driver. No source routing.
