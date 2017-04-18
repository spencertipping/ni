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
5. Classes can change while an instance is running.
6. The mechanism used to provision a ni instance is as secure as the remote
   it's running on (e.g. SSH). This assumption is required in order to use
   PSKs, and it's a reasonable thing to assume.
