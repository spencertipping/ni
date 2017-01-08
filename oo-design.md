# OO refactor design
1. Metaclasses+documenting origin.
2. Objects can be referenced by URL relative to referrer, including across
   remote connections. This means bidirectional stdio-RMI (security?), of which
   we currently have half.
3. ni can examine environment for dependencies, run tests. No more offline
   test suite.
4. Parse states are mutable objects with open-ended continuations.
5. Images are objects; replace `lib` with `init.pl` or similar for libraries,
   import from FS verbatim.
6. RMI protocols are objects, required for HDFS indirection to manage hadoop
   runners.
7. Session recovery? Ultimately yes, but we can add it later. This amounts to
   an implementation change in the SSH RMI protocol.
8. Let's go ahead and completely redo ni's internal data representation to
   parse more efficiently.

## URIs for objects
```pl
$r = u"http://foo.com";                         # stream resource
$r = u"data:,foo";                              # literal string "foo"
$r = u"file://./foo";                           # local file
$r = u"hdfs:///foo/bar/bif";                    # HDFS file
$r = u"./foo";                                  # file shorthand
$r = u"ni.self:/core/pl/init.pl";               # ni state resource
$r = u"ni.ls:/core";                            # list resource names
$r = u"ni.pid:31891";                           # process resource
$r = u"ni.op:[\"n\",10]";                       # stream operation
$r = u"ni.rmi.ssh://user@host:port/ni.pid:19241";

print "$r\n";           # named
print while <$r>;       # readable
print $r->explain;      # human-readable description
```

Different URI schemes (classes) are at liberty to define different methods,
which means delegators like `ssh` must respond to potentially every method
defined on any class.

## Scheme modifiers
These correspond to usage patterns; e.g. `hdfs+rm:` or `file+rm:` when you want
to automatically delete a resource after it goes out of scope. The `+rm` here
is a generic modifier that applies to any removable resource and is implemented
by metaclasses and broadcasting.

- `+async`: modify all methods to return futures of results and not block
- `+rm`: autonuke when GC'd
- `+tmp`: construct the path as a tempfile, e.g. `file+tmp:foo`
  - `file+tmp+rm` and `file+rm+tmp` end up doing the same thing because `+tmp`
    removes itself from the URI scheme when constructed.

## RMI object URIs
```pl
my $obj         = u"...";
my $pipeline    = u"...";
my $hadoop_side = u"ni.rmi.fs-multi+async", 'hdfs:///path', $obj;
my $host_side   = u"ni.rmi.fs", 'hdfs:///path', $pipeline;
$host_side->enable_monitoring($monitor_uri);
print "$host_side\n";   # -> ni.rmi.fs:["hdfs:///path","<pipeline-url>"]
```

## Monitors
```pl
my $mon = u"ni.wmonitor:fd:1";          # new write monitor around FD 1
print $mon "foo";                       # write data to it
my @xs = $mon->sample;                  # some of the rows
my $bytes_s = $mon->throughput;         # bytes/sec throughput
```

## Documentation/tests
```pl
u"ni.scheme:ni.rmi"->child('ssh',
  name        => 'SSH RMI forwarder',
  synopsis    => q{ u"ni.rmi.ssh://[user@]host[:port]/remote resource URI"
                  | u"ni.rmi.ssh", $authority, $remote_resource },
  description => q{
    Sends ni to the remote machine, creates an instance, and connects to it.
    This allows you to access remote resources as though they were local; all
    method calls issued to the RMI forwarder will be synchronously
    network-forwarded to the remote resource URI and their results returned.

    The remote instance runs until this object is destroyed, at which point the
    SSH process and remote ni instance are both killed via SIGTERM.})

->uses(u"ni.behavior:rmi-delegation"
  ->create(
    'Establishes the connection used for RMI communication, storing the
     process locally into $$self{connection}. The $self here is different from
     the RMI method receiver.',

    TODO"Is it appropriate for +packet to be explicit, rather than an implicit
         extension to everything that's readable/writable?",

    NB"The failure modes below are contrived: in practice we'd inherit failure
       modes from things like TCP connections and propagate errors outwards;
       this would result in a very small set of failure modes specific to this
       class.",

    '$self, $uri' => q{
      my ($remote_argv, $init) = remote_instantiation;
      ($$self{connection} =
       u"ni.tcp.ssh+packet", $uri->authority, @$remote_argv)->write($init);
    },

    failure_modes(
    '$self, $uri' => q{
      my $tcp = u"ni.tcp", $uri->authority(port => 22);
      check "DNS lookup of $uri->host"
         => we_expect_defined_from $uri->host->ip;

      check "Connections to $tcp"
         => we_expect_true_from $tcp->connected;

      check "SSH protocol compliance of $tcp"
         => we_expect 'SSH', from => $tcp->peek(3);

      check "SSH client is installed"
         => we_expect_true_from u"ni.program:ssh"->executable;

      check "SSH access for $uri->authority"
         => we_expect qr/^hi/, from => `ssh "$uri->authority" echo hi`;
    }))

  ->method_call(
    'Uses the builtin ni.rmi encoding to send data down the SSH connection,
     then awaits a reply.',
    NB"Obviously we can't monopolize the connection this way because in
       practice it would be multiplexed.",
    '$self, $method, @args' => q{
      $$self{connection}->write_packet(ni_rmi_encode $method, @args);
      ni_rmi_decode $$self{connection}->read_packet;
    })

  ->destroy(
    'Closes the SSH pipe',
    NB"This one isn't really necessary due to refcounting GC, but if it were,
       this is what it would look like.",
    '$self' => q{$$self{connection}->close}))

->eg('Trivial resource access',
     'u"data:,foo"->read returns "foo", so we can access the same resource over
      an SSH connection to localhost. This will only be the case if we can ssh
      to localhost without a password.',
     q{provided `pgrep sshd` ne ""
            and `ssh -o PasswordAuthentication=no localhost echo hi` eq "hi\n",
       we_expect "foo", from => u"ni.rmi.ssh://localhost/data:,foo"->read})

->eg('Connecting to an existing remote',
     'The trick here is to use a ni.rmi.pid:X URI...',
     q{...});
```
