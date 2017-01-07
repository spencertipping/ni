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
to automatically delete a resource after reading it. The `+rm` here is a
generic modifier that applies to any removable, readable resource and is
implemented by metaclasses and broadcasting.

- `+async`: modify all methods to return futures of results and not block
- `+rm`: autonuke after reading
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
u"ni.scheme:ni.scheme.rmi"->create('ni.rmi.ssh',
  name        => 'SSH RMI forwarder',
  synopsis    => q{u"ni.rmi.ssh://[user@]host[:port]/remote resource URI"},
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
     process locally into $$self{rmi_state}{connection}.',
    TODO"Is it appropriate for +packet to be explicit, rather than an implicit
         extension to everything that's readable/writable?",
    '$self, $uri' => q{
      $$self{rmi_state}{connection} =
        u"ni.pipe.ssh+packet", $uri->host, $uri->path;
    })

  ->method_call(
    'Uses the builtin ni.rmi encoding to send data down the SSH connection,
     then awaits a reply.',
    NB"Obviously we can't monopolize the connection this way because in
       practice it would be multiplexed.",
    '$self, $method, @args' => q{
      $$self{rmi_state}{connection}->write_packet(ni_rmi_encode $method, @args);
      ni_rmi_decode $$self{rmi_state}{connection}->read_packet;
    })

  # this one isn't really necessary due to refcounting GC, but if it were,
  # here's what it might look like:
  ->destroy(
    'Closes the SSH pipe',
    '$self' => q{$$self{rmi_state}{connection}->close}))

->eg('Trivial resource access',
     'u"data:,foo"->read returns "foo", so we can access the same resource over
      an SSH connection to localhost. This will only be the case if we can ssh
      to localhost without a password.',
     q{provided `pgrep sshd` ne ""
            and `ssh -o PasswordAuthentication=no localhost echo hi` eq "hi\n",
       we_expect "foo", from => u"ni.rmi.ssh://localhost/data:,foo"->read})

->eg('Connecting to an existing remote',
     'The trick here is to use a ni.pid:X URI...',
     q{...});
```
