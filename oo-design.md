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
$r = uri 'http://foo.com';                      # stream resource
$r = uri 'data:,foo';                           # literal string "foo"
$r = uri 'file://./foo';                        # local file
$r = uri 'hdfs:///foo/bar/bif';                 # HDFS file
$r = uri './foo';                               # file shorthand
$r = uri 'ni.self:/core/pl/init.pl';            # ni state resource
$r = uri 'ni.ls:/core';                         # list resource names
$r = uri 'ni.pid:31891';                        # process resource
$r = uri 'ni.op:["n",10]';                      # stream operation
$r = uri 'ni.rmi.ssh://user@host:port/ni.pid:19241';

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
# obviously we don't construct using interpolation
my $obj         = uri '...';
my $pipeline    = uri '...';
my $hadoop_side = uri qq{ni.rmi.fs-multi+async:["hdfs:///path","$obj"]};
my $host_side   = uri qq{ni.rmi.fs:["hdfs:///path","$pipeline"]};
$host_side->enable_monitoring($monitor_uri);
```

## Protocol metaclasses
```pl
$p = uri 'ni.protocol:readable';
$c = uri 'ni.scheme:http';
$c->implement($p,
  read => fn q{ ... },
  fd   => fn q{ ... }, ...);
```

## Meta-URI syntax
```pl
$c = uri 'ni.scheme:http';              # normal URI syntax
$c = uri 'ni.scheme.json:ni.op';        # modified JSON syntax
```

## Monitors
```pl
my $mon = uri 'ni.wmonitor:fd:1';       # new write monitor around FD 1
print $mon "foo";                       # write data to it
my @xs = $mon->sample;                  # some of the rows
my $bytes_s = $mon->throughput;         # bytes/sec throughput
```
