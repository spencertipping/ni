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
$r = uri 'ni.self:/core/pl/init.pl';            # ni state resource
$r = uri 'ni.ls:/core';                         # list resource names
$r = uri 'ni.pid:31891';                        # process resource
$r = uri 'ni.op:["n",10]';                      # stream operation
$r = uri 'file://./foo';                        # local file
$r = uri 'hdfs:///foo/bar/bif';                 # HDFS file
$r = uri './foo';                               # file shorthand
$r = uri 'ssh://user@host:port/ni.pid:19241';   # forwarded resource

print "$r\n";           # named
print while <$r>;       # readable
print "$_\n" for @$r;   # required for memory-based resources
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
  - `+tmp+rm` works as intended because `+tmp` proxies operations.
  - `+rm+tmp` also works as intended, though for a different reason.

## RMI object URIs
```pl
my $mapper_rmi    = uri 'ni.rmi.hdfs-multi+async:///path';
my $host_rmi      = uri 'ni.rmi.hdfs:///path';
my $future_result = $mapper_rmi->log_monitor("line of data");
$future_result->await(sub {
  print "got a reply from the host: $_[0]\n";
});
$host_rmi->kill(9);             # kill the mapper processes
```

Not quite right: why would an RMI always operate against the "current ni
instance?" That seems like a recipe for disaster; it really should apply to a
pipeline or something (i.e. a specified receiver).
