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
7. Session recovery? (Thinking...)

## URLs for objects
```pl
$r = uri 'http://foo.com';              # stream resource
$r = uri 'ni.self://core/pl/init.pl';   # ni state resource
$r = uri 'ni.pid://31891/...';          # connect to existing ni?
$r = uri 'ssh://user@host:port/...';    # forwarded resource

print "$r\n";   # named
print <$r>;     # readable
```
