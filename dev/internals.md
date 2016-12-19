# ni internals
ni is a self-modifying Perl script -- that is, one that can produce itself or a
modified version of itself without reading its source from disk. This mechanism
is what enables it to self-install remotely without writing any files, and it
also powers [data closures](../doc/closure.md).

All of the bootstrap code lives in [core/boot](../core/boot).

## Self-modifying structure
**Note:** ni's self-modification logic is different from [self-modifying Perl
objects](https://github.com/spencertipping/perl-objects), although it uses a
similar mechanism.

In order to be self-modifying by the constraints above, ni has to store all of
its code before executing it. This is done by organizing all of its code into a
resource stream that is interpreted by a self-storing
[header](../core/boot/ni). For convenience, the header knows how to expand
[SDoc](https://github.com/spencertipping/sdoc), and it automatically evaluates
anything that ends in `.pl`. It does not evaluate anything ending in `.pm` --
those files are treated as plain text data.

Here's how the header works (a simpler version of the real code):

```pl
eval($ni::self{ni} = <<'_');            # store bootstrap code immediately
use strict;
sub next_resource_from {...}            # not literally, but the same idea
sub unsdoc {...}
while (my ($name, $resource) = next_resource_from \*DATA) {
  unsdoc $name, $resource      if $name =~ /\.sdoc$/;
  eval "package ni; $resource" if $name =~ /\.pl$/;
  chomp($ni::self{$name} = $resource);
}
exit ni::main(@ARGV);
_
__DATA__
# ni resource stream goes here
# ni sysread replay goes here (for file-backed data closures)
# input stream goes here if ni is being run with STDIN == DATA
```

In reality the header is organized a little differently and manages a few
things I didn't include in the above code, but the basic idea is the same. The
key invariants are these:

1. After running, the header can reproduce itself and the whole resource
   stream, which is stored in `%ni::self`.
2. Parsing the resource stream will cause all of the relevant Perl code to be
   evaluated, making it possible to call into the `ni::main` function.

## Resource stream
This is sort of like a serialized filesystem for text data. Each resource is
formatted like this:

```
4 test/hello                    # first resource: four lines long
hello world!
this is a resource that occupies four lines
of text
of which this is the last
2 core/boot/thing.pl            # second resource: two lines long
print "this thing is ";
print "not especially useful\n";
```

ni's header will parse the above into two `%ni::self` keys, and
`core/boot/thing.pl` will be evaluated in the `ni` package immediately after it
is parsed. ni uses the functions in
[core/boot/self.pl](../core/boot/self.pl.sdoc) to produce new resource streams
from the set of keys in `%ni::self`.

## `ni.map`
Perl is very sensitive to the order in which you evaluate things. ni doesn't
record the order of resources as they're parsed, so by the time we want to
generate a new resource stream that ordering is completely lost. (There are
also some complications from stuff like SDoc, each of which results in two
`%ni::self` keys.)

[ni.map](../core/boot/ni.map.sdoc) is a set of instructions that describe how
to rebuild the resource stream in its original order. ni modifies this map as
you add libraries using `--internal/lib` (in this case done by
[the build script](../build)), and then replaces itself on disk with the image
produced from this modified map.
