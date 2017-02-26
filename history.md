# Changes and why they happened
### 2017.0211: Remove the `/` prefix from ni namespaces
Prior to this change, ni used package names like `/object` and `/lib/fn`. I
would have preferred that (since they're visually distinct from normal perl
classes), but Perl 5.14 and prior don't support method calls on packages
beginning with `/`. This caused test failures on centos 5-6 and ubuntu 12.04.

- **2017.0212:** Changed this to just impact packages; now ni named objects
  begin with `/` as before, but the OO-related behaviors convert this.

### 2017.0226: Completely change the way images are serialized
This is a big change driven by a couple of factors. One is that the initial
serialization format was always a bit of a hack, albeit one that worked fairly
well. But more importantly, Perl 5.8 will fail to recognize overloads that are
added after an object is blessed: the "is-overloaded" marker is never updated.
This means we need to change the serialization order to install overload
markers before any instantiation is done.

As long as big changes are happening, the serialization format should become
more RPC-friendly by using `pack`-strings to encode data -- and a convenient
way to do this is to have classes specify the encoding strategy for their
instances.
