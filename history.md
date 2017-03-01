# Changes and why they happened
### b27: Remove the `/` prefix from ni namespaces
Prior to this change, ni used package names like `/object` and `/lib/fn`. I
would have preferred that (since they're visually distinct from normal perl
classes), but Perl 5.14 and prior don't support method calls on packages
beginning with `/`. This caused test failures on centos 5-6 and ubuntu 12.04.

- **c27:** Changed this to just impact packages; now ni named objects begin
  with `/` as before, but the OO-related behaviors convert this.

### q27: Completely change the way images are serialized
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

### r27: Factor state into "commits" and structural values
A few new invariants, still tentative:

1. Modifiable values have names; no hidden identity.
2. State-of-visibility, and by extension perl packages, are modified through
   commit objects; commits can be reverted. This results in a fully-journaled
   runtime state.
3. Commits are the only objects that are evaluated.

- **137:** Considering flat object storage, in which case commit-like behavior
  would be a virtual method that performs the eval. The root commit would
  install the virtual method for function objects.
    - Along these lines, is it worth doubly serializing and just emitting the
      subs directly into the image? This could be a lot faster than evaling
      them into existence. (**Update:** not faster at all; it's marginally
      slower in fact.)
