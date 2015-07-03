# Difficulties using nfu
## Stream aliasing
nfu's biggest deficiency is that it often forces you to buffer a stream
manually; it's difficult to alias something, for example to calculate
quantiles.

```sh
$ nfu ... --duplicate : [ -gA ... ]
```

The above doesn't work because `--duplicate` combines row-wise, not juxtaposing
columns. Is broadcasting the right model to use for this? There might be a
situation where the fork emits values to be joined, for example. What we really
want is a first-class operator that forks the current stream, something like
this:

```sh
$ ni ... --zip @[ ... ]         # @[] forks the current stream
$ ni ... --zip  [ ... ]         #  [] starts with an empty stream
```

Forked streams are always disk-buffered, compressed by default, and deleted
after use. You can modify this by setting configuration variables prior to the
fork.

Is it worth also including support for named streams, or other such first-class
representations? It's sketchy to do this because it makes debugging much more
difficult. I also don't know whether there's a compelling use case for it.

If the goal is to build a general-purpose stream computation system, maybe
named or otherwise first-class streams play a role...

Ok, named things in general are a problem; this is why point-free programming
is so useful. Using a single implicit operand biases development towards
concrete state models rather than harder-to-trace event queues. This, in turn,
improves execution transparency and keeps it possible to serialize things.

## Branching
One thing we should have is map-based branching. `{...}` is an alternative
lambda form, `@{...}` a forking operator, and `-{ ... }` is a branching
operator. Branches always dispatch on the first field's value.

```sh
$ ni ... -{ a ... , b ... , c ... } ...
```

(On a related note, we should have a `--zip` operator that accepts infinite
streams and early-terminates. We should also have a `--cycle` modifier to
broadcast short streams.)
