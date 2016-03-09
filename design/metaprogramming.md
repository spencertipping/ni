# Metaprogramming
Nobody is going to write out the huge list of commands required to implement
map/reduce as shown in [distributed.md](distributed.md), nor would you even be
able to do so reliably without hitting argument list size limits. Rather than
typing it out, we want to generate the commands using metaprogramming.

ni's command-line syntax is actually a pure-functional concatenative
programming language called [Canard](https://github.com/spencertipping/canard)
that contains list-generation and templating functions. These functions are all
prefixed with `%` at the toplevel. For example:

```sh
$ ni /usr/share/dict/words              # direct
$ ni %. [/usr/share/dict/words]         # use canard eval to do the same thing

# you'll see stuff like this later, but it is NOT the same:
$ ni %[/usr/share/dict/words]   # NOPE NOPE NOPE
```

If you're calling multiple functions in succession, you can factor off the `%`
prefix by using brackets:

```sh
$ ni %:^ %:^ %:^ [x y z]                # uncons three times
$ ni %[:^ :^ :^] [x y z]                # same thing, factoring out %
```

## Lists and mapping
One of the most common things metaprogramming is good for is generating
redundant lists of commands to be split across a cluster of machines. For
example:

```sh
# long way: retype the same stuff for each machine
$ ni /usr/share/dict/words D[machine1:[stuff] machine2:[stuff] ...]

# better way: use metaprogramming
$ ni /usr/share/dict/words D %[* [_:[stuff]] [machine1 machine2 ...]]

# same thing, but packing out some unnecessary whitespace and brackets
$ ni /usr/share/dict/words D%*[_:[stuff]] [machine1 machine2 ...]
```

## Variables
You'll often want to save the list of machines somewhere so you can reuse it:

```sh
$ ni cluster=[machine1 machine2 ...] \
     /usr/share/dict/words D%*[_:[stuff]] cluster
```

## Generating map/reduce workflows
In practice you'd do this using one of the standard library functions, but
here's how it works. You've got a list of machines, a mapper, a combiner, and a
reducer:

```sh
$ ni map/reduce [machines] [mapper] [combiner] [reducer]
```

We need to generate the stream connections to execute [the
workflow](distributed.md), which we can do using Canard list manipulation
functions:

```
def map/reduce [
  TODO
  : [is   [iota count machines]
     sshs [*[[ssh x]://[x]] machines]
     mrow .[*[[o @combiner ssh nth machines c = str [r c]]://[c]] is]
     m    [*[mrow] is]

     rrow .[[M[* TODO: figure out quasiquoting here]]]]

  : [machines mapper combiner reducer]]
```
