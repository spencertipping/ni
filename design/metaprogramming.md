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
$ ni /usr/share/dict/words D %[* [_:[stuff]] [machine1 machine2 ...]

# same thing, but using the shorthand D%
$ ni /usr/share/dict/words D%[* [_:[stuff]] [machine1 machine2 ...]
```

## Variables
You'll often want to save the list of machines somewhere so you can reuse it:

```sh
$ ni cluster=[machine1 machine2 ...] \
     /usr/share/dict/words D%[* [_:[stuff]] cluster]
```
