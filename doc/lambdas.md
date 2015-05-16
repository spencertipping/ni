## Lambdas
Because ni's command-line arguments are concatenative, you can quote a list
and have that represent a stream transformer. For example:

```sh
$ ni n:5 -00x*
$ ni n:5 [ -00x* ] --eval               # same as above
$ ni n:5 ^00x* --eval                   # ditto
$ ni n:5 [ [ -00x* ] --eval ] --eval    # ditto
```

Some commands like `-a` and the `%` flag process streams rather than individual
rows, so you can pass a lambda rather than a piece of code:

```sh
$ ni n:1000 -a [ -st+1 ]        # -st+1 for the rows in each key-group
$ ni n:1000 -a ^st+1            # same, but using lambda shorthand
```
