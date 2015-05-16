## Lambdas
Because ni's command-line arguments are concatenative, you can quote a list
and have that represent a stream transformer. For example:

```sh
$ ni n:5 -00m/*
$ ni n:5 [ -00m/* ] --eval              # same as above
$ ni n:5 ^00m/* --eval                  # ditto
$ ni n:5 [ [ -00m/* ] --eval ] --eval   # ditto
```

Some commands like `-a` and `-r` process streams rather than individual rows,
so you can pass a lambda rather than a piece of code:

```sh
$ ni n:1000 -R/+                # binary fold
$ ni n:1000 -r [ -R/+ ]         # fold-all, streaming into a binary fold
```
