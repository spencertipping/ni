#`ni` development for masochists, Chapter 1

This isn't a joke. There will be jokes, but this is not one of them.

If you're thinking about developing `ni`, you've probably got a good facility with the language, and I don't think I need to impress upon you that `ni` is uncompromising. This is probably true of its design; I can't tell you for sure because I don't know enough of it right now, and Spencer will say that it's crap, which it is, but it's still highly functional crap.

Anyway, my point is, `P` more than likely ain't `NP`  and the level of Perl you'll need to develop `ni` is far beyond the level you needed to be useful with it. 

This is my liveblog as I get deeper into the internals of the language; coal mine, meet canary.

Let's get into this.

##`lazytest`-driven development

If you look at the underlying Markdown files for the `ni` docs, you'll see code blocks decorated with \`\`\`bash. This is a cue for `lazytest`, `ni`'s testing tool (and another [Spencer Tipping](https://github.com/spencertipping/lazytest) joint).

I assume you know how to use git checkout and git reset

Let's say you want to edit `ni`, create a pull request, and have that pull request accepted. You go into `core`, edit a file, it looks good.

In the `ni` base directory.

Importantly

That means, when you're doing your development, you should be maintaining test cases in the appropriate file. That way correctness of new operators can be verified easily. I'm going to go do that for my last pull request right now.


## `sdoc` structure
### Code Paragraphs
`sdoc` has meaningful whitespace; 

### Comments and `c`
Comments start with a capital letter; to force the following lines to be interpreted as code, you prefix the paragraph with a line of just a `c`.



##`defoperator`

Well,

##`pmap`

## Perl 

### `defconfenv`

`defconfenv` is used to define a `ni` environment variable; its syntax is the following:

`defconfenv <ni_config_variable_name>, <environment_variable_name> => <default value>`

Good examples of `defconfenv` are at the top of `core/hadoop/hadoop.pl`

### `defperlprefix`

`defperlprefix` allows you to import files; however, they must be known to `ni` by adding them to the appropriate `lib` file **in order**.

A good example of this is the `core/hadoop` library; 


```
# core/hadoop/lib
hadoop-conf.pl
hadoop.pl
```

```
# core/hadoop/hadoop-conf.pl
...
our %mr_generics (
'Hpkpo', 'mapreduce.partition.keypartition.options',
...
);
...
```

```
# core/hadoop/hadoop.pl
defperlprefix 'core/hadoop/hadoop-conf.pl';
our %mr_generics;
for (keys %mr_generics) {
  my $env_var_name = uc $mr_generics{$_};
  defconfenv $_, $env_var_name => undef;
}
```

A package-level variable `our %mr_generics`, corresponding to short names for Hadoop Streaming MapReduce operators is written in `hadoop-conf.pl`. This hash is imported by `hadoop.pl` using `defperlprefix`; the package-level variable `our %mr_generics` is then imported, and a large number of environment variables are created using a loop. Note that `defconfenv` would not have worked had it been placed in the `hadoop-conf.pl` file, and that `%mr_generics` must be explicitly imported.
