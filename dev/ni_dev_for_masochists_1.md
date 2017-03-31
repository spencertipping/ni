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



##`defoperator`

Well,

##`pmap`

