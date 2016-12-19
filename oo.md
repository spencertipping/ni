# OO refactor
Thinking about this before committing to it; thoughts so far:

1. Using objects to model subprocesses (kind of a no-brainer given the trouble
   I'm having with `siproc` etc)
2. Committing fully to the functions-are-self-documenting-objects idea, though
   it's unclear how closures should work and it does impose the double-eval
   overhead
3. Do we need SDoc, or can we use metaprogramming? Metaprogramming would
   provide much better documentation, most likely.
4. Better autodocs?
5. Way better monitoring/debugging (drop into a shell after running `less`,
   maybe)
6. Live REPL stuff maybe? This could be really awesome.

Auxiliary goals:

1. Generalize ni aggressively: it should provide multiple images, one of which
   is suitable for remote self-installation and minimal command execution (but
   isn't used for data science; this would be ideal for SSH-based
   sysadmin/monitoring)
2. Get better control over the image state; the "ni.map" stuff is lame,
   uninspired, and clunky.

Reservations:

1. Perl OOP and metaprogramming is how you make stuff slow.
2. It's a lot of work.

Benefits >> costs. Let's do this.

## OO design elements
Let's commit to the idea that every sublinear component of ni is implemented as
an object. This includes:

- Functions
- Modules/libraries
- Documentation
- Parsers
- Images/states (which are collections of modules?)
- Image directories maybe? (how do these differ from modules?)
- Unit tests? (this lets you test ni in the field ... could be useful)
- Data closures and other transient state
- Subprocesses/pipeline elements
- Pipelines as entire objects -> model deps, send minified ni images around
- URIs
- Commands you've run

While we're at it, let's introduce the ability to process `.ni` script files
without using bash as an intermediate layer. This means we need a
metaprogramming context that provides access to stuff like env vars, wildcards,
etc.
