# Portability
ni needs to be able to run in hostile environments, which in this case means
UNIX systems where you don't have root access. It's fine for you to get reduced
functionality if supporting programs/libraries aren't there: ni isn't the RVM,
for example. But ni should do what it can to patch over these deficiencies for
core workflows.

## Stuff we can't hard-rely on
That is, these are things whose absence ni has to tolerate somehow; it's not
reasonable for ni to refuse to operate because these things are missing.

- A read/write filesystem.
- A C compiler. I just found out that not all Docker images include the
  POSIX-specified `c99` compiler, and it's conceivable that minimalistic Linux
  images wouldn't have it either.
- Any specific version of Perl, though we can assume v5.x is installed just
  because it's so common and without it we have few other options. (I'm not
  writing ni's argument parser in POSIX sh, tempting thought that is.)
- A JVM, obviously.
- `/bin/bash`, as opposed to `/bin/sh`.
- GNU coreutils, as opposed to POSIX coreutils. (This impacts the options we
  can pass to `/usr/bin/sort`, among other things.)
- Ruby, Python, Octave, R, or any other scripting language.

## What we can assume
I think it's fine to assume we have Perl 5.0.0, and that we can pipe the ni
image into `ssh machine perl` to run it. That Perl process is at liberty to
`exec` a shell pipeline, provided all of the subcommands will work correctly.

It's probably also fine to degrade ni's functionality when running on a
read-only filesystem. `sort` will trivially have this problem, for example --
so any read-only nodes are relegated to simple streams rather than having the
ability to buffer data.
