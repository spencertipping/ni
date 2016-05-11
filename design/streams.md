# Streams
Streams are a type of object that can be manipulated by Canard, and are the
default things produced by ni command-line options. A stream is defined as a
flat-mapping transformation over an input, with a concatenation against
error/diagnostic output. In other words, it's a UNIX process, and it can always
be expressed as such.

Unlike `nfu`, which used fork/exec to build its own pipelines internally, `ni`
serializes a stream into shell commands and executes it externally. This makes
streams much easier to inspect and should bypass some types of
state-propagation errors that arise when Perl is filtering the input/output of
a subprocess.

## How ni uses stderr
```sh
$ command1 < infile | command2 | command3 > outfile
```

The pipeline above is a function composition of sorts:
`outfile = command3(command2(command1(infile)))`. As with any functional
program, though, we're separating data transformation from the process. ni
can't afford to do this, of course, because then we have no profiling for
individual processing stages. That's where stderr comes in:

```sh
$ { command1 | command2 | command3; } < infile > outfile 2>logfile
```

Now we have two equations:

```
outfile = command3(command2(command1(infile).stdout).stdout).stdout
logfile = command1(infile).stderr
        + command2(command1(infile).stdout).stderr
        + command3(command2(command1(infile).stdout).stdout).stderr
```

The logfile's contents provide metadata about the running processes as
processes rather than as data transformers. ni uses this to collect information
about buffer states, bottlenecks, etc.
