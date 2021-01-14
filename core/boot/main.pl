# CLI entry point.
# Some custom toplevel option handlers and the main function that ni uses to
# parse CLI options and execute the data pipeline.

our %cli_special;
BEGIN {defdocumentable 'clispecial', \%cli_special}

sub defclispecial($$$) {
  $cli_special{$_[0]} = fn $_[1];
  docclispecial $_[0], $_[2];
}

# Development options.
# Things useful for developing ni.

defclispecial '--dev/eval', q{print ni::eval($_[0], "anon $_[0]"), "\n"}, <<'_';
Development option: evaluate an expression within the `ni::` package, and print
the result. For example:

$ ni --dev/eval '3 + 4'
7
_

defclispecial '--dev/parse', q{dev_trace 'ni::parse'; cli_parse @_}, <<'_';
Development option: trace the ni::parse function and attempt to parse the
command line. Mostly useful for debugging.
_

defclispecial '--dev/parse-one', q{
  dev_trace 'ni::parse';
  parse ni::eval($_[0]), @_[1..$#_];
}, <<'_';
Development option: trace the ni::parse function and evaluate the specified
parser on the rest of the command line arguments. For example:

$ ni --dev/parse-one 'parser "/qfn"' [gc]
_

defclispecial '--dev/doc-check' => q{
  my $undocumented = 0;
  for my $type (sort keys %ni::doc_entities) {
    exists $ni::doc{$type}{$_} or ++$undocumented && print "$type\t$_\n"
      for sort keys %{$ni::doc_entities{$type}};
  }
  print "$undocumented undocumented thing(s)\n";
  !!$undocumented;
}, <<'_';
Development option: find things (parsers, ops, etc) that have been defined but
have no documentation associated with them.
_

# Extensions.
# Options to extend and modify the ni image.

defclispecial '--internal/lib', q{
  extend_self 'lib', $_ for @_;
  modify_self;
  0;
}, <<'_';
Usage: ni --internal/lib lib1 lib2 ... libN
Modifies the ni image in-place to include the specified libraries. See ni
//help/libraries for more information.
_

defclispecial '--extend', q{
  extend_self 'lib', $_ for @_;
  modify_self;
  0;
}, <<'_';
Usage: ni --extend lib1 lib2 ... libN
Modifies the ni image in-place to include the specified libraries.
See ni //help/libraries for more information.

You may also want to use ni --lib libdir, which will run ni with a library but
won't modify it on disk.
_

defclispecial '--lib', q{intern_lib shift; goto \&main}, <<'_';
Usage: ni --lib lib-dir normal-ni-options...
Preloads a library before running normally. ni does not self-modify on disk if
you do this, though the library will be available in remote contexts.
_

defclispecial '--run', q{
  $ni::self{"transient/eval"} .= "\n$_[0]\n";
  ni::eval $_[0], "--run $_[0]";
  shift;
  goto \&main;
}, <<'_';
Usage: ni --run 'perl code' normal-ni-options...
Runs code before running normally.
_


# Updating to latest git image
defclispecial '--upgrade', q{
  my $branch = @_ ? $_[0] : 'develop';
  chomp(my $version = $ni::self{'core/boot/version'});
  chomp(my $online_version = `curl -sSL https://raw.githubusercontent.com/spencertipping/ni/$branch/core/boot/version`);
  if ($version eq $online_version)
  {
    print "ni is already at version $online_version\n";
    exit 0;
  }

  die 'ni is not installed on this machine' unless -r $0;
  die 'ni is not modifiable' unless -w $0;
  print "upgrading $version to $online_version from branch $branch...\n";
  wf "$0.upgrade",
     `curl -sSL https://github.com/spencertipping/ni/blob/$branch/ni?raw=true`;
  chmod +(stat $0)[2], "$0.upgrade";
  die 'new image is corrupt; aborting upgrade'
    unless `$0.upgrade //ni r+1` =~ /^__END__$/m;
  rename "$0.upgrade", "$0" or
    die "failed to replace ni image at $0 with $0.upgraded; aborting upgrade";
  print "ni has been upgraded to version $online_version";
}, <<'_';
Usage: ni --upgrade [branch]
Upgrades to the latest ni version on the develop branch, or whichever branch is
specified.
_

defclispecial '--version', q{
  print "$ni::self{'core/boot/version'}\n";
}, <<'_';
Usage: ni --version
Outputs the version of your ni image.
_


# Documentation.

defclispecial '--explain', q{
  my ($r, @rest) = cli_parse @_;
  print "UNPARSED: @rest\n" if @rest;
  if (ref $r) {
    print json_encode($_), "\n" for flatten_operators $r;
  }
}, <<'_';
Usage: ni --explain normal-ni-options...
Describes the operators and meta-operators produced from the specified command
line. Meta-operators are unevaluated in this form.
_

defclispecial '--explain-meta', q{
  my ($r, @rest) = cli_parse @_;
  print "UNPARSED: @rest\n" if @rest;
  if (ref $r) {
    print json_encode($_), "\n" for apply_meta_operators flatten_operators $r;
  }
}, <<'_';
Usage: ni --explain-meta normal-ni-options...
Describes the operators produced from the specified command line, after
evaluating all meta-operators. Each operator in the output corresponds to a
forked process in the pipeline.
_

defclispecial "--doc/$_", qq{
  if (\@_) {
    eval {print ${_}_doc \$_, "\\n"}, \$@ && warn \$@ for \@_;
  } else {
    print "\$_\\n" for sort keys \%{\$ni::doc_entities{'$_'}};
  }
}, <<_
Usage: ni --doc/$_ [<${_}-name>]
Print documentation for <${_}-name> if specified; otherwise list all ${_}s.
_
for keys %ni::doc;

defclispecial '--doc', q{print "--doc/$_\n" for sort keys %ni::doc}, <<'_';
Usage: ni --doc
Prints a list of all documentable types of things. For example, "parser" is one
such thing, and from there you can run `ni --doc/parser X`, where `X` is the
name of a parser. (`ni --doc/parser` will list all of them.)
_

# Root CLI context.
# This is used by extensions that define long and short options.

defcontext '', q{toplevel CLI context};

# Main stuff.
# sub main() is called by the ni boot header on @ARGV. I've separated
# $main_operator so it can be extended to handle various cases; for instance, ni
# launches a pager when its output is connected to a terminal, etc. This is
# handled by core/stream.

our $main_operator = sub {die "ni: no main operator defined (your ni is broken)"};

sub main {
  my ($cmd, @args) = @_;
  $ni::is_toplevel = 1;

  @_ = ('//help/usage', @_[1..$#_])
    if -t STDIN and -t STDOUT and !@_ || $_[0] =~ /^-h$|^-\?$|^--help$/;

  if (exists $ENV{HOME} && !exists $ENV{NI_NO_HOME} && -d "$ENV{HOME}/.ni") {
    eval {intern_lib "$ENV{HOME}/.ni"};
    if ($@) {
      print STDERR "ni: note: failed to load ~/.ni as a library: $@\n";
      print STDERR "    (export NI_NO_HOME to disable ~/.ni autoloading,\n";
      print STDERR "     or run `ni //help/libraries` for details about libraries)\n";
    }
  }

  return $cli_special{$cmd}->(@args) if defined $cmd && exists $cli_special{$cmd};

  my ($r, @rest) = cli_parse @_;
  return &$main_operator(flatten_operators $r) if !@rest && ref $r;

  print STDERR "ni: failed to parse starting here (ni --dev/parse to trace):\n";
  print STDERR "  @rest\n";
  print STDERR "If ni is failing to parse a filename, start it with /, ./,\n";
  print STDERR "or file:// to qualify it.\n";
  exit 1;
}
