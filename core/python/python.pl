# Python stuff.
# A context for processing stuff in Python, as well as various functions to
# handle the peculiarities of Python code.

# Indentation fixing.
# This is useful in any context where code is artificially indented, e.g. when
# you've got a multiline quotation and the first line appears outdented because
# the quote opener has taken up space:

# my $python_code = q{import numpy as np
#                     print np};
# # -----------------| <- this indentation is misleading

# In this case, we want to have the second line indented at zero, not at the
# apparent indentation. The pydent function does this transformation for you, and
# correctly handles Python block constructs:

# my $python_code = pydent q{if True:
#                              print "well that's good"};

use POSIX ();

BEGIN
{
  defconfenv 'python3', NI_PYTHON3 => 'python3';
}

sub pydent($) {
  my @lines   = split /\n/, $_[0];
  my @indents = map length(sr $_, qr/\S.*$/, ''), @lines;
  my $indent  = @lines > 1 ? $indents[1] - $indents[0] : 0;

  $indent = min $indent - 1, @indents[2..$#indents]
    if $lines[0] =~ /:\s*(#.*)?$/ && @lines >= 2;

  my $spaces = ' ' x $indent;
  $lines[$_] =~ s/^$spaces// for 1..$#lines;
  join "\n", @lines;
}

sub pyquote($) {"'" . sgr(sgr($_[0], qr/\\/, '\\\\'), qr/'/, '\\\'') . "'"}


sub stdin_to_python($)
{
  eval {cdup2 0, 3; POSIX::close 0};
  die "ni: python driver failed to move FD 0 to 3 ($!)\n"
    . "    this usually means you're running in a context with no STDIN"
  if $@;
  my $fh = siproc {exec conf('python3'), '-'};
  safewrite $fh, $_[0];
  close $fh;
  $fh->await;
}


# Python code parse element.
# Uses the same logic that the perl driver does to ask the python interpreter
# about brackets.
#
# We also detect expressions and replace them with return statements if this is
# syntactically valid; that is, if 'x' can be replaced with 'return (x)'.

use constant py_check =>
  conf('python3') . q{ -c 'import ast; import sys; ast.parse(sys.stdin.read())'};

sub py_returnify($$)
{
  my ($transform, $code) = @_;

  # TODO: we need a way to bypass this check

  my $returned = "return ($code)";
  syntax_check(py_check, &$transform($returned)) ? $code : $returned;
}

BEGIN
{
defparser 'pycode', '$', q{
  my ($self, $code, @xs) = @_;
  return py_returnify($$self[1], $_[1]), '', @_[2..$#_] unless $code =~ /\]$/;

  my $codegen = $$self[1];
  my $status  = 0;
  my $x       = '';

  $x .= ']' while $status = syntax_check py_check, &$codegen($code)
                  and $code =~ s/\]$//;

  die <<EOF if $status;
ni: failed to get closing bracket count for python code "$code$x".
    Because python doesn't use compile-time metaprogramming, this most likely
    means that your code is being checked by a different version of python
    than the one your operation is targeting. You can bypass this problem by
    ending the shell argument containing your python code with a space:

    y'[[some code]]'              # ni checks syntax (problem case)
    y'[[some code]] '             # bypass syntax check
    [y'[some code] ' ]            # bypass syntax check within a lambda
EOF

  (py_returnify($codegen, $code), $x, @xs);
};
}

defparseralias pycode_identity => pycode sub { $_[1], '', @_[2..$#_] };

# Python line processor.
use constant py_mapgen => gen pydent q{
  import os
  import sys
  sys.stdin.close()
  stdin = os.fdopen(3, 'r')
  %prefix
  %closures
  class py_mapper:
    def __init__(self):
      self.first = True
    def is_first(self):
      if self.first:
        self.first = False
        return True
      else:
        return False
    def row(self):
  %body
  each = py_mapper()
  while rl() is not None:
  %each
};

our @python_prefix_keys = qw| core/python/stream.py |;

defoperator python_prefix => q{ sio; print join"\n", @ni::python_prefix_keys };
defshort '///ni/python_prefix' => pmap q{python_prefix_op}, pnone;

sub defpythonprefix($)
{
  warn "defpythonprefix(\"$_[0]\") refers to an undefined attribute"
    unless exists $ni::self{$_[0]};
  push @python_prefix_keys, $_[0];
}

sub python_prefix() { join "\n", @ni::self{@python_prefix_keys} }

sub python_expand_begin($) { sr $_[0], qr/^\s*\^:/, 'if self.is_first():' }

sub python_code($$) {py_mapgen->(prefix   => python_prefix,
                                 closures => '# TODO: dataclosures',
                                 body     => indent($_[0], 4),
                                 each     => indent($_[1], 2))}

sub python_mapper($)
{ python_code python_expand_begin $_[0], pydent
  q{row_out = each.row()
    if type(row_out) is list:
      print("\t".join((str(s) for s in row_out)))
    elif row_out is not None:
      print(str(row_out))
    } }

sub python_grepper($)
{ python_code python_expand_begin $_[0], pydent
  q{if each.row():
      print(_)
    } }

defoperator python_mapper  => q{stdin_to_python python_mapper  pydent $_[0]};
defoperator python_grepper => q{stdin_to_python python_grepper pydent $_[0]};

defmetaoperator python_require => q{
  my ($args, $left, $right) = @_;
  my $code_fh = sni @$args;
  my $code    = join '', <$code_fh>;
  my $key     = "core/python/require/" . gensym;
  self_append_resource $key, $code;
  self_append_resource "$key-prepend.pl",
    qq{ push \@ni::python_prefix_keys, q{$key} };
  push @ni::python_prefix_keys, $key;
  ($left, $right);
};

BEGIN
{
  defparseralias python_mapper_code  => pycode \&python_mapper;
  defparseralias python_grepper_code => pycode \&python_grepper;
}

defshort '/y',
  defalt 'pyalt', 'alternatives for /y python operator',
    pmap q{python_mapper_op $_}, python_mapper_code;

defshort '/yR', pmap q{python_require_op @$_}, _qfn;

defrowalt pmap q{python_grepper_op $_},
          pn 1, pstr 'y', python_grepper_code;
