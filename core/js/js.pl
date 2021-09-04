# NodeJS stuff.
# A context for nodeJS mappers and filters.

use POSIX ();

BEGIN
{
  defconfenv 'js', NI_JS => 'nodejs';
}


# FIXME: this needs to handle various escapes; '' in JS is soft-quote
sub jsquote($) {"'" . sgr(sgr($_[0], qr/\\/, '\\\\'), qr/'/, '\\\'') . "'"}


sub stdin_to_js($)
{
  eval {cdup2 0, 3; POSIX::close 0};
  die "ni: nodeJS driver failed to move FD 0 to 3 ($!)\n"
    . "    this usually means you're running in a context with no STDIN"
  if $@;
  my $fh = siproc {exec conf('js'), '-'};
  safewrite $fh, $_[0];
  close $fh;
  $fh->await;
}

use constant js_check => conf('js') . q{ -c -};

sub js_returnify($$)
{
  my ($transform, $code) = @_;

  # TODO: we need a way to bypass this check

  my $returned = "return ($code)";
  syntax_check(js_check, &$transform($returned)) ? $code : $returned;
}

BEGIN
{
defparser 'jscode', '$', q{
  my ($self, $code, @xs) = @_;
  return js_returnify($$self[1], $_[1]), '', @_[2..$#_] unless $code =~ /\]$/;

  my $codegen = $$self[1];
  my $status  = 0;
  my $x       = '';

  $x .= ']' while $status = syntax_check js_check, &$codegen($code)
                  and $code =~ s/\]$//;

  die <<EOF if $status;
ni: failed to get closing bracket count for nodeJS code "$code$x".
    Because nodeJS doesn't use compile-time metaprogramming, this most likely
    means that your code is being checked by a different version of nodeJS
    than the one your operation is targeting. You can bypass this problem by
    ending the shell argument containing your nodeJS code with a space:

    js'[[some code]]'             # ni checks syntax (problem case)
    js'[[some code]] '            # bypass syntax check
    [js'[some code] ' ]           # bypass syntax check within a lambda
EOF

  (js_returnify($codegen, $code), $x, @xs);
};
}

defparseralias jscode_identity => jscode sub { $_[1], '', @_[2..$#_] };

# NodeJS line processor.
use constant js_mapgen => gen q{
%prefix
%closures

fs.closeSync(0);

let is_first = true;
function row(_)
{
%body
}

function go(_)
{
  if (_ == null) return;
  %each
}

function next_block() { rl(go, next_block) }
next_block();
};

our @js_prefix_keys = qw| core/js/stream.js |;

defoperator js_prefix => q{ sio; print join"\n", @ni::js_prefix_keys };
defshort '///ni/js_prefix' => pmap q{js_prefix_op}, pnone;

sub defjsprefix($)
{
  warn "defjsprefix(\"$_[0]\") refers to an undefined attribute"
    unless exists $ni::self{$_[0]};
  push @js_prefix_keys, $_[0];
}

sub js_prefix() { join "\n", @ni::self{@js_prefix_keys} }

sub js_expand_begin($)
{ sr $_[0], qr/^\s*\^\{/, 'if (is_first) { is_first = false; '}

sub js_code($$) {js_mapgen->(prefix   => js_prefix,
                             closures => '// TODO: dataclosures',
                             body     => $_[0],
                             each     => $_[1])}

sub js_mapper($)
{ js_code js_expand_begin $_[0],
  q{let row_out = row(_);
    if (row_out != null)
    {
      while (true)
      {
        try
        {
          // TODO: replace writeSync with something that can buffer for
          // performance
          if (row_out instanceof Array)
            fs.writeSync(1, row_out.join("\t") + "\n");
          else
            fs.writeSync(1, `${row_out}\n`);
          return;
        }
        catch (err)
        {
          if (err.message.indexOf('EAGAIN') < 0)
            throw err;
        }
      }
    }} }

sub js_grepper($)
{ js_code js_expand_begin $_[0], q{if (row(_)) fs.writeSync(1, _);} }

defoperator js_mapper  => q{stdin_to_js js_mapper  $_[0]};
defoperator js_grepper => q{stdin_to_js js_grepper $_[0]};

defmetaoperator js_require => q{
  my ($args, $left, $right) = @_;
  my $code_fh = sni @$args;
  my $code    = join '', <$code_fh>;
  my $key     = "core/js/require/" . gensym;
  self_append_resource $key, $code;
  self_append_resource "$key-prepend.pl",
    qq{ push \@ni::js_prefix_keys, q{$key} };
  push @ni::js_prefix_keys, $key;
  ($left, $right);
};

BEGIN
{
  defparseralias js_mapper_code   => jscode \&js_mapper;
  defparseralias js_grepper_code  => jscode \&js_grepper;
}

defshort '/js',
  defalt 'jsalt', 'alternatives for /js JS operator',
    pmap q{js_mapper_op $_}, js_mapper_code;

defshort '/jsR', pmap q{js_require_op @$_}, _qfn;

defrowalt pmap q{js_grepper_op $_},
          pn 1, pstr 'js', js_grepper_code;
