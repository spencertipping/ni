# Self invocation.
# You can run ni and read from the resulting file descriptor; this gives you a
# way to evaluate lambda expressions (this is how checkpoints work, for example).
# If you do this, ni's standard input will come from a continuation of __DATA__.

use Errno qw/EINTR/;

our @quoted_resources;

sub quoted_resources()     {@quoted_resources}
sub add_quoted_resource($) {push @quoted_resources, $_[0]}

sub safereadbuf($$$;$) {
  my $n;
  do {
    return $n if defined($n = read $_[0], $_[1], $_[2], $_[3] || 0);
  } while $!{EINTR};
  return undef;
}

defclispecial '--internal/operate-quoted', q{
  my $parent_env = json_decode $ni::self{'quoted/env'};
  $ENV{$_} ||= $$parent_env{$_} for keys %$parent_env;

  sforward_buf_unquoted $ni::data, resource_write($_)
    for @{json_decode $ni::self{'quoted/resources'}};

  $ni::is_toplevel = 0;
  my $fh = siproc {
    &$ni::main_operator(flatten_operators json_decode $ni::self{'quoted/op'});
  };
  safewrite $fh, $_ while safereadbuf $ni::data, $_, 8192;
  close $fh;
  $fh->await;
}, <<'_';
Internal option: causes ni to parse keys within its image and use those as the
list of operators to run. This is how all of ni's remoting is done; the
indirection prevents us from hitting any size limits on ARGV or ENV.
_

sub sforward_quoted($$) {
  my ($n, $b);
  safewrite $_[1], pack 'na*', $n, $b while $n = saferead $_[0], $b, 8192;
  safewrite $_[1], pack 'n', 0;
}

sub sforward_buf_unquoted($$) {
  my ($n, $nb, $b, $eof) = (0, '', '', 0);
  while (!$eof and safereadbuf $_[0], $nb, 2 and ($n) = unpack 'n', $nb) {
    $b = '';
    $eof ||= !safereadbuf $_[0], $b, $n - length($b), length $b
      until $eof or length($b) >= $n;
    safewrite $_[1], $b;
  }
}

sub ni_quoted_exec_args() {qw|perl - --internal/operate-quoted|}

sub ni_quoted_image($@) {
  my ($include_quoted_resources, @args) = @_;
  image_with
    'quoted/op'        => json_encode [@args],
    'quoted/env'       => json_encode {%ENV},
    'quoted/resources' => json_encode($include_quoted_resources
                                        ? [@quoted_resources]
                                        : []);
}

sub quote_ni_into($@) {
  my ($fh, @args) = @_;
  safewrite $fh, ni_quoted_image 1, @args;
  sforward_quoted resource_read($_), $fh for @quoted_resources;
  sforward \*STDIN, $fh;
  close $fh;
  $fh->await;
}

sub exec_ni(@) {
  my $ni = siproc {exec ni_quoted_exec_args};
  quote_ni_into $ni, @_;
}

sub sni(@) {soproc {nuke_stdin; exec_ni @_} @_}
