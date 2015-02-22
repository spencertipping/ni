# Preprocess command line, collapsing stuff into array and hash references as
# appropriate.

sub preprocess_cli {
  my @preprocessed;
  for (my $o; defined($o = shift @_);) {
    if ($o =~ s/\[$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\]$/;
        $depth += /\[$/;
        push @xs, $_;
      }
      push @preprocessed, bless [@xs], $o;
    } elsif ($o =~ s/\{$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\}$/;
        $depth += /\{$/;
        push @xs, $_;
      }
      push @preprocessed, bless {@xs}, $o;
    } else {
      push @preprocessed, $o;
    }
  }
  @preprocessed;
}

$|++;
my $data = -t STDIN ? ni::io::empty : ni::io::fh->new(\*STDIN);
for (parse_commands preprocess_cli @ARGV) {
  my ($command, @args) = @$_;
  $data = $ni::io::{$command}($data, @args);
}

if (-t STDOUT) {
  # Preview using a pager
  my $fifo = ni_fifo();
  if (fork) {
    close STDIN;
    dup2 fileno $fifo->{out}, 0 or die "failed to redirect stdin for pager: $!";
    exec $ENV{NI_PAGER} // 'less';
  } else {
    $data->into($fifo);
  }
} else {
  $data->into(\*STDOUT);
}
