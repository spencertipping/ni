# Preprocess command line, collapsing stuff into array and hash references as
# appropriate.

use POSIX qw/dup2/;

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

sub stream_for {
  my ($stream, @options) = @_;
  $stream //= -t STDIN ? ni_sum() : ni_file('[stdin]', \*STDIN, undef);
  for (parse_commands @options) {
    my ($command, @args) = @$_;
    eval {$stream = $ni::io::{long_op_method $command}($stream, @args)};
    die "failed to apply stream command $command [@args] "
      . "(method: " . long_op_method($command) . "): $@" if $@;
  }
  $stream;
}

sub stream_to_process {
  my ($stream, @process_alternatives) = @_;
  my $fh = $stream->reader_fh;
  if (fileno $fh) {
    close STDIN;
    dup2 fileno $fh, 0 or die "dup2 failed: $!";
  }
  exec $_ for @process_alternatives;
}

sub main {
  $|++;
  my $data = stream_for undef, preprocess_cli @_;
  if (-t STDOUT && !exists $ENV{NI_NO_PAGER}) {
    # Use a pager rather than writing straight to the terminal
    stream_to_process $data, $ENV{NI_PAGER} // $ENV{PAGER} // 'less',
                             'more';

    # Ok, we're out of options; just write to the terminal after all
    print STDERR "ni: couldn't exec any pagers, writing to the terminal\n";
    print STDERR "ni: (sorry about this; if you set \$PAGER it should work)\n";
    print STDERR "\n";
    print while <>;
  } else {
    $data > \*STDOUT;
  }
}

END { main @ARGV }

}
