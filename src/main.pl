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

$|++;
my $data = -t STDIN ? ni_sum() : ni_file(\*STDIN);
for (parse_commands preprocess_cli @ARGV) {
  my ($command, @args) = @$_;
  $data = $ni::io::{$command}($data, @args);
}

if (-t STDOUT && !exists $ENV{NI_NO_PAGER}) {
  # Use a pager rather than writing straight to the terminal
  close STDIN;
  dup2 0, fileno $data->into_fh or die "dup2 failed: $!";
  exec $ENV{NI_PAGER} // $ENV{PAGER} // 'less';
  exec 'more';
  # Ok, we're out of options; just write to the terminal after all
  print while <>;
} else {
  $data > \*STDOUT;
}
