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

my $initial = -t STDIN ? ni::io::empty : ni::io::fh->new(\*STDIN);
for (parse_commands preprocess_cli @ARGV) {
  my ($command, @args) = @$_;
  $initial = ${"ni::io::$command"}->($initial, @args);
}
$initial > \*STDOUT;
