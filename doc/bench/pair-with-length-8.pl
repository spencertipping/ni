use POSIX qw/dup2/;

close STDOUT;
open my $fh, '> /dev/null' or die $!;
dup2 1, fileno $fh or die $!;

while (<>) {
  chomp;
  print "$_\t" . length($_) . "\n";
}
