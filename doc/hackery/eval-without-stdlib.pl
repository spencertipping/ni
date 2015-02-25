# Can we eval code in a world where all stdlib stuff has been replaced with
# custom functions?
eval q{
  BEGIN {
    for (qw/open close foo tie grep/) {
      my $n = $_;
      eval qq{ sub $n {
        print "yes we can: $n(\@_)\n";
      }};
      *{"CORE::GLOBAL::$_"} = \&$n;
    }
  }

  open my $fh, "< foo";
  close $fh;
  foo 'bar';
  my $x;
  tie $x, 'foo';
  grep /foo/, qw/foo bar bif/;
};
die $@ if $@;
