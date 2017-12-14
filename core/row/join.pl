# Streaming joins.
# The UNIX `join` command does this, but rearranges fields in the process. ni
# implements its own operators as a workaround.

defoperator join => q{
  my ($left_cols, $right_cols, $f) = @_;
  my $fh = sni @$f;
  my ($leof, $reof) = (0, 0);
  my ($llimit, @lcols) = @$left_cols;
  my ($rlimit, @rcols) = @$right_cols;
  my @rrows = ();
  my @lrows = ();

  chomp(my $lkey = join "\t", (split /\t/, my $lrow = <STDIN>, $llimit + 1)[@lcols]);
  chomp(my $rkey = join "\t", (split /\t/, my $rrow = <$fh>,   $rlimit + 1)[@rcols]);

  while (!$leof && !$reof) {
    if ($lkey lt $rkey) {
      print "$lkey < $rkey\n";
      chomp($lkey = join "\t", (split /\t/, $lrow = <STDIN>, $llimit + 1)[@lcols]);
      $leof ||= !defined $lrow;
    } elsif ($lkey gt $rkey) {
      print "$lkey > $rkey\n";
      chomp($rkey = join "\t", (split /\t/, $rrow = <$fh>,   $rlimit + 1)[@rcols]);
      $reof ||= !defined $rrow;
    } else {
      @rrows = ($rrow,);
      while(!$reof) {
        chomp(my $new_rkey = join "\t", (split /\t/, $rrow = <$fh>, $rlimit + 1)[@rcols]);
        $reof ||= !defined $rrow;
        if($new_rkey eq $rkey) {
          push @rrows, $rrow;
        } else {
          $rkey = $new_rkey;
          last;
        }
      }

      while(!$leof) {
        chomp $lrow;
        print "$lrow\t$_" for @rrows;
        chomp(my $new_lkey = join "\t", (split /\t/, $lrow = <STDIN>, $llimit + 1)[@lcols]);
        if ($new_lkey eq $lkey) {
        } else {
          $lkey = $new_lkey; 
          last;
        }
      }
    }
  }
};

defshort '/j', pmap q{join_op $$_[0] || [1, 0], $$_[0] || [1, 0], $$_[1]},
               pseq popt colspec, _qfn;
