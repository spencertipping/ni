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
      chomp($lkey = join "\t", (split /\t/, $lrow = <STDIN>, $llimit + 1)[@lcols]);
      $leof ||= !defined $lrow;
    } elsif ($lkey gt $rkey) {
      chomp($rkey = join "\t", (split /\t/, $rrow = <$fh>,   $rlimit + 1)[@rcols]);
      $reof ||= !defined $rrow;
    } else {
      @rrows = $rrow;
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

      my @clean_rrows = ();
      my %delete_inds = map {$_ => 1} @rcols;
      for my $rrow(@rrows) {
        my @row_data = split /\t/, $rrow;
        push @clean_rrows, join "\t", @row_data[grep {not $delete_inds{$_}} 0..$#row_data];
      }
      # If we join on all the columns on the right
      # we'll need to append a newline;
      @clean_rrows = map {substr($_, -1) ne "\n" ? "$_\n" : $_ } @clean_rrows;

      while(!$leof) {
        chomp $lrow;
        print "$lrow\t$_" for @clean_rrows;
        chomp(my $new_lkey = join "\t", (split /\t/, $lrow = <STDIN>, $llimit + 1)[@lcols]);
        if ($new_lkey ne $lkey) { $lkey = $new_lkey; last;}
      }
    }
  }
  if (!$leof) {
    # We need to stream the entire left side
    # of the join to avoid breaking the pipe in
    # a Hadoop streaming context.
    1 while read STDIN, $_, 65536;
  }
};

defshort '/j', pmap q{join_op $$_[0] || [1, 0], $$_[0] || [1, 0], $$_[1]},
               pseq popt colspec, _qfn;

# In-memory joins
# This is an alternative to the usual build-a-dataclosure approach in perl.

defoperator memory_join =>
q{
  my ($col, $n, $f) = @_;
  my $default = "\t" x $n;
  my %lookup;
  my $fh = sni @$f;
  chomp, /^([^\t]+)\t(.*)/ and $lookup{$1} = $2 while <$fh>;
  close $fh;
  $fh->await;

  while (<STDIN>)
  {
    chomp;
    my $f = (split /\t/, $_, $col + 2)[$col];
    print exists $lookup{$f}
      ? "$_\t$lookup{$f}\n"
      : "$_$default\n";
  }
};

defshort '/J', pmap q{memory_join_op $$_[0] || 0, $$_[1] || 1, $$_[2]},
               pseq popt colspec1, popt integer, _qfn;
