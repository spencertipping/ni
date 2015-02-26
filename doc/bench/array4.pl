while (<>) {
  (sub {
    print "$_[0]\t" . length($_[0]) . "\n";
  })->((split /\t/)[0]);
}
