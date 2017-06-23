# Bloom filter operators.
# Operators to construct and query bloom filters. The bloom constructor is a
# sub-operator of `z` (compress), and querying is done using `rb`.
#
# Notation is two digits: power of ten #elements, and power of ten
# false-positive probability. So `zB74` means "create a filter designed to
# store 10^7 (== 10M) elements with a 10^-4 likelihood of false positives."

BEGIN {
  defparseralias bloom_size_spec => pmap q{10 **  $_}, prx qr/\d/;
  defparseralias bloom_fp_spec   => pmap q{10 ** -$_}, prx qr/\d/;
}

defoperator bloomify => q{
  my ($n, $p) = @_;
  my $f = bloom_new $n, $p;
  chomp, bloom_add $f, $_ while <STDIN>;
  print $f;
};

defshort '/zB', pmap q{bloomify_op @$_}, pseq bloom_size_spec, bloom_fp_spec;

defoperator bloom_rows => q{
  my ($include_mode, $col, $bloom_lambda) = @_;
  my $bloom;
  my $r = sni @$bloom_lambda;
  1 while read $r, $bloom, 65536, length $bloom;
  $r->await;
  while (<STDIN>) {
    chomp(my @cols = split /\t/, $_, $col + 2);
    print if !$include_mode == !bloom_contains $bloom, $cols[$col];
  }
};

defrowalt pmap q{bloom_rows_op 1, @$_}, pn [1, 2], pstr 'b', colspec1, _qfn;
defrowalt pmap q{bloom_rows_op 0, @$_}, pn [1, 2], pstr 'B', colspec1, _qfn;
