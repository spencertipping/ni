use File::Temp qw/tmpnam/;

defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * $_[1] };

defop 'flatmap', '+m', 's',
  'produces multiple output records per input',
  sub { $_[0]->flatmap($_[1]) };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % $_[1] };

defop 'transform', 'M', 's',
  'transforms the stream as an object using the specified function',
  sub { fn($_[1])->($_[0]) };

defop 'read', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni_cat($_[0] * 'ni %0') };

defop 'into', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my ($self, $f) = @_;
        $self > ni($f //= "file:" . tmpnam);
        ni_memory($f) };

defop 'iterate', undef, 'ss',
  '(x, f): generates x, f(x), f(f(x)), f(f(f(x))), ...',
  sub { $_[0] + ni_iterate($_[1], $_[2]) };

defop 'iota', 'i', 'D',
  'generates numbers from 0 to n-1',
  sub {
    my $source = ni_iterate 0, '%0 + 1';
    $_[0] + (defined $_[1] ? $source >> take_binding $_[1] : $source);
  };
