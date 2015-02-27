use File::Temp qw/tmpnam/;

defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * $_[1] };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % $_[1] };

defop 'transform', 'M', 's',
  'transforms the stream as an object using the specified function',
  sub { compile($_[1])->($_[0]) };

defop 'deref', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni_cat($_[0] * \&ni) };

defop 'ref', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my ($self, $f) = @_;
        $self > ni($f //= "file:" . tmpnam);
        ni_memory($f) };

defop 'iterate', undef, 'ss',
  '(x, f): generates x, f(x), f(f(x)), f(f(f(x))), ...',
  sub { $_[0] + ni_iterate($_[1], $_[2]) };
