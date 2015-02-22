# Operator implementations

use File::Temp qw/tmpnam/;

# Meta
defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni_const(self) };

defop 'explain', undef, '',
  'explains the current pipeline to stderr',
  sub { print STDERR $_[0]->quoted_into('print $_;', {}); $_[0] };

# Functional transforms
defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * record_transformer $_[1] };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % record_transformer $_[1] };

defop 'deref', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni::io::sum->new($_[0] * \&::ni) };

defop 'ref', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my $f = $_[1] // tmpnam;
        $_[0]->into(ni $_[1]);
        ni::io::array->new($f) };

defop 'branch', 'b', 's',
  'splits input by its first field, forwarding to subprocesses',
  sub {
    my ($in, $subprocesses) = @_;
    my @subs = unpack_branch_map $subprocesses;
    my $fifo = ni::io::fifo->new->from(map ${$_}[1], @subs);

    unless (fork) {
      my $line;
      while (defined($line = <$in>)) {
        my ($k, $v) = split /\t/, $line, 2;
        for my $s (@subs) {
          if ($s->[0]->($k)) {
            $s->[1]->enqueue($line);
            last;
          }
        }
      }
      exit;
    }
    $fifo;
  };

# Sorting (shells out to command-line sort)
sub sort_options {
  my @fieldspec = split //, $_[0] // '';
  # TODO
}

defop 'order', 'o', 'AD',
  'order {n|N|g|G|l|L|u|U|m} [fields]',
  sub {
    my ($in, $flags, $fields) = @_;
    $in | 'sort';
  };
