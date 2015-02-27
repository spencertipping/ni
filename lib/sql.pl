NI_MODULE sql

our %sql_databases;

sub defsqldb {
  my ($name, $prefix, $io_fn) = @_;
  $sql_databases{$prefix} = {name => $name, io => $io_fn};
}

our %sql_shorthands = (
  '%\*' => 'select * from',
  '%c'  => 'select count(1) from',
  '%d'  => 'select distinct',
  '%g'  => 'group by',
  '%j'  => 'inner join',
  '%l'  => 'outer left join',
  '%r'  => 'outer right join',
  '%w'  => 'where',
);

sub expand_sql_shorthands {
  my ($sql) = @_;
  $sql =~ s/$_/" $sql_shorthands{$_} "/eg for keys %sql_shorthands;
  $sql;
}

defdata 'sql',
  sub { $_[0] =~ s/^sql:// },
  sub {
    my ($prefix, $db, $x) = $_[0] =~ /^(.)([^\/]*)\/(.*)$/;
    die "invalid sql: syntax: sql:$_[0]"
      if grep !defined, $prefix, $db, $x;
    die "unknown sql db prefix: $prefix" unless exists $sql_databases{$prefix};
    $sql_databases{$prefix}{io}->($db, $x);
  };

sub transpose_tsv_lines {
  my $n       = max map scalar(split /\t/), @_;
  my @columns = map [], 1 .. $n;
  for (@_) {
    my @vs = split /\t/;
    push $columns[$_], $vs[$_] for 0 .. $#vs;
  }
  @columns;
}

sub infer_column_type {
  # Try to figure out the right type for a column based on some values for it.
  # The possibilities are 'text', 'integer', or 'real'; this is roughly the set
  # of stuff supported by both postgres and sqlite.
  return 'integer' unless grep length && !/^-?[0-9]+$/, @_;
  return 'real'    unless grep length &&
                               !/^-?[0-9]+$
                               | ^-?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?$
                               | ^-?[0-9]*   \.[0-9]+  (?:[eE][-+]?[0-9]+)?$/x,
                               @_;
  return 'text';
}

sub inferred_table_schema {
  my @types = map infer_column_type(@$_), transpose_tsv_lines @_;
  join ', ', map sprintf("f%d %s", $_, $types[$_]), 0 .. $#types;
}

sub sqlite_table_reader_io {
  my ($db, $table) = @_;
  ni_process shell_quote('sqlite3', $db),
             ni_memory qq{.mode tabs\nselect * from $table;};
}

sub create_index_statement {
  my ($should_index, $table, $schema) = @_;
  return '' unless $should_index;
  my $column = $schema =~ s/\s.*$//r;
  "CREATE INDEX $table$column ON $table($column);\n";
}

sub sqlite_table_writer_io {
  my ($db, $table, $schema) = @_;
  my $index_first_field = $table =~ s/^\+//;

  my $result = ni_pipe();

  unless (fork) {
    $result->close_writer;
    unless (defined $schema) {
      my ($first_rows, $rest) = $result->peek(20);
      $schema = inferred_table_schema @$first_rows;
      $result = ni_fifo() <= $first_rows + $rest;
    }

    my $index_statement = create_index_statement($index_first_field,
                                                 $table,
                                                 $schema);

    ni_process(shell_quote('sqlite3', $db),
               ni_memory(qq{.mode tabs
                            DROP TABLE IF EXISTS $table;
                            CREATE TABLE $table ($schema);
                            .import $result $table
                            $index_statement})) > \*STDERR;
    exit;
  }

  $result;
}

defsqldb 'sqlite3', 's',
  sub {
    my ($db, $x) = @_;
    $db = "/tmp/ni-$ENV{USER}-sqlite.db" unless length $db;

    if ($x =~ /^\S+$/) {
      # Not a query since queries require whitespace. Construct an IO that
      # reads and writes the given table, inferring a schema if the table
      # doesn't already exist.
      ni_file "sql:s$db/$x",
        sub {sqlite_table_reader_io($db, $x)->close_writer->reader_fh},
        sub {sqlite_table_writer_io($db, $x)->close_reader->writer_fh};
    } else {
      # Probably a query; apply SQL expansions and run it.
      my $query = expand_sql_shorthands $x;
      ni_process shell_quote('sqlite3', $db),
                 ni_memory(qq{.mode tabs\n$query;\n});
    }
  };

NI_MODULE_END
