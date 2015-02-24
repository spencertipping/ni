{

package ni;

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

defsqldb 'sqlite3', 's',
  sub {
    my ($db, $x) = @_;
    $db = "/tmp/ni-$ENV{USER}-sqlite.db" unless length $db;

    if ($x =~ /^\S+$/) {
      # Not a query since queries require whitespace. Construct an IO that
      # reads and writes the given table, inferring a schema if the table
      # doesn't already exist.
      
    }
  };

}
