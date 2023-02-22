# SQLite support via URI schemas

defconfenv sqlite => NI_SQLITE => 'sqlite3';

sub sqlite_tsv
{
  my ($db, $sql, @sqlite_opts) = @_;
  sh join" | ", shell_quote(conf"sqlite", "-csv", "-separator", ",",
                            @sqlite_opts, $db, $sql),
                shell_quote("perl", "-e", $ni::operators{split_proper_csv});
}


# sqlite:///path/to/file.db
# List all tables as sqlitet:// entries
defresource 'sqlite',
  read => q{
    my $db = $_[1];
    soproc { sqlite_tsv
      "file:$db?immutable=1",
      "select 'sqlitet://$db:' || name, "
           . "'sqlites://$db:' || name, "
           . "'sqliteq://$db:''select * from ' || name || '''' "
      . "from sqlite_master where type='table' order by name" };
  };

defresource 'sqlites',
  read => q{
    my ($db, $table) = $_[1] =~ /(.*):([^:]+)$/;
    soproc {
      sh shell_quote conf"sqlite", "file:$db?immutable=1",
                     "select sql from sqlite_master where name='$table'" };
  };

defresource 'sqlitet',
  read => q{
    my ($db, $table) = $_[1] =~ /(.*):([^:]+)$/;
    soproc { sqlite_tsv "file:$db?immutable=1",
                        "select * from $table",
                        "-header" };
  },
  write => q{
    my ($db, $table) = $_[1] =~ /(.*):([^:]+)$/;
    siproc { exec conf"sqlite", $db, ".mode ascii",
                                    q{.separator "\t" "\n"},
                                     ".import /dev/stdin $table" };
  };

defresource 'sqliteq',
  read => q{
    my ($db, $sql) = $_[1] =~ /(.*):([^:]+)$/;
    soproc { sqlite_tsv "file:$db?immutable=1", $sql, "-header" };
  };
