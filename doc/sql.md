# SQL interop
ni defines a parsing context that translates command-line syntax into SQL
queries. We'll need to define a SQL connection profile in order to use it:

```bash
$ mkdir sqlite-profile
$ echo sqlite.pl > sqlite-profile/lib
$ cat > sqlite-profile/sqlite.pl <<'EOF'
defoperator sqlite => q{
  my ($db, $query) = @_;
  exec 'sqlite', '-separator', "\t", $db, $query;
};
defsqlprofile S => pmap q{sqlite_op $$_[0], $$_[1]},
                        pseq pc filename, sql_query;
EOF
```

Now we can create a test database and use this library to access it.

```bash
$ sqlite test.db <<'EOF'
CREATE TABLE foo(x int, y int);
INSERT INTO foo(x, y) VALUES (1, 2);
INSERT INTO foo(x, y) VALUES (3, 4);
INSERT INTO foo(x, y) VALUES (5, 6);
EOF
$ ni --lib sqlite-profile QStest.db foo[rx=3]
3	4
$ ni --lib sqlite-profile QStest.db foo rx=3
3	4
$ ni --lib sqlite-profile QStest.db foo Ox
5	6
3	4
1	2
```
