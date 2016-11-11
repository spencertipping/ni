# Scripting interface
It's common to have a shell script you'd like to integrate into a ni pipeline,
and possibly also into the ni image itself. This allows you to do something
more graceful than `ni e[sh my-script args...]`.

ni's scripting interface makes this possible. All you need to do is write a
normal library and include it into the ni image. For example, here's a
scripting extension that echoes its command-line arguments:

```bash
$ mkdir echo-script
$ echo echo.pl >> echo-script/lib
$ echo echo.sh >> echo-script/lib
$ cat > echo-script/echo.sh <<'EOF'
#!/bin/sh
echo "$@"
EOF
$ cat > echo-script/echo.pl <<'EOF'
defshort '/echo' =>
  pmap q{script_op 'echo-script', "sh ./echo.sh " . $_},
  shell_command;
EOF
```

Now let's use the library:

```bash
$ ni --lib echo-script echo[1 2 3]
1 2 3
```
