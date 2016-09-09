# Extending ni
You can extend ni by writing a library. For example, suppose we want a new
operator `N` that counts lines by shelling out to `wc -l`:

```bash
$ mkdir my-library
$ echo my-lib.pl > my-library/lib
$ cat > my-library/my-lib.pl <<'EOF'
defoperator count_lines => q{exec 'wc', '-l'};
defshort '/N', pmap q{count_lines_op}, pnone;
EOF
$ ni --lib my-library n100N
100
```

Most ni extensions are about defining a new operator, which involves extending
ni's command-line grammar.
