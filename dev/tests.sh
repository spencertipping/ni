cd /tmp
export NI_NO_MONITOR=yes
#!/bin/bash
lazytest_n=0
lazytest_fail=0
lazytest_case() {
  echo -ne '\r\033[J'
  echo -n "$1" | tr '\n' ' ' | head -c79
  echo -ne '\r'

# local actual="$(eval "$1"; echo "[exit code $?]")"
# local expected="$(cat <&3; echo "[exit code $?]")"
  local actual="$(eval "$1")"
  local expected="$(cat <&3)"

  lazytest_n=$((lazytest_n + 1))
  if [[ "$actual" = "$expected" ]]; then
    return 0
  else
    lazytest_fail=$((lazytest_fail + 1))
    echo -e "\033[J\033[1;31mFAIL\033[0;0m $*"
    echo -e "\033[1;31m$lazytest_file:$lazytest_line\033[0;0m"
    echo -e "EXPECTED\033[1;34m"
    echo    "$expected"
    echo
    echo -e "\033[0;0mACTUAL\033[1;34m"
    echo    "$actual"
    echo -e "\033[0;0m"
    return 1
  fi
}
lazytest_end() {
  if ((lazytest_fail)); then
    echo -e "\r\033[J\033[1;31m$lazytest_n tests, $lazytest_fail failed\033[0;0m"
    exit 1
  else
    echo -e "\r\033[J\033[1;32m$lazytest_n tests run, all passed\033[0;0m"
    exit 0
  fi
}
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/c.md'
lazytest_line=11
lazytest_case 'cat > wcl.pl <<'\''EOF'\''
# Defines the "wcl" operator, which works like "wc -l"
defoperator wcl => q{
  exec_c99 indent(q{
    #include <unistd.h>
    #include <stdio.h>
    int main(int argc, char **argv)
    {
      char buf[8192];
      ssize_t got = 0;
      long lines = 0;
      unlink(argv[0]);
      while (got = read(0, buf, sizeof(buf)))
        while (--got)
          lines += buf[got] == '\''\n'\'';
      printf("%ld\n", lines);
      return 0;
    }
  }, -4);
};

defshort '\''/wcl'\'' => pmap q{wcl_op}, pnone;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/c.md'
lazytest_line=39
lazytest_case 'ni --lib wcl.pl n10 wcl
' 3<<'LAZYTEST_EOF'
10
LAZYTEST_EOF
lazytest_end
