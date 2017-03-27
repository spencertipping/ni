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
lazytest_file='doc/row.md'
lazytest_line=19
lazytest_case 'ni n10r3                      # take first 3
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=23
lazytest_case 'ni n10r~3                     # take last 3
' 3<<'LAZYTEST_EOF'
8
9
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=27
lazytest_case 'ni n10r-7                     # drop first 7
' 3<<'LAZYTEST_EOF'
8
9
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=39
lazytest_case 'ni n10000rx4000               # take every 4000th row
' 3<<'LAZYTEST_EOF'
4000
8000
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=42
lazytest_case 'ni n10000r.0002               # sample uniformly, P(row) = 0.0002
' 3<<'LAZYTEST_EOF'
1
6823
8921
9509
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=70
lazytest_case 'ni n10000r/[42]000$/
' 3<<'LAZYTEST_EOF'
2000
4000
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=73
lazytest_case 'ni n1000r/[^1]$/r3
' 3<<'LAZYTEST_EOF'
2
3
4
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=88
lazytest_case 'ni n10000rp'\''$_ % 100 == 42'\'' r3
' 3<<'LAZYTEST_EOF'
42
142
242
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=107
lazytest_case 'ni n1000p'\''r/(.)(.*)/'\'' r15     # until 10, the second field is empty
' 3<<'LAZYTEST_EOF'
1	
2	
3	
4	
5	
6	
7	
8	
9	
1	0
1	1
1	2
1	3
1	4
1	5
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=123
lazytest_case 'ni n1000p'\''r/(.)(.*)/'\'' rB r8   # rB = "rows for which field B exists"
' 3<<'LAZYTEST_EOF'
1	0
1	1
1	2
1	3
1	4
1	5
1	6
1	7
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=135
lazytest_case 'ni n10rB | wc -l              # no field B here, so no output
' 3<<'LAZYTEST_EOF'
0
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=140
lazytest_case 'ni n10p'\''r a; ""'\'' rA | wc -l   # remove blank lines
' 3<<'LAZYTEST_EOF'
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=149
lazytest_case 'ni n100n10gr4                 # g = '\''group'\''
' 3<<'LAZYTEST_EOF'
1
1
10
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=154
lazytest_case 'ni n100n100gur4               # u = '\''uniq'\''
' 3<<'LAZYTEST_EOF'
1
10
100
11
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=168
lazytest_case 'ni n100or3                    # o = '\''order'\'': sort numeric ascending
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=172
lazytest_case 'ni n100Or3                    # O = '\''reverse order'\''
' 3<<'LAZYTEST_EOF'
100
99
98
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=185
lazytest_case 'ni n100p'\''r a, sin(a), log(a)'\'' > data          # generate multicolumn data
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=186
lazytest_case 'ni data r4
' 3<<'LAZYTEST_EOF'
1	0.841470984807897	0
2	0.909297426825682	0.693147180559945
3	0.141120008059867	1.09861228866811
4	-0.756802495307928	1.38629436111989
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=197
lazytest_case 'ni data oBr4
' 3<<'LAZYTEST_EOF'
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=209
lazytest_case 'ni data oBg r4                # '\''g'\'' is a modifier of B, not another sort
' 3<<'LAZYTEST_EOF'
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=214
lazytest_case 'ni data oB g r4               # '\''g'\'' is a sorting operator
' 3<<'LAZYTEST_EOF'
1	0.841470984807897	0
10	-0.54402111088937	2.30258509299405
100	-0.506365641109759	4.60517018598809
11	-0.999990206550703	2.39789527279837
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=226
lazytest_case 'ni i{foo,bar,bif,baz,quux,uber,bake} p'\''r length, a'\'' ggAB
' 3<<'LAZYTEST_EOF'
3	bar
3	baz
3	bif
3	foo
4	bake
4	quux
4	uber
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=240
lazytest_case 'ni i{foo,bar,bif,baz,quux,uber,bake} p'\''r length, a'\'' ggAB-
' 3<<'LAZYTEST_EOF'
3	foo
3	bif
3	baz
3	bar
4	uber
4	quux
4	bake
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=255
lazytest_case 'ni //license FWpF_ > word-list
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=256
lazytest_case 'ni word-list cr10             # unsorted count
' 3<<'LAZYTEST_EOF'
1	ni
1	https
1	github
1	com
1	spencertipping
1	ni
1	Copyright
1	c
1	2016
1	Spencer
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=267
lazytest_case 'ni word-list gcr10            # sort first to group words
' 3<<'LAZYTEST_EOF'
1	2016
1	A
1	ACTION
1	AN
1	AND
2	ANY
1	ARISING
1	AS
1	AUTHORS
1	BE
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=278
lazytest_case 'ni word-list gcOr10           # by descending count
' 3<<'LAZYTEST_EOF'
7	to
7	the
7	OR
6	THE
5	Software
4	of
4	and
4	OF
4	IN
3	SOFTWARE
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=296
lazytest_case 'ni word-list p'\''r a, length a'\'' > word-lengths
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=297
lazytest_case 'ni word-list gj[word-lengths g] r10
' 3<<'LAZYTEST_EOF'
2016	2016	4
A	A	1
ACTION	ACTION	6
AN	AN	2
AND	AND	3
ANY	ANY	3
ANY	ANY	3
ARISING	ARISING	7
AS	AS	2
AUTHORS	AUTHORS	7
LAZYTEST_EOF
lazytest_end
