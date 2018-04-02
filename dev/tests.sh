#!/bin/bash
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
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=84
lazytest_case 'ni n3 ^[n05 fAA]
' 3<<'LAZYTEST_EOF'
0	0
1	1
2	2
3	3
4	4
1
2
3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=97
lazytest_case 'ni n3 +[n05 fAA]
' 3<<'LAZYTEST_EOF'
1
2
3
0	0
1	1
2	2
3	3
4	4
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=133
lazytest_case 'ni n10 =[r5 \>short] r3fAA
' 3<<'LAZYTEST_EOF'
1	1
2	2
3	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=140
lazytest_case 'ni short
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=165
lazytest_case 'ni ia ib ic w[n3p'\''a*a'\'']
' 3<<'LAZYTEST_EOF'
a	1
b	4
c	9
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=176
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' Wn
' 3<<'LAZYTEST_EOF'
1	a
2	b
3	c
4	d
5	e
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=190
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' Wn p'\''r a, uc(b)'\''
' 3<<'LAZYTEST_EOF'
1	A
2	B
3	C
4	D
5	E
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=201
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' Wn vBpuc
' 3<<'LAZYTEST_EOF'
1	A
2	B
3	C
4	D
5	E
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=222
lazytest_case 'ni i[operator could you help me] i[ place this call ] i[see the number on the matchbook]  FW Y r10
' 3<<'LAZYTEST_EOF'
0	0	operator
0	1	could
0	2	you
0	3	help
0	4	me
1	0	place
1	1	this
1	2	call
2	0	see
2	1	the
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=239
lazytest_case 'ni i[operator could you help me] i[ place this call ] i[see the number on the matchbook] FW Y r10 X 
' 3<<'LAZYTEST_EOF'
operator	could	you	help	me
place	this	call
see	the
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=249
lazytest_case 'ni 1p'\''"a".."l"'\'' Z4
' 3<<'LAZYTEST_EOF'
a	b	c	d
e	f	g	h
i	j	k	l
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=287
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' :letters gA- wn gA
' 3<<'LAZYTEST_EOF'
a	5
b	4
c	3
d	2
e	1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=298
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' :letters gA- wn gA +[letters]
' 3<<'LAZYTEST_EOF'
a	5
b	4
c	3
d	2
e	1
a
b
c
d
e
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=314
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' :letters gA- wn gA +[letters] wn
' 3<<'LAZYTEST_EOF'
a	5	1
b	4	2
c	3	3
d	2	4
e	1	5
a	6
b	7
c	8
d	9
e	10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=331
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' :letters gA- wn gA +[letters] wn gABn
' 3<<'LAZYTEST_EOF'
a	5	1
a	6
b	4	2
b	7
c	3	3
c	8
d	2	4
d	9
e	1	5
e	10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=400
lazytest_case 'ni nE4 fAAA ,aA ,sB ,dC r~1
' 3<<'LAZYTEST_EOF'
5000.5	50005000	1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=425
lazytest_case 'ni n3p'\''r map a*$_, 1..3'\'' N'\''x = x + 1'\''
' 3<<'LAZYTEST_EOF'
2	3	4
3	5	7
4	7	10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=432
lazytest_case 'ni n5p'\''r map a . $_, 1..3'\'' N'\''x = x.T'\''
' 3<<'LAZYTEST_EOF'
11	21	31	41	51
12	22	32	42	52
13	23	33	43	53
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=447
lazytest_case 'ni i[1 0] i[1 1] N'\''x = dot(x, x.T)'\''
' 3<<'LAZYTEST_EOF'
1	1
1	2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=494
lazytest_case 'ni n4m'\''r a, ai + 1'\''
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=505
lazytest_case 'ni i[operator could you help me] i[ place this call ] i[see the number on the matchbook] FWr2m'\''r fields[0..3]'\''
' 3<<'LAZYTEST_EOF'
operator	could	you	help
place	this	call
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=521
lazytest_case 'ni n4fAA l"(r (sr ('\''+ a) ('\''* b)))"
' 3<<'LAZYTEST_EOF'
10	24
LAZYTEST_EOF
lazytest_end
