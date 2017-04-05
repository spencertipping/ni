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
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=33
lazytest_case 'ni 1p'\''my $v1="5"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
15	555	5 golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=40
lazytest_case 'ni 1p'\''my $v2="4.3" * "6.7"; r $v2'\''
' 3<<'LAZYTEST_EOF'
28.81
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=47
lazytest_case 'ni 1p'\''my $v1="hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
0	hihihi	hi golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=52
lazytest_case 'ni 1p'\''my $v1="3.14hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
9.42	3.14hi3.14hi3.14hi	3.14hi golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=57
lazytest_case 'ni 1p'\''my $v1="3.1E17"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
930000000000000000	3.1E173.1E173.1E17	3.1E17 golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=97
lazytest_case 'ni n3p'\''r a, one'\''
' 3<<'LAZYTEST_EOF'
1	one
2	one
3	one
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=123
lazytest_case 'ni n3p'\''*v = sub {$_[0] x 4}; &v(a)'\''
' 3<<'LAZYTEST_EOF'
1111
2222
3333
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=134
lazytest_case 'ni 1p'\''sub yo {"hi " . $_[0]} yo a'\''
' 3<<'LAZYTEST_EOF'
hi 1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=149
lazytest_case 'ni n20p'\''/^(\d)\d*$/'\''
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
1
1
1
1
1
1
1
1
1
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=184
lazytest_case 'ni n3p'\''*v = sub {$_[0] x 4}; &v(a)'\''
' 3<<'LAZYTEST_EOF'
1111
2222
3333
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=193
lazytest_case 'ni n3p'\''^{*v = sub {$_[0] x 4}} &v(a)'\''
' 3<<'LAZYTEST_EOF'
1111
2222
3333
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=202
lazytest_case 'ni n3p'\''sub v {$_[0] x 4} &v(a)'\''
' 3<<'LAZYTEST_EOF'
1111
2222
3333
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=218
lazytest_case 'ni ::data[n5] 1p'\''a_ data'\''
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=240
lazytest_case 'ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] p'\''r acS rea'\''
' 3<<'LAZYTEST_EOF'
x	5
y	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=250
lazytest_case 'ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] p'\''r acS rea'\'' p'\''r kbv_dsc(ab_ rl(3))'\''
' 3<<'LAZYTEST_EOF'
y	x	z
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=256
lazytest_case 'ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] p'\''r acS rea'\'' p'\''r kbv_asc(ab_ rl(3))'\''
' 3<<'LAZYTEST_EOF'
z	x	y
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=275
lazytest_case 'ni n1E5p'\''sr {$_[0] + a} 0'\''
' 3<<'LAZYTEST_EOF'
5000050000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=288
lazytest_case 'ni n1E1p'\''r sr {$_[0] + a, $_[1] * a, $_[2] . a} 0, 1, ""'\''
' 3<<'LAZYTEST_EOF'
55	3628800	12345678910
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=312
lazytest_case 'ni n1000p'\''se {$_[0] + a} sub {length}, 0'\''
' 3<<'LAZYTEST_EOF'
45
4905
494550
1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=322
lazytest_case 'ni n1000p'\''r length a, se {$_[0] + a} sub {length}, 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=334
lazytest_case 'ni n1000p'\''sub len($) {length $_}; r len a, se {$_[0] + a} \&len, 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=349
lazytest_case 'ni n1000p'\''r a, length a'\'' p'\''r b, se {$_[0] + a} \&b, 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=360
lazytest_case 'ni n1000p'\''r length a, a'\'' p'\''r a, sea {$_[0] + b} 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=373
lazytest_case 'ni n100p'\''my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[3], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=383
lazytest_case 'ni n100p'\''r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=401
lazytest_case 'ni 1p'\''cart [1, 2], ["a", "b", "c"]'\''
' 3<<'LAZYTEST_EOF'
1	a
2	a
1	b
2	b
1	c
2	c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=424
lazytest_case 'ni 1p'\''cart [1, 2], ["a", "b", "c"]'\'' p'\''sum a_ re {b}'\''
' 3<<'LAZYTEST_EOF'
3
3
3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=433
lazytest_case 'ni 1p'\''cart [1, 2], ["a", "b", "c"]'\'' p'\''sum a_ reb'\''
' 3<<'LAZYTEST_EOF'
1
2
1
2
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=470
lazytest_case 'ni n3p'\''r map a*$_, 1..3'\'' N'\''x = x + 1'\''
' 3<<'LAZYTEST_EOF'
2	3	4
3	5	7
4	7	10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=477
lazytest_case 'ni n5p'\''r map a . $_, 1..3'\'' N'\''x = x.T'\''
' 3<<'LAZYTEST_EOF'
11	21	31	41	51
12	22	32	42	52
13	23	33	43	53
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=493
lazytest_case 'ni i[1 0] i[1 1] N'\''x = dot(x, x.T)'\''
' 3<<'LAZYTEST_EOF'
1	1
1	2
LAZYTEST_EOF
lazytest_end
