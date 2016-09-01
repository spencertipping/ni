#!/bin/bash
lazytest_n=0
lazytest_fail=0
lazytest_case() {
  echo -ne '\r\033[J'
  echo -n "$1" | tr '\n' ' ' | head -c79
  echo -ne '\r'

  local actual="$(eval "$1"; echo "[exit code $?]")"
  local expected="$(cat <&3; echo "[exit code $?]")"
  lazytest_n=$((lazytest_n + 1))
  if [[ "$actual" = "$expected" ]]; then
    return 0
  else
    lazytest_fail=$((lazytest_fail + 1))
    echo -e "\033[J\033[1;31mFAIL\033[0;0m $*"
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
  else
    echo -e "\r\033[J\033[1;32m$lazytest_n tests run, all passed\033[0;0m"
  fi
}
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni --internal/parse generic_code [foo]
' 3<<'LAZYTEST_EOF'
[foo]
LAZYTEST_EOF
lazytest_case 'ni --internal/parse generic_code [foo]]]
' 3<<'LAZYTEST_EOF'
[foo] | ]]
LAZYTEST_EOF
lazytest_case 'ni n:4l'\''(+ a 2)'\''
' 3<<'LAZYTEST_EOF'
3
4
5
6
LAZYTEST_EOF
lazytest_case 'ni n:4l'\''(r a (1+ a))'\''                   # generate two columns
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_case 'ni n:4l'\''(r a (1+ a))'\'' l'\''(r (+ a b))'\''        # ... and sum them
' 3<<'LAZYTEST_EOF'
3
5
7
9
LAZYTEST_EOF
lazytest_case 'ni n:2l'\''a (+ a 100)'\''                   # return without "r"
' 3<<'LAZYTEST_EOF'
1
101
2
102
LAZYTEST_EOF
lazytest_case 'ni n:10000l"(sr ('\''+ a))"
' 3<<'LAZYTEST_EOF'
50005000
LAZYTEST_EOF
lazytest_case 'ni n:4fAA l"(r (sr ('\''+ a) ('\''* b)))"
' 3<<'LAZYTEST_EOF'
10	24
LAZYTEST_EOF
lazytest_case 'echo test > foo
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni foo
' 3<<'LAZYTEST_EOF'
test
LAZYTEST_EOF
lazytest_case 'ni foo foo
' 3<<'LAZYTEST_EOF'
test
test
LAZYTEST_EOF
lazytest_case 'echo test | gzip > fooz
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni fooz
' 3<<'LAZYTEST_EOF'
test
LAZYTEST_EOF
lazytest_case 'cat fooz | ni
' 3<<'LAZYTEST_EOF'
test
LAZYTEST_EOF
lazytest_case 'ni $:'\''seq 4'\''                  # shell command stdout
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_case 'ni n:4                        # integer generator
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_case 'ni n0:4                       # integer generator, zero-based
' 3<<'LAZYTEST_EOF'
0
1
2
3
LAZYTEST_EOF
lazytest_case 'ni id:foo                     # literal text
' 3<<'LAZYTEST_EOF'
foo
LAZYTEST_EOF
lazytest_case 'ni n:3 | sort
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3 $=sort                 # $= filters through a command
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3 $='\''sort -r'\''
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_case 'ni n:3 g      # g = sort
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3g       # no need for whitespace
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3gAr     # reverse-sort by first field
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_case 'ni n:3O       # NOTE: capital O, not zero; more typical reverse numeric sort
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_case 'ni n:3 >file
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni file
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3 \>file2
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni file2
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3Z >file3.gz
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'zcat file3.gz
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni id:gzip Z | gzip -dc               # gzip by default
' 3<<'LAZYTEST_EOF'
gzip
LAZYTEST_EOF
lazytest_case 'ni id:gzip Zg | gzip -dc              # explicitly specify
' 3<<'LAZYTEST_EOF'
gzip
LAZYTEST_EOF
lazytest_case 'ni id:gzip Zg9 | gzip -dc             # specify compression level
' 3<<'LAZYTEST_EOF'
gzip
LAZYTEST_EOF
lazytest_case 'ni id:xz Zx | xz -dc
' 3<<'LAZYTEST_EOF'
xz
LAZYTEST_EOF
lazytest_case 'ni id:lzo Zo | lzop -dc
' 3<<'LAZYTEST_EOF'
lzo
LAZYTEST_EOF
lazytest_case 'ni id:bzip2 Zb | bzip2 -dc
' 3<<'LAZYTEST_EOF'
bzip2
LAZYTEST_EOF
lazytest_case 'ni n:4 Z ZD
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_case 'ni n:4 ZD
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_case 'ni n:4 ZN | wc -c
' 3<<'LAZYTEST_EOF'
0
LAZYTEST_EOF
lazytest_case 'ni n:1000000gr4
' 3<<'LAZYTEST_EOF'
1
10
100
1000
LAZYTEST_EOF
lazytest_case 'ni :numbers[n:1000000gr4]
' 3<<'LAZYTEST_EOF'
1
10
100
1000
LAZYTEST_EOF
lazytest_case 'ni :numbers[n:1000000gr4]O
' 3<<'LAZYTEST_EOF'
1000
100
10
1
LAZYTEST_EOF
lazytest_case 'echo '\''checkpointed'\'' > numbers
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni :numbers[n:1000000gr4]O
' 3<<'LAZYTEST_EOF'
checkpointed
LAZYTEST_EOF
lazytest_case 'ni :biglist[n:100000Z]r5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_case 'ni :biglist[n:100000Z]r5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_case 'ni n:8p'\''r map a*$_, 1..8'\'' > mult-table
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni mult-table
' 3<<'LAZYTEST_EOF'
1	2	3	4	5	6	7	8
2	4	6	8	10	12	14	16
3	6	9	12	15	18	21	24
4	8	12	16	20	24	28	32
5	10	15	20	25	30	35	40
6	12	18	24	30	36	42	48
7	14	21	28	35	42	49	56
8	16	24	32	40	48	56	64
LAZYTEST_EOF
lazytest_case 'ni mult-table fA      # the first column
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
6
7
8
LAZYTEST_EOF
lazytest_case 'ni mult-table fDC     # fourth, then third column
' 3<<'LAZYTEST_EOF'
4	3
8	6
12	9
16	12
20	15
24	18
28	21
32	24
LAZYTEST_EOF
lazytest_case 'ni mult-table fAA     # first column, duplicated
' 3<<'LAZYTEST_EOF'
1	1
2	2
3	3
4	4
5	5
6	6
7	7
8	8
LAZYTEST_EOF
lazytest_case 'ni mult-table fDA.    # fourth, first, "and the rest (i.e. 5-8)"
' 3<<'LAZYTEST_EOF'
4	1	5	6	7	8
8	2	10	12	14	16
12	3	15	18	21	24
16	4	20	24	28	32
20	5	25	30	35	40
24	6	30	36	42	48
28	7	35	42	49	56
32	8	40	48	56	64
LAZYTEST_EOF
lazytest_case 'ni mult-table fBA.    # an easy way to swap first two columns
' 3<<'LAZYTEST_EOF'
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
6	3	9	12	15	18	21	24
8	4	12	16	20	24	28	32
10	5	15	20	25	30	35	40
12	6	18	24	30	36	42	48
14	7	21	28	35	42	49	56
16	8	24	32	40	48	56	64
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd r2F::          # F: followed by :, which is the split char
' 3<<'LAZYTEST_EOF'
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
LAZYTEST_EOF
lazytest_case 'ni //ni r3                            # some data
' 3<<'LAZYTEST_EOF'
#!/usr/bin/env perl
# ni: https://github.com/spencertipping/ni
# Copyright (c) 2016 Spencer Tipping
LAZYTEST_EOF
lazytest_case 'ni //ni r3F/\\//                      # split on forward slashes
' 3<<'LAZYTEST_EOF'
#!	usr	bin	env perl
# ni: https:		github.com	spencertipping	ni
# Copyright (c) 2016 Spencer Tipping
LAZYTEST_EOF
lazytest_case 'ni //ni r3FW                          # split on non-words
' 3<<'LAZYTEST_EOF'
	usr	bin	env	perl
	ni	https	github	com	spencertipping	ni
	Copyright	c	2016	Spencer	Tipping
LAZYTEST_EOF
lazytest_case 'ni //ni r3FS                          # split on whitespace
' 3<<'LAZYTEST_EOF'
#!/usr/bin/env	perl
#	ni:	https://github.com/spencertipping/ni
#	Copyright	(c)	2016	Spencer	Tipping
LAZYTEST_EOF
lazytest_case 'ni //ni r3Fm'\''/\/\w+/'\''                 # words beginning with a slash
' 3<<'LAZYTEST_EOF'
/usr	/bin	/env
/github	/spencertipping	/ni

LAZYTEST_EOF
lazytest_case 'ni n:10r3                     # take first 3
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:10r+3                    # take last 3
' 3<<'LAZYTEST_EOF'
8
9
10
LAZYTEST_EOF
lazytest_case 'ni n:10r-7                    # drop first 7
' 3<<'LAZYTEST_EOF'
8
9
10
LAZYTEST_EOF
lazytest_case 'ni n:10000rx4000              # take every 4000th row
' 3<<'LAZYTEST_EOF'
4000
8000
LAZYTEST_EOF
lazytest_case 'ni n:10000r.0002              # sample uniformly, P(row) = 0.0002
' 3<<'LAZYTEST_EOF'
1
6823
8921
9509
LAZYTEST_EOF
lazytest_case 'ni n:10000r/[42]000$/
' 3<<'LAZYTEST_EOF'
2000
4000
LAZYTEST_EOF
lazytest_case 'ni n:1000r/[^1]$/r3
' 3<<'LAZYTEST_EOF'
2
3
4
LAZYTEST_EOF
lazytest_case 'ni n:10000rp'\''$_ % 100 == 42'\'' r3
' 3<<'LAZYTEST_EOF'
42
142
242
LAZYTEST_EOF
lazytest_case 'ni n:100n:10gr4               # g = '\''group'\''
' 3<<'LAZYTEST_EOF'
1
1
10
10
LAZYTEST_EOF
lazytest_case 'ni n:100n:100Gr4              # G = '\''group uniq'\''
' 3<<'LAZYTEST_EOF'
1
10
100
11
LAZYTEST_EOF
lazytest_case 'ni n:100or3                   # o = '\''order'\'': sort numeric ascending
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:100Or3                   # O = '\''reverse order'\''
' 3<<'LAZYTEST_EOF'
100
99
98
LAZYTEST_EOF
lazytest_case 'ni n:100p'\''r a, sin(a), log(a)'\'' > data         # generate multicolumn data
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni data r4
' 3<<'LAZYTEST_EOF'
1	0.841470984807897	0
2	0.909297426825682	0.693147180559945
3	0.141120008059867	1.09861228866811
4	-0.756802495307928	1.38629436111989
LAZYTEST_EOF
lazytest_case 'ni data oB r4
' 3<<'LAZYTEST_EOF'
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
LAZYTEST_EOF
lazytest_case 'ni data oBr r4                # r suffix = reverse sort
' 3<<'LAZYTEST_EOF'
33	0.999911860107267	3.49650756146648
77	0.999520158580731	4.34380542185368
58	0.992872648084537	4.06044301054642
14	0.99060735569487	2.63905732961526
LAZYTEST_EOF
lazytest_case 'ni //ni FWpF_ r500 > word-list
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni word-list cr10             # unsorted count
' 3<<'LAZYTEST_EOF'
1	usr
1	bin
1	env
1	perl
1	
1	ni
1	https
1	github
1	com
1	spencertipping
LAZYTEST_EOF
lazytest_case 'ni word-list Cr10             # sort first to group words
' 3<<'LAZYTEST_EOF'
41	0
8	006_000
1	1
9	2
2	2016
2	3
1	43
1	5
2	A
3	ACTION
LAZYTEST_EOF
lazytest_case 'ni @pa '\''r a, rca rsum 1'\'' <<'\''EOF'\''
foo
bar
foo
bif
EOF
' 3<<'LAZYTEST_EOF'
bar	1
bif	1
foo	2
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd F::@pg '\''r a, @{rca rarr B}'\''
' 3<<'LAZYTEST_EOF'
/bin/bash	root
/bin/false	syslog
/bin/sh	backup	bin	daemon	games	gnats	irc	libuuid	list	lp	mail	man	news	nobody	proxy	sys	uucp	www-data
/bin/sync	sync
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd F::gGp'\''r g, a_ reg'\''
' 3<<'LAZYTEST_EOF'
/bin/bash	root
/bin/false	syslog
/bin/sh	backup	bin	daemon	games	gnats	irc	libuuid	list	lp	mail	man	news	nobody	proxy	sys	uucp	www-data
/bin/sync	sync
LAZYTEST_EOF
lazytest_case 'mkdir sqlite-profile
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'echo sqlite.pl > sqlite-profile/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'cat > sqlite-profile/sqlite.pl <<'\''EOF'\''
$sql_profiles{S} = pmap {sh "sqlite", "-separator", "\t", $$_[0], $$_[1]}
                        seq mrc '\''^.*'\'', $sql_query;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'sqlite test.db <<'\''EOF'\''
CREATE TABLE foo(x int, y int);
INSERT INTO foo(x, y) VALUES (1, 2);
INSERT INTO foo(x, y) VALUES (3, 4);
INSERT INTO foo(x, y) VALUES (5, 6);
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni --lib sqlite-profile QStest.db foo[wx=3]
' 3<<'LAZYTEST_EOF'
3	4
LAZYTEST_EOF
lazytest_case 'ni --lib sqlite-profile QStest.db foo[Ox]
' 3<<'LAZYTEST_EOF'
5	6
3	4
1	2
LAZYTEST_EOF
lazytest_case 'mkdir my-library
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'echo my-lib.pl > my-library/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'echo "defshort '\''root'\'', '\''N'\'', k sh ['\''wc'\'', '\''-l'\''];" > my-library/my-lib.pl
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni --lib my-library n:100N
' 3<<'LAZYTEST_EOF'
100
LAZYTEST_EOF
lazytest_case 'ni n:5p'\''a * a'\''                # square some numbers
' 3<<'LAZYTEST_EOF'
1
4
9
16
25
LAZYTEST_EOF
lazytest_case 'ni n:4p'\''r a, a + 1'\''                   # generate two columns
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_case 'ni n:4p'\''r a, a + 1'\'' p'\''r a + b'\''        # ... and sum them
' 3<<'LAZYTEST_EOF'
3
5
7
9
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd F::r3
' 3<<'LAZYTEST_EOF'
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd F::r3p'\''r F_ 0..3'\''
' 3<<'LAZYTEST_EOF'
root	x	0	0
daemon	x	1	1
bin	x	2	2
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd F::r3p'\''r scalar F_'\''            # number of fields
' 3<<'LAZYTEST_EOF'
7
7
7
LAZYTEST_EOF
lazytest_case 'ni n:2p'\''a, a + 100'\''                   # return without "r"
' 3<<'LAZYTEST_EOF'
1
101
2
102
LAZYTEST_EOF
lazytest_case 'ni n:2p'\''r a, a + 100'\''                 # use "r" for side effect, return ()
' 3<<'LAZYTEST_EOF'
1	101
2	102
LAZYTEST_EOF
lazytest_case 'ni n:3p'\''r $_ for 1..a; ()'\''            # use r imperatively, explicit return
' 3<<'LAZYTEST_EOF'
1
1
2
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3p'\''r $_ for 1..a'\''                # use r imperatively, implicit return
' 3<<'LAZYTEST_EOF'
1

1
2

1
2
3

LAZYTEST_EOF
lazytest_case 'ni n:10p'\''r ru {a%4 == 0}'\''             # read forward until a multiple of 4
' 3<<'LAZYTEST_EOF'
1	2	3
4	5	6	7
8	9	10
LAZYTEST_EOF
lazytest_case 'ni n:10p'\''r map a*$_, 1..10'\'' | tee mult-table
' 3<<'LAZYTEST_EOF'
1	2	3	4	5	6	7	8	9	10
2	4	6	8	10	12	14	16	18	20
3	6	9	12	15	18	21	24	27	30
4	8	12	16	20	24	28	32	36	40
5	10	15	20	25	30	35	40	45	50
6	12	18	24	30	36	42	48	54	60
7	14	21	28	35	42	49	56	63	70
8	16	24	32	40	48	56	64	72	80
9	18	27	36	45	54	63	72	81	90
10	20	30	40	50	60	70	80	90	100
LAZYTEST_EOF
lazytest_case 'ni mult-table p'\''r g_ ru {a%4 == 0}'\''   # extract seventh column from each line
' 3<<'LAZYTEST_EOF'
7	14	21
28	35	42	49
56	63	70
LAZYTEST_EOF
lazytest_case 'ni n:100p'\''sum rw {1}'\''
' 3<<'LAZYTEST_EOF'
5050
LAZYTEST_EOF
lazytest_case 'ni n:10p'\''prod rw {1}'\''
' 3<<'LAZYTEST_EOF'
3628800
LAZYTEST_EOF
lazytest_case 'ni n:100p'\''mean rw {1}'\''
' 3<<'LAZYTEST_EOF'
50.5
LAZYTEST_EOF
lazytest_case 'ni n:10000p'\''sr {$_[0] + a} 0'\''
' 3<<'LAZYTEST_EOF'
50005000
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd F::gGp'\''r g, se {"$_[0]," . a} \&g, ""'\''
' 3<<'LAZYTEST_EOF'
/bin/bash	,root
/bin/false	,syslog
/bin/sh	,backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	,sync
LAZYTEST_EOF
lazytest_case 'ni n:100p'\''my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[2], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_case 'ni n:100p'\''r rc \&sr, rsum A, rmean A, rmin A, rmax A'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_case 'ni /etc/passwd FWpsplit// r/[a-z]/ \
     p'\''my %freqs = %{rc \&sr, rfn q{ ++${%1}{a()} && %1 }, {}};
       map r($_, $freqs{$_}), sort keys %freqs'\''
' 3<<'LAZYTEST_EOF'
a	39
b	36
c	14
d	13
e	17
f	1
g	11
h	20
i	46
k	3
l	19
m	14
n	50
o	25
p	15
r	24
s	51
t	15
u	17
v	12
w	12
x	23
y	12
LAZYTEST_EOF
lazytest_end
