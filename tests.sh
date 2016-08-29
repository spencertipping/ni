#!/bin/bash
lazytest_case() {
  local actual="$(eval "$1")"
  local expected="$(cat <&3)"
  if [[ "$actual" = "$expected" ]]; then
    echo "$1" | tr '\n' ' '; echo
    return 0
  else
    echo -e "\033[1;31mFAIL\033[0;0m $*: expected '$expected', got '$actual'"
    return 1
  fi
}
cat <<'LAZYTEST_EOF'
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
lazytest_case 'ni n:3 g                      # g = sort
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3g                       # no need for whitespace
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_case 'ni n:3gAr                     # reverse-sort by first field
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_case 'ni n:3O                       # more typical reverse numeric sort
' 3<<'LAZYTEST_EOF'
3
2
1
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
lazytest_case 'ni /etc/passwd r2F/:/
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
lazytest_case 'mkdir sqlite-profile
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'echo sqlite.pl > sqlite-profile/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'cat > sqlite-profile/sqlite.pl <<'\''EOF'\''
$sql_profiles{S} = pmap {sh "sqlite3", $$_[0], $$_[1]}
                        seq mrc '\''^.*'\'', $sql_query;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'sqlite3 test.db <<'\''EOF'\''
CREATE TABLE foo(x int, y int);
INSERT INTO foo(x, y) VALUES (1, 2);
INSERT INTO foo(x, y) VALUES (3, 4);
INSERT INTO foo(x, y) VALUES (5, 6);
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_case 'ni --lib sqlite-profile QStest.db foo [wx=3]
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
