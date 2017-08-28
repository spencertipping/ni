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
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=48
lazytest_case 'ni n5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=61
lazytest_case 'ni n03
' 3<<'LAZYTEST_EOF'
0
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=74
lazytest_case 'ni ihello
' 3<<'LAZYTEST_EOF'
hello
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=81
lazytest_case 'ni i"hello there"
' 3<<'LAZYTEST_EOF'
hello there
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=88
lazytest_case 'ni i[hello there new friend]
' 3<<'LAZYTEST_EOF'
hello	there	new	friend
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=97
lazytest_case 'ni n500 e'\''grep 22'\''
' 3<<'LAZYTEST_EOF'
22
122
220
221
222
223
224
225
226
227
228
229
322
422
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=159
lazytest_case 'ni n5 \>five.txt
' 3<<'LAZYTEST_EOF'
five.txt
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=170
lazytest_case 'ni n5 \>five.txt \<
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=231
lazytest_case 'ni n10 z \>ten.gz
' 3<<'LAZYTEST_EOF'
ten.gz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=258
lazytest_case 'ni n10 z \>ten.gz \<
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
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=287
lazytest_case 'ni n10 r3
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=295
lazytest_case 'ni n10 r-3
' 3<<'LAZYTEST_EOF'
4
5
6
7
8
9
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=308
lazytest_case 'ni n10 r~3
' 3<<'LAZYTEST_EOF'
8
9
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=315
lazytest_case 'ni n10 r+3
' 3<<'LAZYTEST_EOF'
8
9
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=324
lazytest_case 'ni n10 rx3
' 3<<'LAZYTEST_EOF'
1
4
7
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=334
lazytest_case 'ni n20 r.15
' 3<<'LAZYTEST_EOF'
1
9
11
12
14
15
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=350
lazytest_case 'ni n500 r/22/
' 3<<'LAZYTEST_EOF'
22
122
220
221
222
223
224
225
226
227
228
229
322
422
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=370
lazytest_case 'ni n1000 r-500 r'\''/^(\d)\1+$/'\''
' 3<<'LAZYTEST_EOF'
555
666
777
888
999
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=383
lazytest_case 'ni i[one_column] i[two columns] i[three columns here]
' 3<<'LAZYTEST_EOF'
one_column
two	columns
three	columns	here
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=392
lazytest_case 'ni i[one_column] i[two columns] i[three columns here] rB
' 3<<'LAZYTEST_EOF'
two	columns
three	columns	here
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=402
lazytest_case 'ni i[one_column] i[two columns] i[three columns here] \
     riA[ione_column ithree]
' 3<<'LAZYTEST_EOF'
one_column
three	columns	here
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=414
lazytest_case 'ni ibubbles ibaubles ibarbaras F/[aeiou]+/
' 3<<'LAZYTEST_EOF'
b	bbl	s
b	bl	s
b	rb	r	s
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=426
lazytest_case 'ni i~/bin/dependency/nightmare.jar FD
' 3<<'LAZYTEST_EOF'
~	bin	dependency	nightmare.jar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=433
lazytest_case 'ni i"here               is   an              example" FS
' 3<<'LAZYTEST_EOF'
here	is	an	example
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=440
lazytest_case 'ni ibread,eggs,milk i'\''fruit gushers,index cards'\'' FC
' 3<<'LAZYTEST_EOF'
bread	eggs	milk
fruit gushers	index cards
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=448
lazytest_case 'ni i'\''"hello,there",one,two,three'\'' FV
' 3<<'LAZYTEST_EOF'
hello,there	one	two	three
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=455
lazytest_case 'ni i'\''this@#$$gets&(*&^split'\'' FW
' 3<<'LAZYTEST_EOF'
this	gets	split
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=462
lazytest_case 'ni i'\''need|quotes|around|pipes|because|of|bash'\'' FP
' 3<<'LAZYTEST_EOF'
need	quotes	around	pipes	because	of	bash
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=469
lazytest_case 'ni ibubbles ibaubles ibarbaras F:a
' 3<<'LAZYTEST_EOF'
bubbles
b	ubles
b	rb	r	s
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=482
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" i"and I feel all right" FS
' 3<<'LAZYTEST_EOF'
this	is	how	we	do	it
it's	friday	night
and	I	feel	all	right
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=491
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" \
     i"and I feel all right" FS fC
' 3<<'LAZYTEST_EOF'
how
night
feel
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=501
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" \
     i"and I feel all right" FS fAB
' 3<<'LAZYTEST_EOF'
this	is
it's	friday
and	I
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=511
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" \
     i"and I feel all right" FS fAAC
' 3<<'LAZYTEST_EOF'
this	this	how
it's	it's	night
and	and	feel
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=521
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" \
     i"and I feel all right" FS fAD.
' 3<<'LAZYTEST_EOF'
this	we	do	it
it's	
and	all	right
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=530
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" \
     i"and I feel all right" FS fB-E
' 3<<'LAZYTEST_EOF'
is	how	we	do
friday	night
I	feel	all	right
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=540
lazytest_case 'ni i"this is how we do it" i"it'\''s friday night" \
     i"and I feel all right" FS fCBAD
' 3<<'LAZYTEST_EOF'
how	is	this	we
night	friday	it's	
feel	I	and	all
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=556
lazytest_case 'ni i"Ain'\''t nobody dope as me" \
     i"I'\''m dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS
' 3<<'LAZYTEST_EOF'
Ain't	nobody	dope	as	me
I'm	dressed	so	fresh,	so	clean
So	fresh	and	so	clean,	clean
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=567
lazytest_case 'ni i"Ain'\''t nobody dope as me" \
     i"I'\''m dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS x
' 3<<'LAZYTEST_EOF'
nobody	Ain't	dope	as	me
dressed	I'm	so	fresh,	so	clean
fresh	So	and	so	clean,	clean
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=578
lazytest_case 'ni i"Ain'\''t nobody dope as me" \
     i"I'\''m dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS xD
' 3<<'LAZYTEST_EOF'
as	nobody	dope	Ain't	me
fresh,	dressed	so	I'm	so	clean
so	fresh	and	So	clean,	clean
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=589
lazytest_case 'ni i"Ain'\''t nobody dope as me" \
     i"I'\''m dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS xEB
' 3<<'LAZYTEST_EOF'
me	nobody	dope	as	Ain't
so	dressed	so	fresh,	I'm	clean
clean,	fresh	and	so	So	clean
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=606
lazytest_case 'ni ib ia ic g
' 3<<'LAZYTEST_EOF'
a
b
c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=615
lazytest_case 'ni ib ia ic gA-
' 3<<'LAZYTEST_EOF'
c
b
a
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=624
lazytest_case 'ni i10 i5 i0.3 gAn
' 3<<'LAZYTEST_EOF'
0.3
5
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=634
lazytest_case 'ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] gABn
' 3<<'LAZYTEST_EOF'
a	0
a	1
a	2
b	3
b	6
c	4
c	5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=647
lazytest_case 'ni i[b 0] i[b 4] i[a 2] i[a 1] i[c 4] i[c 0] i[a 0] gBnA
' 3<<'LAZYTEST_EOF'
a	0
b	0
c	0
a	1
a	2
b	4
c	4
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=662
lazytest_case 'ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] oB
' 3<<'LAZYTEST_EOF'
a	0
a	1
a	2
b	3
c	4
c	5
b	6
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=673
lazytest_case 'ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] OB
' 3<<'LAZYTEST_EOF'
b	6
c	5
c	4
b	3
a	2
a	1
a	0
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=688
lazytest_case 'ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] fAgu
' 3<<'LAZYTEST_EOF'
a
b
c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=698
lazytest_case 'ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] fAgc
' 3<<'LAZYTEST_EOF'
3	a
2	b
2	c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=714
lazytest_case 'ni i[b ba bar] i[b bi bif] i[b ba baz] \
     i[q qa qat] i[q qu quux] i[b ba bake] \
     i[u ub uber] gA \>tmp \<
' 3<<'LAZYTEST_EOF'
b	ba	bake
b	ba	bar
b	ba	baz
b	bi	bif
q	qa	qat
q	qu	quux
u	ub	uber
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=729
lazytest_case 'ni i[b ba bar] i[b bi bif] i[b ba baz] \
     i[q qa qat] i[q qu quux] i[b ba bake] \
     i[u ub uber] gA \>tmp \< gB-
' 3<<'LAZYTEST_EOF'
u	ub	uber
q	qu	quux
q	qa	qat
b	bi	bif
b	ba	bake
b	ba	bar
b	ba	baz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=744
lazytest_case 'ni i[b ba bar] i[b bi bif] i[b ba baz] \
     i[q qa qat] i[q qu quux] i[b ba bake] \
     i[u ub uber] gA \>tmp \< ggAB-
' 3<<'LAZYTEST_EOF'
b	bi	bif
b	ba	bake
b	ba	bar
b	ba	baz
q	qu	quux
q	qa	qat
u	ub	uber
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=782
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''a()'\''
' 3<<'LAZYTEST_EOF'
first
foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=788
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''c'\''
' 3<<'LAZYTEST_EOF'
third
baz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=796
lazytest_case 'ni i[3 5 7] p'\''a + b + c'\''
' 3<<'LAZYTEST_EOF'
15
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=801
lazytest_case 'ni i[easy speak] p'\''b . a'\''
' 3<<'LAZYTEST_EOF'
speakeasy
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=812
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''c, a'\''
' 3<<'LAZYTEST_EOF'
third
first
baz
foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=828
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''r(c, a)'\''
' 3<<'LAZYTEST_EOF'
third	first
baz	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=836
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''r c, a'\''
' 3<<'LAZYTEST_EOF'
third	first
baz	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=848
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''k'\''
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=852
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''r k'\''
' 3<<'LAZYTEST_EOF'


LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=870
lazytest_case 'ni n3 rp'\''a'\''
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=879
lazytest_case 'ni n03 rp'\''a'\''
' 3<<'LAZYTEST_EOF'
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=887
lazytest_case 'ni n03 rp'\''r a'\''
' 3<<'LAZYTEST_EOF'
0
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=899
lazytest_case 'ni n03 rp'\''r b'\''
' 3<<'LAZYTEST_EOF'



LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=917
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r F_(1..3)'\''
' 3<<'LAZYTEST_EOF'
second	third	fourth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=924
lazytest_case 'ni i[first second third fourth fifth sixth] \
     i[only two_fields ] p'\''r FM'\''
' 3<<'LAZYTEST_EOF'
5
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=933
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r F_(3..FM)'\''
' 3<<'LAZYTEST_EOF'
fourth	fifth	sixth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=940
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r FR 3'\''
' 3<<'LAZYTEST_EOF'
fourth	fifth	sixth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=947
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r FT 3'\''
' 3<<'LAZYTEST_EOF'
first	second	third
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=959
lazytest_case 'ni p'\''r "foo" . "bar"'\''
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=965
lazytest_case 'ni 1p'\''r "foo" . "bar"'\''
' 3<<'LAZYTEST_EOF'
foobar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=982
lazytest_case 'ni --explain n10 \>ten.txt \<
' 3<<'LAZYTEST_EOF'
["n",1,11]
["file_write","ten.txt"]
["file_read"]
LAZYTEST_EOF
lazytest_end
