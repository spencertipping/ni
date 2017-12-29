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
lazytest_file='bugs/2016.0918.replicated-garbage.md'
lazytest_line=653
lazytest_case 'ni //ni --dev/local-operate[ \
     p'\''/(^\d+ )(.*)/ ? map r($2, $_), rl $1 : ()'\'' ,zA r-1 r10]
' 3<<'LAZYTEST_EOF'
0	# Resource layout map.
0	# ni is assembled by following the instructions here. This script is also
0	# included in the ni image itself so it can rebuild accordingly.
0	
0	bootcode
0	resource core/boot/ni.map
0	
0	resource core/boot/util.pl
0	resource core/boot/doc.pl
0	resource core/boot/dev.pl
LAZYTEST_EOF
lazytest_file='bugs/2017.0407.hash-constructors.md'
lazytest_line=6
lazytest_case 'ni i[a 1 b] i[a 2] i[a 3 c] p'\''@lines = rea; %h = cb_ @lines; @sorted_keys = sort keys %h;  r($_, $h{$_}) for @sorted_keys'\''
' 3<<'LAZYTEST_EOF'
	2
b	1
c	3
LAZYTEST_EOF
lazytest_file='doc/binary.md'
lazytest_line=12
lazytest_case 'ni n1p'\''wp "A4VA8VvvVVvvA4V",
         qw|RIFF 176436 WAVEfmt 16 1 2 44100 176400 4 16 data 176400|'\'' \
     +n44100p'\''my $v = 32767 * sin a*440*tau/44100;
              wp "ss", $v, $v'\'' \
  > test.wav
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/binary.md'
lazytest_line=35
lazytest_case 'ni test.wav bp'\''rp "A4VA8VvvVVvvA4V" if bi == 0;       # skip the header
                 r rp"ss"'\'' r10
' 3<<'LAZYTEST_EOF'
2052	2052
4097	4097
6126	6126
8130	8130
10103	10103
12036	12036
13921	13921
15752	15752
17521	17521
19222	19222
LAZYTEST_EOF
lazytest_file='doc/binary.md'
lazytest_line=76
lazytest_case 'ni test.wav bp'\''bi?r rp "ss":rb 44'\'' fA N'\''x = fft.fft(x, axis=0).real'\'' \
     Wn rp'\''a <= 22050'\'' OB r5,qB.01
' 3<<'LAZYTEST_EOF'
441	45263289.95
14941	755.22
7341	745.63
8461	667.75
12181	620.78
LAZYTEST_EOF
lazytest_file='doc/bloom.md'
lazytest_line=32
lazytest_case 'ni nE4 rbA[i108 i571 i3491 zB45]
' 3<<'LAZYTEST_EOF'
108
571
3491
LAZYTEST_EOF
lazytest_file='doc/bloom.md'
lazytest_line=36
lazytest_case 'ni nE4 r^bA[nE4 rp'\''a != 61 && a != 108'\'' zB45]
' 3<<'LAZYTEST_EOF'
61
108
LAZYTEST_EOF
lazytest_file='doc/bloom.md'
lazytest_line=44
lazytest_case 'ni ::bloom[i108 i571 i3491 zB45] nE4 fAA rbA//:bloom
' 3<<'LAZYTEST_EOF'
108	108
571	571
3491	3491
LAZYTEST_EOF
lazytest_file='doc/bloom.md'
lazytest_line=53
lazytest_case 'ni ::bloom[i100 i101 i102 zB45] nE4 p'\''r a, a + 1'\'' rp'\''bloom_contains bloom, a'\''
' 3<<'LAZYTEST_EOF'
100	101
101	102
102	103
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
lazytest_file='doc/cell.md'
lazytest_line=11
lazytest_case 'ni n5 p'\''r a, a*2'\''         # generate two columns of numbers
' 3<<'LAZYTEST_EOF'
1	2
2	4
3	6
4	8
5	10
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=17
lazytest_case 'ni n5 p'\''r a, a*2'\'' ,s      # sums only first column
' 3<<'LAZYTEST_EOF'
1	2
3	4
6	6
10	8
15	10
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=23
lazytest_case 'ni n5 p'\''r a, a*2'\'' ,sAB    # sums both columns
' 3<<'LAZYTEST_EOF'
1	2
3	6
6	12
10	20
15	30
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=38
lazytest_case 'echo -e "The\ntide\nrises\nthe\ntide\nfalls" > tide.csv
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=39
lazytest_case 'cat tide.csv
' 3<<'LAZYTEST_EOF'
The
tide
rises
the
tide
falls
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=46
lazytest_case 'ni tide.csv ,z
' 3<<'LAZYTEST_EOF'
0
1
2
3
1
4
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=58
lazytest_case 'echo -e "The\ntide\nrises\nthe\ntide\nfalls" > tide.csv
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=59
lazytest_case 'cat tide.csv
' 3<<'LAZYTEST_EOF'
The
tide
rises
the
tide
falls
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=66
lazytest_case 'ni tide.csv ,h
' 3<<'LAZYTEST_EOF'
3967641545
2614616249
3746350261
865469908
2614616249
1943727641
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=79
lazytest_case 'ni tide.csv ,H
' 3<<'LAZYTEST_EOF'
0.923788534710184
0.608762784162536
0.872265142621472
0.201507915742695
0.608762784162536
0.452559357741848
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=95
lazytest_case 'ni n4 ,l
' 3<<'LAZYTEST_EOF'
0
0.693147180559945
1.09861228866811
1.38629436111989
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=100
lazytest_case 'ni n4 ,e
' 3<<'LAZYTEST_EOF'
2.71828182845905
7.38905609893065
20.0855369231877
54.5981500331442
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=110
lazytest_case 'ni n4 ,l2
' 3<<'LAZYTEST_EOF'
0
0.999999999999998
1.58496250072115
2
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=115
lazytest_case 'ni n4 ,e2
' 3<<'LAZYTEST_EOF'
2
4
7.99999999999999
16
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=152
lazytest_case 'ni n5 p'\''a*0.3'\''         # generate some non-integer numbers
' 3<<'LAZYTEST_EOF'
0.3
0.6
0.9
1.2
1.5
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=158
lazytest_case 'ni n5 p'\''a*0.3'\'' ,q      # round to the nearest integer
' 3<<'LAZYTEST_EOF'
0
1
1
1
2
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=164
lazytest_case 'ni n5 p'\''a*0.3'\'' ,q.5    # round to the nearest 0.5
' 3<<'LAZYTEST_EOF'
0.5
0.5
1
1
1.5
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=170
lazytest_case 'ni n6 ,q2              # round to the nearest multiple of 2
' 3<<'LAZYTEST_EOF'
2
2
4
4
6
6
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=183
lazytest_case 'ni n05
' 3<<'LAZYTEST_EOF'
0
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=189
lazytest_case 'ni n05 ,q4
' 3<<'LAZYTEST_EOF'
0
0
4
4
4
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=203
lazytest_case 'ni n5 ,s    # running sum
' 3<<'LAZYTEST_EOF'
1
3
6
10
15
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=209
lazytest_case 'ni n5 ,d    # running difference
' 3<<'LAZYTEST_EOF'
1
1
1
1
1
LAZYTEST_EOF
lazytest_file='doc/cell.md'
lazytest_line=215
lazytest_case 'ni n5 ,a    # running average
' 3<<'LAZYTEST_EOF'
1
1.5
2
2.5
3
LAZYTEST_EOF
lazytest_file='doc/cheatsheet_op.md'
lazytest_line=286
lazytest_case 'ni i[a b c d] i[a b x y] i[a b foo bar] YC
' 3<<'LAZYTEST_EOF'
a	b	0	0	c
a	b	0	1	d
a	b	1	0	x
a	b	1	1	y
a	b	2	0	foo
a	b	2	1	bar
LAZYTEST_EOF
lazytest_file='doc/cheatsheet_op.md'
lazytest_line=302
lazytest_case 'ni i[a b 1 5] i[a b 100 500] i[a b -10 -20] \
     i[c d 1 0] i[c d 1 1] \
      NC'\''x = dot(x.T, x)'\''
' 3<<'LAZYTEST_EOF'
a	b	10101	50205
a	b	50205	250425
c	d	2	1
c	d	1	1
LAZYTEST_EOF
lazytest_file='doc/cheatsheet_op.md'
lazytest_line=316
lazytest_case 'ni i[a b c d] i[a b x y] i[a b foo bar] YC XC
' 3<<'LAZYTEST_EOF'
a	b	c	d
a	b	x	y
a	b	foo	bar
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=10
lazytest_case 'ni n5                         # some data
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=16
lazytest_case 'ni ::foo[n5]                  # ...in a memory-resident closure
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=42
lazytest_case 'ni ::foo[n5] Cubuntu[//:foo]
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=48
lazytest_case 'ni ::foo[n5] Cubuntu[n1p'\''r split /\n/, foo'\'']
' 3<<'LAZYTEST_EOF'
1	2	3	4	5
LAZYTEST_EOF
fi                      # $SKIP_DOCKER
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=60
lazytest_case 'rm -r /tmp/* || :
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=61
lazytest_case 'ni :@foo[n10] //@foo e[wc -l]         # disk-backed data closure
' 3<<'LAZYTEST_EOF'
10
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=63
lazytest_case 'ls /tmp | wc -l
' 3<<'LAZYTEST_EOF'
0
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=65
lazytest_case 'ni :@foo[nE5] :@bar[nE4] //@foo //@bar gr9
' 3<<'LAZYTEST_EOF'
1
1
10
10
100
100
1000
1000
10000
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=80
lazytest_case 'ni :@foo[nE6] \
      n1p'\''open my $fh, "<", foo =~ /^file:\/\/(.*)/ or die $!;
          print while <$fh>;()'\'' e[wc -c]
' 3<<'LAZYTEST_EOF'
6888896
LAZYTEST_EOF
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=94
lazytest_case 'ni :@foo[nE5] :@bar[nE4] Cubuntu[//@foo //@bar gr9]
' 3<<'LAZYTEST_EOF'
1
1
10
10
100
100
1000
1000
10000
LAZYTEST_EOF
lazytest_file='doc/closure.md'
lazytest_line=104
lazytest_case 'ni :@foo[nE6] Cubuntu[ \
    n1p'\''open my $fh, "<", foo =~ /^file:\/\/(.*)/ or die $!;
        print while <$fh>;()'\''] e[wc -c]
' 3<<'LAZYTEST_EOF'
6888896
LAZYTEST_EOF
fi                      # $SKIP_DOCKER
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=15
lazytest_case 'ni n8p'\''r map a*$_, 1..8'\'' > mult-table
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=16
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
lazytest_file='doc/col.md'
lazytest_line=31
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
lazytest_file='doc/col.md'
lazytest_line=40
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
lazytest_file='doc/col.md'
lazytest_line=49
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
lazytest_file='doc/col.md'
lazytest_line=58
lazytest_case 'ni mult-table fA-D    # first four columns
' 3<<'LAZYTEST_EOF'
1	2	3	4
2	4	6	8
3	6	9	12
4	8	12	16
5	10	15	20
6	12	18	24
7	14	21	28
8	16	24	32
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=74
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
lazytest_file='doc/col.md'
lazytest_line=83
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
lazytest_file='doc/col.md'
lazytest_line=92
lazytest_case 'ni mult-table x       # even easier (see below)
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
lazytest_file='doc/col.md'
lazytest_line=107
lazytest_case 'ni mult-table xC r2   # swap third column into first position
' 3<<'LAZYTEST_EOF'
3	2	1	4	5	6	7	8
6	4	2	8	10	12	14	16
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=110
lazytest_case 'ni mult-table xGHr2   # swap seventh, eighth columns into first two
' 3<<'LAZYTEST_EOF'
7	8	3	4	5	6	1	2
14	16	6	8	10	12	2	4
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=113
lazytest_case 'ni mult-table xr2     # swap first two columns
' 3<<'LAZYTEST_EOF'
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=137
lazytest_case 'ni /etc/passwd r2F::          # F: followed by :, which is the split char
' 3<<'LAZYTEST_EOF'
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=144
lazytest_case 'ni //ni r3                            # some data
' 3<<'LAZYTEST_EOF'
#!/usr/bin/env perl
$ni::is_lib = caller();
$ni::self{license} = <<'_';
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=152
lazytest_case 'ni //ni r3F/\\//                      # split on forward slashes
' 3<<'LAZYTEST_EOF'
#!	usr	bin	env perl
$ni::is_lib = caller();
$ni::self{license} = <<'_';
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=160
lazytest_case 'ni //ni r3FW                          # split on non-words
' 3<<'LAZYTEST_EOF'
	usr	bin	env	perl
	ni	is_lib	caller	
	ni	self	license	_	
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=168
lazytest_case 'ni //ni r3FS                          # split on whitespace
' 3<<'LAZYTEST_EOF'
#!/usr/bin/env	perl
$ni::is_lib	=	caller();
$ni::self{license}	=	<<'_';
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=176
lazytest_case 'ni //ni r3Fm'\''/\/\w+/'\''                 # words beginning with a slash
' 3<<'LAZYTEST_EOF'
/usr	/bin	/env


LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=188
lazytest_case 'ni //ni r3FW p'\''r a, b, uc(c), FR 3'\''
' 3<<'LAZYTEST_EOF'
	usr	BIN	env	perl
	ni	IS_LIB	caller
	ni	SELF	license	_
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=198
lazytest_case 'ni //ni r3FW vCpuc
' 3<<'LAZYTEST_EOF'
	usr	BIN	env	perl
	ni	IS_LIB	caller
	ni	SELF	license	_
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=210
lazytest_case 'ni //ni r3FWfB
' 3<<'LAZYTEST_EOF'
usr
ni
ni
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=214
lazytest_case 'ni //ni r3FWfB wn100          # right-join numbers
' 3<<'LAZYTEST_EOF'
usr	1
ni	2
ni	3
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=218
lazytest_case 'ni //ni r3FWfB Wn100          # left-join numbers
' 3<<'LAZYTEST_EOF'
1	usr
2	ni
3	ni
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=229
lazytest_case 'ni //license Wn r~3
' 3<<'LAZYTEST_EOF'
19	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
20	SOFTWARE.
21	
LAZYTEST_EOF
lazytest_file='doc/col.md'
lazytest_line=233
lazytest_case 'ni nE5p'\''a*a'\'' Wn r~3
' 3<<'LAZYTEST_EOF'
99998	9999600004
99999	9999800001
100000	10000000000
LAZYTEST_EOF
# These tests only get run in environments where docker is installed
# (centos 5 uses i386 libraries and doesn't support docker, for example).
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=14
lazytest_case 'ni nE4 Cubuntu[gr4] O
' 3<<'LAZYTEST_EOF'
1000
100
10
1
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=33
lazytest_case 'docker build -q -t ni-test/numpy - <<EOF > /dev/null
FROM ubuntu
RUN apt-get update
RUN apt-get install -y python-numpy
CMD /bin/bash
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=39
lazytest_case 'ni n100 Cni-test/numpy[N'\''x = x + 1'\''] r4
' 3<<'LAZYTEST_EOF'
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=53
lazytest_case 'ni n100 CU+python-numpy+sbcl[N'\''x = x + 1'\'' l'\''(1+ a)'\''] r4
' 3<<'LAZYTEST_EOF'
3
4
5
6
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=58
lazytest_case 'ni n100 CA+py-numpy@community+sbcl@testing[N'\''x = x + 1'\'' l'\''(1+ a)'\''] r4
' 3<<'LAZYTEST_EOF'
3
4
5
6
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=70
lazytest_case 'docker run --detach -i --name ni-test-container ubuntu >/dev/null
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=71
lazytest_case 'ni Eni-test-container[n100g =\>/tmp/in-container Bn] r4
' 3<<'LAZYTEST_EOF'
1
10
100
11
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=76
lazytest_case '[[ -e /tmp/in-container ]] || echo '\''file not in host (good)'\''
' 3<<'LAZYTEST_EOF'
file not in host (good)
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=78
lazytest_case 'ni Eni-test-container[/tmp/in-container] | wc -l
' 3<<'LAZYTEST_EOF'
100
LAZYTEST_EOF
lazytest_file='doc/container.md'
lazytest_line=80
lazytest_case 'docker rm -f ni-test-container >/dev/null
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
fi                      # $SKIP_DOCKER (lazytest condition)
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/extend.md'
lazytest_line=6
lazytest_case 'mkdir my-library
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/extend.md'
lazytest_line=7
lazytest_case 'echo my-lib.pl > my-library/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/extend.md'
lazytest_line=8
lazytest_case 'cat > my-library/my-lib.pl <<'\''EOF'\''
defoperator count_lines => q{exec '\''wc'\'', '\''-l'\''};
defshort '\''/wc'\'', pmap q{count_lines_op}, pnone;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/extend.md'
lazytest_line=12
lazytest_case 'ni --lib my-library n100wc
' 3<<'LAZYTEST_EOF'
100
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=7
lazytest_case 'mkdir fractional
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=8
lazytest_case 'echo fractional.pl > fractional/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=9
lazytest_case 'cat >fractional/fractional.pl <<'\''EOF'\''
defexpander ['\''/frac'\'', n => pc integer, step => pc number],
            '\''n$n'\'', '\''pa * $step'\'';
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=18
lazytest_case 'ni --lib fractional frac 10 .5
' 3<<'LAZYTEST_EOF'
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=29
lazytest_case 'ni --lib fractional frac4.5
' 3<<'LAZYTEST_EOF'
0.5
1
1.5
2
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=39
lazytest_case 'ni --run '\''defexpander "/license-words", qw[//license FWpF_]'\'' \
     license-words r10
' 3<<'LAZYTEST_EOF'
ni
https
github
com
spencertipping
ni
Copyright
c
2016
Spencer
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=57
lazytest_case 'mkdir fractional2
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=58
lazytest_case 'echo fractional2.pl > fractional2/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=59
lazytest_case 'cat >fractional2/fractional2.pl <<'\''EOF'\''
defexpander ['\''/frac'\'', n => pc integer, step => pc number],
  sub {
    my %args = @_;
    ("+n$args{n}", "pa * $args{step}");
  };
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/fn.md'
lazytest_line=66
lazytest_case 'ni --lib fractional2 frac 10 .5
' 3<<'LAZYTEST_EOF'
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
LAZYTEST_EOF
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
start_time=0;
until docker exec -i ni-test-hadoop \
      /usr/local/hadoop/bin/hadoop fs -mkdir /test-dir; do
  if (( $(date +%s) - start_time > 60 )); then
    docker rm -f ni-test-hadoop >&2
    docker run --detach -i -m 2G --name ni-test-hadoop \
      sequenceiq/hadoop-docker \
      /etc/bootstrap.sh -bash >&2
    start_time=$(date +%s)
  fi
done
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/hadoop.md'
lazytest_line=55
lazytest_case 'NI_HADOOP=/usr/local/hadoop/bin/hadoop \
  ni n5 Eni-test-hadoop [HS[p'\''r a, a*a'\''] _ _ \<]
' 3<<'LAZYTEST_EOF'
1	1
2	4
3	9
4	16
5	25
LAZYTEST_EOF
lazytest_file='doc/hadoop.md'
lazytest_line=67
lazytest_case 'ni n5 ^{hadoop/name=/usr/local/hadoop/bin/hadoop} \
          Eni-test-hadoop [HS[p'\''r a, a*a'\''] _ [p'\''r a, b+1'\''] \<] o
' 3<<'LAZYTEST_EOF'
1	2
2	5
3	10
4	17
5	26
LAZYTEST_EOF
lazytest_file='doc/hadoop.md'
lazytest_line=79
lazytest_case 'ni //license ^{hadoop/name=/usr/local/hadoop/bin/hadoop} \
                 Eni-test-hadoop [HS[FW pF_] _ [fAcx] \<] r10
' 3<<'LAZYTEST_EOF'
2016	1
A	1
ACTION	1
AN	1
AND	1
ANY	2
ARISING	1
AS	1
AUTHORS	1
BE	1
LAZYTEST_EOF
lazytest_file='doc/hadoop.md'
lazytest_line=99
lazytest_case 'ni //license ^{hadoop/name=/usr/local/hadoop/bin/hadoop \
                 hadoop/jobconf='\''mapred.map.tasks=10
                                 mapred.reduce.tasks=4'\''} \
                 Eni-test-hadoop [HSFWpF_ _ fAcx \<] r10
' 3<<'LAZYTEST_EOF'
2016	1
A	1
BE	1
BUT	1
FOR	2
INCLUDING	1
LIABILITY	1
LIABLE	1
OF	4
OR	7
LAZYTEST_EOF
docker rm -f ni-test-hadoop >&2

fi                      # $SKIP_DOCKER (lazytest condition)
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
Some of these tests require Docker, so skip if we don't have it
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/invariants.md'
lazytest_line=8
lazytest_case 'ni n4E7 ,hA Cubuntu[o] uc
' 3<<'LAZYTEST_EOF'
39814375
LAZYTEST_EOF
lazytest_file='doc/json.md'
lazytest_line=18
lazytest_case 'ni //license FWp'\''json_encode [F_]'\'' r4
' 3<<'LAZYTEST_EOF'
["ni","https","github","com","spencertipping","ni"]
["Copyright","c",2016,"Spencer","Tipping","MIT","license"]
[]
["Permission","is","hereby","granted","free","of","charge","to","any","person","obtaining","a","copy"]
LAZYTEST_EOF
lazytest_file='doc/json.md'
lazytest_line=28
lazytest_case 'ni //license FWp'\''json_encode [F_]'\'' p'\''r @{json_decode a}'\'' r4
' 3<<'LAZYTEST_EOF'
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
LAZYTEST_EOF
lazytest_file='doc/json.md'
lazytest_line=47
lazytest_case 'ni //license FWpF_ p'\''r pl 3'\'' \
     p'\''json_encode {type    => '\''trigram'\'',
                    context => {w1 => a, w2 => b},
                    word    => c}'\'' \>jsons
' 3<<'LAZYTEST_EOF'
jsons
LAZYTEST_EOF
lazytest_file='doc/json.md'
lazytest_line=52
lazytest_case 'ni jsons r5
' 3<<'LAZYTEST_EOF'
{"context":{"w1":"https","w2":"github"},"type":"trigram","word":"com"}
{"context":{"w1":"github","w2":"com"},"type":"trigram","word":"spencertipping"}
{"context":{"w1":"com","w2":"spencertipping"},"type":"trigram","word":"ni"}
{"context":{"w1":"spencertipping","w2":"ni"},"type":"trigram","word":"Copyright"}
{"context":{"w1":"ni","w2":"Copyright"},"type":"trigram","word":"c"}
LAZYTEST_EOF
lazytest_file='doc/json.md'
lazytest_line=63
lazytest_case 'ni jsons D:w1,:w2,:word r5
' 3<<'LAZYTEST_EOF'
https	github	com
github	com	spencertipping
com	spencertipping	ni
spencertipping	ni	Copyright
ni	Copyright	c
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=6
lazytest_case 'ni n4l'\''(+ a 2)'\''
' 3<<'LAZYTEST_EOF'
3
4
5
6
LAZYTEST_EOF
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=21
lazytest_case 'docker build -q -t ni-test/sbcl - <<EOF > /dev/null
FROM ubuntu
RUN apt-get update
RUN apt-get install -y sbcl
CMD /bin/bash
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=27
lazytest_case 'ni Cni-test/sbcl[n4l'\''(+ a 2)'\'']
' 3<<'LAZYTEST_EOF'
3
4
5
6
LAZYTEST_EOF
fi                      # $HAVE_DOCKER
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=44
lazytest_case 'ni n4l'\''(r a (1+ a))'\''                  # generate two columns
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=49
lazytest_case 'ni n4l'\''(r a (1+ a))'\'' l'\''(r (+ a b))'\''   # ... and sum them
' 3<<'LAZYTEST_EOF'
3
5
7
9
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=63
lazytest_case 'ni n2l'\''a (+ a 100)'\''                   # return without "r"
' 3<<'LAZYTEST_EOF'
1
101
2
102
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=86
lazytest_case 'ni n10000l"(sr ('\''+ a))"
' 3<<'LAZYTEST_EOF'
50005000
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=93
lazytest_case 'ni n4fAA l"(r (sr ('\''+ a) ('\''* b)))"
' 3<<'LAZYTEST_EOF'
10	24
LAZYTEST_EOF
lazytest_file='doc/lisp.md'
lazytest_line=108
lazytest_case 'ni /etc/passwd F::gG l"(r g (se (partial #'\''join #\,) a g))"
' 3<<'LAZYTEST_EOF'
/bin/bash	root
/bin/false	syslog
/bin/sh	backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	sync
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=7
lazytest_case 'ni //ni FWr10
' 3<<'LAZYTEST_EOF'
	usr	bin	env	perl
	ni	is_lib	caller	
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
of	this	software	and	associated	documentation	files	the	Software	to	deal
in	the	Software	without	restriction	including	without	limitation	the	rights
to	use	copy	modify	merge	publish	distribute	sublicense	and	or	sell
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=23
lazytest_case 'ni //ni FW Yr10
' 3<<'LAZYTEST_EOF'
0	0	
0	1	usr
0	2	bin
0	3	env
0	4	perl
1	0	
1	1	ni
1	2	is_lib
1	3	caller
1	4	
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=39
lazytest_case 'ni //ni FW fABCD Y X r10
' 3<<'LAZYTEST_EOF'
	usr	bin	env
	ni	is_lib	caller
	ni	self	license
ni	https	github	com
Copyright	c	2016	Spencer

Permission	is	hereby	granted
of	this	software	and
in	the	Software	without
to	use	copy	modify
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=56
lazytest_case 'ni n010p'\''r 0, a%3, 1'\'' X
' 3<<'LAZYTEST_EOF'
4	3	3
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=64
lazytest_case 'ni i[a b] i[c d] pF_
' 3<<'LAZYTEST_EOF'
a
b
c
d
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=74
lazytest_case 'ni i[a b] i[c d] pF_ Z2
' 3<<'LAZYTEST_EOF'
a	b
c	d
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=86
lazytest_case 'ni n10p'\''r map a*$_, 1..10'\''
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
lazytest_file='doc/matrix.md'
lazytest_line=97
lazytest_case 'ni n10p'\''r map a*$_, 1..10'\'' N'\''x = x + 1'\''
' 3<<'LAZYTEST_EOF'
2	3	4	5	6	7	8	9	10	11
3	5	7	9	11	13	15	17	19	21
4	7	10	13	16	19	22	25	28	31
5	9	13	17	21	25	29	33	37	41
6	11	16	21	26	31	36	41	46	51
7	13	19	25	31	37	43	49	55	61
8	15	22	29	36	43	50	57	64	71
9	17	25	33	41	49	57	65	73	81
10	19	28	37	46	55	64	73	82	91
11	21	31	41	51	61	71	81	91	101
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=113
lazytest_case 'ni n4N'\''x = x.T'\''
' 3<<'LAZYTEST_EOF'
1	2	3	4
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=120
lazytest_case 'ni n4N'\''x = reshape(x, (-1))'\''
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=134
lazytest_case 'ni //license plc FW Z1 p'\''r/(.)(.*)/'\'' g r10
' 3<<'LAZYTEST_EOF'
2	016
a	
a	
a	bove
a	ction
a	ll
a	n
a	nd
a	nd
a	nd
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=150
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' g r10
' 3<<'LAZYTEST_EOF'
2	0	1	6
a
a
a	b	o	v	e
a	c	t	i	o	n
a	l	l
a	n
a	n	d
a	n	d
a	n	d
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=161
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' g YB r10
' 3<<'LAZYTEST_EOF'
2	0	0	0
2	0	1	1
2	0	2	6
a	1	0	b
a	1	1	o
a	1	2	v
a	1	3	e
a	2	0	c
a	2	1	t
a	2	2	i
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=172
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' gYB fABD gcfBCDA r10
' 3<<'LAZYTEST_EOF'
2	0	6	1
a			2
a	b	v	1
a	c	i	1
a	l		1
a	n		9
a	r	s	1
a	s		1
a	s	o	1
a	u	h	1
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=191
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' \
      gYBfABDgcfBCDA ,zC o XB r10
' 3<<'LAZYTEST_EOF'
a		2
a			1
a				1
a		1
a		9
a					1
a		1				1
a							1
b		2
b		1
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=208
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' \
     gYBfABDgcfBCDA,zCo XB \
     NB'\''x *= 2'\'' YB,qD.01XB r10
' 3<<'LAZYTEST_EOF'
a	0	4	0	0	0	0	0
a	0	0	2	0	0	0	0
a	0	0	0	2	0	0	0
a	0	2	0	0	0	0	0
a	0	18	0	0	0	0	0
a	0	0	0	0	2	0	0
a	0	2	0	0	0	2	0
a	0	0	0	0	0	0	2
b	0	4
b	0	2
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=226
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' \
     gYBfABDgcfBCDA,zCo XB \
     NB'\''x *= 2
        x += 1'\'' r10
' 3<<'LAZYTEST_EOF'
a	1	5	1	1	1	1	1
a	1	1	3	1	1	1	1
a	1	1	1	3	1	1	1
a	1	3	1	1	1	1	1
a	1	19	1	1	1	1	1
a	1	1	1	1	3	1	1
a	1	3	1	1	1	3	1
a	1	1	1	1	1	1	3
b	1	5
b	1	3
LAZYTEST_EOF
lazytest_file='doc/matrix.md'
lazytest_line=245
lazytest_case 'ni //license plc FWpF_ p'\''r split//'\'' \
     gYBfABDgcfBCDA,zCo XB \
     NB'\''if True:
          x = x + 1'\'' r3
' 3<<'LAZYTEST_EOF'
a	1	3	1	1	1	1	1
a	1	1	2	1	1	1	1
a	1	1	1	2	1	1	1
LAZYTEST_EOF
lazytest_file='doc/net.md'
lazytest_line=6
lazytest_case 'nc -l 1400 <<'\''EOF'\'' > /dev/null &      # an enterprise-grade web server
HTTP/1.1 200 OK
Content-length: 12

Hello world
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/net.md'
lazytest_line=12
lazytest_case 'sleep 1; ni http://localhost:1400 n3
' 3<<'LAZYTEST_EOF'
Hello world
1
2
3
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
lazytest_line=773
lazytest_case 'ni i[foo bar] i[foo car] i[foo dar] i[that no] i[this yes] \
  j[ i[foo mine] i[not here] i[this OK] i[this yipes] ]
' 3<<'LAZYTEST_EOF'
foo	bar	mine
foo	car	mine
foo	dar	mine
this	yes	OK
this	yes	yipes
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=785
lazytest_case 'ni i[M N foo] i[M N bar] i[M O qux] i[X Y cat] i[X Z dog] \
  jAB[ i[M N hi] i[X Y bye] ]
' 3<<'LAZYTEST_EOF'
M	N	foo	hi
M	N	bar	hi
X	Y	cat	bye
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_1.md'
lazytest_line=810
lazytest_case 'ni --explain n10 \>ten.txt \<
' 3<<'LAZYTEST_EOF'
["n",1,11]
["file_write","ten.txt"]
["file_read"]
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=67
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''a()'\''
' 3<<'LAZYTEST_EOF'
first
foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=73
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''c'\''
' 3<<'LAZYTEST_EOF'
third
baz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=81
lazytest_case 'ni i[3 5 7] p'\''a + b + c'\''
' 3<<'LAZYTEST_EOF'
15
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=86
lazytest_case 'ni i[easy speak] p'\''b . a'\''
' 3<<'LAZYTEST_EOF'
speakeasy
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=97
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''c, a'\''
' 3<<'LAZYTEST_EOF'
third
first
baz
foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=113
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''r(c, a)'\''
' 3<<'LAZYTEST_EOF'
third	first
baz	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=121
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''r c, a'\''
' 3<<'LAZYTEST_EOF'
third	first
baz	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=136
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r F_(1..3)'\''
' 3<<'LAZYTEST_EOF'
second	third	fourth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=143
lazytest_case 'ni i[first second third fourth fifth sixth] \
     i[only two_fields ] p'\''r FM'\''
' 3<<'LAZYTEST_EOF'
5
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=152
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r F_(3..FM)'\''
' 3<<'LAZYTEST_EOF'
fourth	fifth	sixth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=159
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r FR 3'\''
' 3<<'LAZYTEST_EOF'
fourth	fifth	sixth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=166
lazytest_case 'ni i[first second third fourth fifth sixth] p'\''r FT 3'\''
' 3<<'LAZYTEST_EOF'
first	second	third
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=178
lazytest_case 'ni p'\''r "foo" . "bar"'\''
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=184
lazytest_case 'ni 1p'\''r "foo" . "bar"'\''
' 3<<'LAZYTEST_EOF'
foobar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=196
lazytest_case 'ni i1 i10 i100 i1000 p'\''r a, length(a)'\''
' 3<<'LAZYTEST_EOF'
1	1
10	2
100	3
1000	4
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=208
lazytest_case 'ni i1 i10 i100 i1000 p'\''r a, length'\''
' 3<<'LAZYTEST_EOF'
1	1
10	2
100	3
1000	4
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=221
lazytest_case 'ni i1 i10 i100 i1000 p'\''$_'\''
' 3<<'LAZYTEST_EOF'
1
10
100
1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=235
lazytest_case 'ni 1p'\''$x; ++$x'\''
' 3<<'LAZYTEST_EOF'
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=240
lazytest_case 'ni 1p'\''@x; $x[3] = "yo"; r @x'\''
' 3<<'LAZYTEST_EOF'
			yo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=250
lazytest_case 'ni 1p'\''@x; $x[3] = "yo"; exists $x[0] ? "yes" : "no"'\''
' 3<<'LAZYTEST_EOF'
no
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=255
lazytest_case 'ni 1p'\''%h = {"u" => "ok"}; exists $h["me"] ? "yes" : "no"'\''
' 3<<'LAZYTEST_EOF'
no
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=262
lazytest_case 'ni 1p'\''$x; defined $x ? "yes" : "no"'\''
' 3<<'LAZYTEST_EOF'
no
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=286
lazytest_case 'ni n5 p'\''my $x = a; {my $x = 100;} ++$x'\''
' 3<<'LAZYTEST_EOF'
2
3
4
5
6
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=297
lazytest_case 'ni n5 p'\''my $y = a; {$x = 100;} ++$x'\''
' 3<<'LAZYTEST_EOF'
101
101
101
101
101
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=309
lazytest_case 'ni n5 p'\''my $y = a; {my $x = 100;} 
			defined $x ? "defined" : "not defined"'\''
' 3<<'LAZYTEST_EOF'
not defined
not defined
not defined
not defined
not defined
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=321
lazytest_case 'ni i[foo bar] p'\''my ($first, $second) = F_; r $second, $first'\''
' 3<<'LAZYTEST_EOF'
bar	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=328
lazytest_case 'ni i[foo bar baz qux qal] \
	  p'\''my ($first, $second, @rest) = F_;
	    r $second, $first; r @rest'\''
' 3<<'LAZYTEST_EOF'
bar	foo
baz	qux	qal
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=345
lazytest_case 'ni n5p'\''^{$x = 10} $x += a; r a, $x'\''
' 3<<'LAZYTEST_EOF'
1	11
2	13
3	16
4	20
5	25
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=356
lazytest_case 'ni n5p'\''$x = 10; $x += a; r a, $x'\''
' 3<<'LAZYTEST_EOF'
1	11
2	12
3	13
4	14
5	15
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=369
lazytest_case 'ni n5p'\''^{$x = 10} $x += a; r a, $x'\'' p'\''r $x'\''
' 3<<'LAZYTEST_EOF'




LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=380
lazytest_case 'ni i[faygo cola] i[orange juice] i[grape soda] i[orange crush] i[hawaiian punch] rp'\''^{%good_flavors = ("orange" => 1, "grape" => 1)} my $flavor = a; $good_flavors{$flavor}'\''
' 3<<'LAZYTEST_EOF'
orange	juice
grape	soda
orange	crush
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=390
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''k'\''
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=394
lazytest_case 'ni i[first second third] i[foo bar baz] p'\''r k'\''
' 3<<'LAZYTEST_EOF'


LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=412
lazytest_case 'ni n3 rp'\''a'\''
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=421
lazytest_case 'ni n03 rp'\''a'\''
' 3<<'LAZYTEST_EOF'
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=429
lazytest_case 'ni n03 rp'\''r a'\''
' 3<<'LAZYTEST_EOF'
0
1
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=441
lazytest_case 'ni n03 rp'\''r b'\''
' 3<<'LAZYTEST_EOF'



LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=467
lazytest_case 'ni i37 p'\''if (a == 2) { r "input was 2" } 
			 elsif (a =~ /^[Qq]/ ) { r "input started with a Q" } 
			 else { r "I dunno" }'\''
' 3<<'LAZYTEST_EOF'
I dunno
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=478
lazytest_case 'ni i5 p'\''r "input = 5" if a == 5'\''
' 3<<'LAZYTEST_EOF'
input = 5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=488
lazytest_case 'ni i5 p'\''r "input = 5" unless a != 5'\''
' 3<<'LAZYTEST_EOF'
input = 5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=497
lazytest_case 'ni i6 p'\''a == 5 ? "input = 5" : "input != 5"'\''
' 3<<'LAZYTEST_EOF'
input != 5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=543
lazytest_case 'ni ihello p'\''a =~ /^h/ ? "match" : "no match"'\''
' 3<<'LAZYTEST_EOF'
match
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=550
lazytest_case 'ni igoodbye p'\''a !~ /^h/ ? "negative match" : "positive match"'\''
' 3<<'LAZYTEST_EOF'
negative match
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=559
lazytest_case 'ni iabcdefgh p'\''my @v = a =~ /^(.)(.)/; r @v'\''
' 3<<'LAZYTEST_EOF'
a	b
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=570
lazytest_case 'ni i/usr/bin p'\''m#^/usr/(.*)$#'\''
' 3<<'LAZYTEST_EOF'
bin
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=580
lazytest_case 'ni iabcdefgh p'\''tr/a-z/A-Z/'\''
' 3<<'LAZYTEST_EOF'
8
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=585
lazytest_case 'ni iabcdefgh p'\''s/abc/ABC/'\''
' 3<<'LAZYTEST_EOF'
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=593
lazytest_case 'ni iabcdefgh p'\''$v = tr/a-z/A-Z/; $v'\''
' 3<<'LAZYTEST_EOF'
8
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=600
lazytest_case 'ni iabcdefgh p'\''tr/a-z/A-Z/; $_'\''
' 3<<'LAZYTEST_EOF'
ABCDEFGH
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=605
lazytest_case 'ni iabcdefgh p'\''s/abc/ABC/; $_'\''
' 3<<'LAZYTEST_EOF'
ABCdefgh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=617
lazytest_case 'ni 1p'\''$foo = "house"; 
        "housecat" =~ /$foo/ ? "match" : "no match"'\''
' 3<<'LAZYTEST_EOF'
match
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=623
lazytest_case 'ni 1p'\''$foo = "house"; 
        "cathouse" =~ /cat$foo/ ? "match" : "no match"'\''
' 3<<'LAZYTEST_EOF'
match
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=629
lazytest_case 'ni 1p'\''$foo = "house"; 
        "housecat" =~ /${foo}cat/ ? "match" : "no match"'\''
' 3<<'LAZYTEST_EOF'
match
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=641
lazytest_case 'ni 1p'\''"foo" . "bar"'\'' 
' 3<<'LAZYTEST_EOF'
foobar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=651
lazytest_case 'ni 1p'\''my $x = "foo"; r "$x bar"'\''
' 3<<'LAZYTEST_EOF'
foo bar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=658
lazytest_case 'ni 1p'\''my $x = "foo"; r "${x}bar"'\''
' 3<<'LAZYTEST_EOF'
foobar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=665
lazytest_case 'ni ifoo p'\''r "${\a}bar"'\''
' 3<<'LAZYTEST_EOF'
foobar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=680
lazytest_case 'ni 1p'\'' "ab" == "cd" ? "equal" : "not equal"'\''
' 3<<'LAZYTEST_EOF'
equal
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=687
lazytest_case 'ni 1p'\'' "ab" eq "cd" ? "equal" : "not equal"'\''
' 3<<'LAZYTEST_EOF'
not equal
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=702
lazytest_case 'ni iabcdefgh p'\''r substr(a, 3), substr(a, 0, 3),
                   substr(a, -2), substr(a, 0, -2)'\''
' 3<<'LAZYTEST_EOF'
defgh	abc	gh	abcdef
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=713
lazytest_case 'ni iabcdefgh p'\''r /(.{5})$/, /^(.{3})/, /(.{2})$/, /^(.*)../'\''
' 3<<'LAZYTEST_EOF'
defgh	abc	gh	abcdef
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=729
lazytest_case 'ni 1p'\''my @arr = (1, 2, 3); r @arr'\''
' 3<<'LAZYTEST_EOF'
1	2	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=738
lazytest_case 'ni 1p'\''my @arr = qw(1 2 3); r @arr'\''
' 3<<'LAZYTEST_EOF'
1	2	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=748
lazytest_case 'ni iabcd iefgh p'\''my @v = split /[cf]/; r @v'\''
' 3<<'LAZYTEST_EOF'
ab	d
e	gh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=771
lazytest_case 'ni 1p'\''my $arr_ref = [1, 2, 3]; r @$arr_ref'\''
' 3<<'LAZYTEST_EOF'
1	2	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=779
lazytest_case 'ni 1p'\''my @arr = [1, 2, 3]; r @{$arr[0]}'\''
' 3<<'LAZYTEST_EOF'
1	2	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=789
lazytest_case 'ni iabcdefgh p'\''my $string= a; for (my $i=0; $i < length $string; $i++) {r substr($string, $i, 1) x 2;}'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
dd
ee
ff
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=803
lazytest_case 'ni iabcdefgh p'\''for my $letter(split //, $_) {r $letter x 2}'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
dd
ee
ff
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=817
lazytest_case 'ni iabcdefgh p'\''for(split //, $_) {r $_ x 2}'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
dd
ee
ff
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=831
lazytest_case 'ni iabcdefgh p'\''for(split //, $_) {r $_ x 2} r $_'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
dd
ee
ff
gg
hh
abcdefgh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=852
lazytest_case 'ni iabcdefgh p'\''r $_ x 2 for split //'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
dd
ee
ff
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=868
lazytest_case 'ni iabcdefgh p'\''for my $letter(split //, $_) 
                 {if($letter eq "b") {next;} r $letter x 2}'\''
' 3<<'LAZYTEST_EOF'
aa
cc
dd
ee
ff
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=883
lazytest_case 'ni iabcdefgh p'\''for my $letter(split //, $_) {r $letter x 2; last if $letter ge "c"}'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=896
lazytest_case 'ni iabcd iefgh p'\''join "__", split //'\''
' 3<<'LAZYTEST_EOF'
a__b__c__d
e__f__g__h
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=906
lazytest_case 'ni iabcdefgh p'\''map {$_ x 2} split //'\''
' 3<<'LAZYTEST_EOF'
aa
bb
cc
dd
ee
ff
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=920
lazytest_case 'ni iabcdefgh p'\''my @v = map {$_ x 2} split //; r $_'\''
' 3<<'LAZYTEST_EOF'
abcdefgh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=937
lazytest_case 'ni iabcdefgh p'\''my @v = grep /^[acgh]/, map {$_ x 2} split //, $_; @v'\''
' 3<<'LAZYTEST_EOF'
aa
cc
gg
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=947
lazytest_case 'ni iabcdefgh p'\''my @v = grep { ord(substr($_, 0, 1)) % 2 == 0} 
                         map { $_ x 2 } split //, $_; @v'\''
' 3<<'LAZYTEST_EOF'
bb
dd
ff
hh
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=963
lazytest_case 'ni 1p'\''my @arr = (3, 5, 1); r reverse @arr'\''
' 3<<'LAZYTEST_EOF'
1	5	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=970
lazytest_case 'ni 1p'\''my @arr = (3, 5, 1); r sort @arr'\''
' 3<<'LAZYTEST_EOF'
1	3	5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=975
lazytest_case 'ni 1p'\''@arr = qw[ foo bar baz ]; r sort @arr'\''
' 3<<'LAZYTEST_EOF'
bar	baz	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=982
lazytest_case 'ni 1p'\''@arr = qw[ foo bar baz ]; r reverse sort @arr'\''
' 3<<'LAZYTEST_EOF'
foo	baz	bar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=989
lazytest_case 'ni i[romeo juliet rosencrantz guildenstern] p'\''r sort F_'\''
' 3<<'LAZYTEST_EOF'
F_
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1000
lazytest_case 'ni i[romeo juliet rosencrantz guildenstern] p'\''r sort +F_'\''
' 3<<'LAZYTEST_EOF'
guildenstern	juliet	romeo	rosencrantz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1007
lazytest_case 'ni i[romeo juliet rosencrantz guildenstern] \
     p'\''my @arr = F_; r sort @arr'\''
' 3<<'LAZYTEST_EOF'
guildenstern	juliet	romeo	rosencrantz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1021
lazytest_case 'ni i[romeo juliet rosencrantz guildenstern hero leander] \
     p'\''my @arr = F_; r sort {length $a <=> length $b } F_'\''
' 3<<'LAZYTEST_EOF'
hero	romeo	juliet	leander	rosencrantz	guildenstern
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1029
lazytest_case 'ni i[romeo juliet rosencrantz guildenstern hero leander] \
     p'\''my @arr = F_; r sort {length $b <=> length $a } F_'\''
' 3<<'LAZYTEST_EOF'
guildenstern	rosencrantz	leander	juliet	romeo	hero
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1042
lazytest_case 'ni 1p'\''my @x = (1, 2); my $y = "yo"; my @z = ("good", "bye");
        map {$_ x 2} @x, $y, @z'\''
' 3<<'LAZYTEST_EOF'
11
22
yoyo
goodgood
byebye
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1060
lazytest_case 'ni 1p'\''my %h = ("x" => 1); r $h{"x"}'\''
' 3<<'LAZYTEST_EOF'
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1067
lazytest_case 'ni 1p'\''my %h = ("x", 1); r $h{"x"}'\''
' 3<<'LAZYTEST_EOF'
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1075
lazytest_case 'ni 1p'\''my @arr = qw[foo 1 bar 2]; my %h = @arr; r $h{"bar"}'\''
' 3<<'LAZYTEST_EOF'
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1084
lazytest_case 'ni 1p'\''my %h = ("foo" => 1, "bar" => 2); r sort keys %h'\''
' 3<<'LAZYTEST_EOF'
bar	foo
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1091
lazytest_case 'ni 1p'\''my %h = ("foo" => 1, "bar" => 2, "baz" => 3); 
        my @ks = keys %h; my @vs = values %h; 
        my $same_order = 1; 
        for(my $i = 0; $i <= $#ks; $i++) { 
          if($h{$ks[i]} != $vs[i]) {$same_order = 0; last} 
        } 
        r $same_order ? "Same order" : "Different order"'\''
' 3<<'LAZYTEST_EOF'
Same order
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1139
lazytest_case 'ni 1p'\''125 >> 3'\''
' 3<<'LAZYTEST_EOF'
15
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1144
lazytest_case 'ni 1p'\''int(125/2**3)'\''
' 3<<'LAZYTEST_EOF'
15
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1152
lazytest_case 'ni 1p'\''125 << 3'\''
' 3<<'LAZYTEST_EOF'
1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1168
lazytest_case 'ni 1p'\''r 0x3 + 0xa, 0x3 + 0xA'\''
' 3<<'LAZYTEST_EOF'
13	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1177
lazytest_case 'ni 1p'\''r 0x3 & 0xa, 0x3 | 0xa'\''
' 3<<'LAZYTEST_EOF'
2	11
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1185
lazytest_case 'ni 1p'\''r 3 & 10, 3 | 10'\''
' 3<<'LAZYTEST_EOF'
2	11
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1197
lazytest_case 'ni 1p'\''my $v1="5"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
15	555	5 golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1204
lazytest_case 'ni 1p'\''my $v2="4.3" * "6.7"; r $v2'\''
' 3<<'LAZYTEST_EOF'
28.81
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1211
lazytest_case 'ni 1p'\''my $v1="hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
0	hihihi	hi golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1216
lazytest_case 'ni 1p'\''my $v1="3.14hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
9.42	3.14hi3.14hi3.14hi	3.14hi golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1221
lazytest_case 'ni 1p'\''my $v1="3.1E17"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'\''
' 3<<'LAZYTEST_EOF'
930000000000000000	3.1E173.1E173.1E17	3.1E17 golden rings
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1231
lazytest_case 'ni n3p'\''r a, one'\''
' 3<<'LAZYTEST_EOF'
1	one
2	one
3	one
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1242
lazytest_case 'ni 1p'\''my %h = ("foo" => 32); $h{foo}'\''
' 3<<'LAZYTEST_EOF'
32
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1256
lazytest_case 'ni ifoo p'\''my %h = ("foo" => 32, "a" => "hello"); $h{a}'\''
' 3<<'LAZYTEST_EOF'
hello
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_2.md'
lazytest_line=1264
lazytest_case 'ni ifoo p'\''my %h = ("foo" => 32); $h{+a}'\''
' 3<<'LAZYTEST_EOF'
32
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=42
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe(a, b, 7)'\''
' 3<<'LAZYTEST_EOF'
9q5cc25
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=47
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe(a, b, -35)'\''
' 3<<'LAZYTEST_EOF'
10407488581
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=54
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe(a, b)'\''
' 3<<'LAZYTEST_EOF'
9q5cc25twby7
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=61
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b, 9'\''
' 3<<'LAZYTEST_EOF'
9q5cc25tw
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=76
lazytest_case 'ni i[34.058566 -118.416526] p'\''r ghd ghe a, b'\''
' 3<<'LAZYTEST_EOF'
34.058565851301	-118.416526280344
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=81
lazytest_case 'ni i[34.058566 -118.416526] p'\''r ghd ghe(a, b, -41), 41'\''
' 3<<'LAZYTEST_EOF'
34.0584754943848	-118.416652679443
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=90
lazytest_case 'ni 1p'\''r ghb "95qc"'\''
' 3<<'LAZYTEST_EOF'
18.6328123323619	18.45703125	-125.156250335276	-125.5078125
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=98
lazytest_case 'ni 1p'\''gh_dist "95qcc25y", "95qccdnv", mi'\''
' 3<<'LAZYTEST_EOF'
1.23981551084308
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=105
lazytest_case 'ni 1p'\''gh_dist "95qcc25y", "95qccdnv"'\''
' 3<<'LAZYTEST_EOF'
1.99516661267524
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=114
lazytest_case 'ni 1p'\''lat_lon_dist 31.21984, 121.41619, 34.058686, -118.416762'\''
' 3<<'LAZYTEST_EOF'
10426.7380460312
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=130
lazytest_case 'ni 1p'\''tpe(2017, 1, 22, 8, 5, 13)'\''
' 3<<'LAZYTEST_EOF'
1485072313
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=137
lazytest_case 'ni 1p'\''tpe("mdYHMS", 1, 22, 2017, 8, 5, 13)'\''
' 3<<'LAZYTEST_EOF'
1485072313
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=146
lazytest_case 'ni 1p'\''r tep tpe 2017, 1, 22, 8, 5, 13'\''
' 3<<'LAZYTEST_EOF'
2017	1	22	8	5	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=159
lazytest_case 'ni i[34.058566 -118.416526] \
     p'\''my $epoch_time = 1485079513; my $tz_offset = tsec(a, b); 
       my @local_time_parts = tep($epoch_time + $tz_offset); 
       r @local_time_parts'\''
' 3<<'LAZYTEST_EOF'
2017	1	22	2	41	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=174
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b'\'' \
     p'\''my $epoch_time = 1485079513; 
       my @local_time_parts = tep ghl($epoch_time, a); 
       r @local_time_parts'\''
' 3<<'LAZYTEST_EOF'
2017	1	22	2	41	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=182
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b, -60'\'' \
     p'\''my $epoch_time = 1485079513; 
       my @local_time_parts = tep gh6l($epoch_time, a); 
       r @local_time_parts'\''
' 3<<'LAZYTEST_EOF'
2017	1	22	2	41	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=197
lazytest_case 'ni i2017-06-24T18:23:47+00:00 i2017-06-24T19:23:47+01:00 \
     i2017-06-24T15:23:47-03:00 i2017-06-24T13:08:47-05:15 \
     i20170624T152347-0300 i20170624T182347Z \
     i2017-06-24T18:23:47+00:00 i"2017-06-24 18:23:47.683" p'\''i2e a'\''
' 3<<'LAZYTEST_EOF'
1498328627
1498328627
1498328627
1498328627
1498328627
1498328627
1498328627
1498328627
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=214
lazytest_case 'ni i2017-06-24T18:23:47+00:00 p'\''i2e a'\'' \
     p'\''r e2i a; r e2i a, -1.5; r e2i a, "+3";
       r e2i a, "-05:45"'\''
' 3<<'LAZYTEST_EOF'
2017-06-24T18:23:47Z
2017-06-24T16:53:47-01:30
2017-06-24T21:23:47+03:00
2017-06-24T12:38:47-05:45
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=226
lazytest_case 'ni i2017-06-24T18:23:47+00:00 p'\''i2e a'\'' \
     p'\''r e2i a; r e2i a, -1.5; r e2i a, "+3";
       r e2i a, "-05:45"'\'' p'\''i2e a'\''
' 3<<'LAZYTEST_EOF'
1498328627
1498328627
1498328627
1498328627
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=241
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b, -60'\'' \
     p'\''my $epoch_time = 1485079513; dow gh6l($epoch_time, a)'\''
' 3<<'LAZYTEST_EOF'
Sun
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=247
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b, -60'\'' \
     p'\''my $epoch_time = 1485079513; hod gh6l($epoch_time, a)'\''
' 3<<'LAZYTEST_EOF'
2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=253
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b, -60'\'' \
     p'\''my $epoch_time = 1485079513; how gh6l($epoch_time, a)'\''
' 3<<'LAZYTEST_EOF'
Sun_02
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=259
lazytest_case 'ni i[34.058566 -118.416526] p'\''ghe a, b, -60'\'' \
     p'\''my $epoch_time = 1485079513; ym gh6l($epoch_time, a)'\''
' 3<<'LAZYTEST_EOF'
2017-01
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=269
lazytest_case 'ni i1494110651 p'\''r tep ttd(a); r tep tth(a);
                   r tep tt15(a); r tep ttm(a); r tep a'\''
' 3<<'LAZYTEST_EOF'
2017	5	6	0	0	0
2017	5	6	22	0	0
2017	5	6	22	30	0
2017	5	6	22	44	0
2017	5	6	22	44	11
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=307
lazytest_case 'ni n10p'\''r rw {a < 7}'\''
' 3<<'LAZYTEST_EOF'
1	2	3	4	5	6
7
8
9
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=316
lazytest_case 'ni n10p'\''r ru {a % 4 == 0}'\''
' 3<<'LAZYTEST_EOF'
1	2	3
4	5	6	7
8	9	10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=323
lazytest_case 'ni n10p'\''r re {int(a**2/30)}'\''
' 3<<'LAZYTEST_EOF'
1	2	3	4	5
6	7
8	9
10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=336
lazytest_case 'ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'\''my @lines = re {a}; r @lines;'\''
' 3<<'LAZYTEST_EOF'
j	can	j	you	j	feel
k	the	k	love
l	tonight
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=349
lazytest_case 'ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'\''my @lines = re {a}; r b_(@lines)'\''
' 3<<'LAZYTEST_EOF'
can	you	feel
the	love
tonight
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=362
lazytest_case 'ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'\''my @lines = rea; r b_ @lines'\''
' 3<<'LAZYTEST_EOF'
can	you	feel
the	love
tonight
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=373
lazytest_case 'ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'\''r b_ rea'\''
' 3<<'LAZYTEST_EOF'
can	you	feel
the	love
tonight
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=384
lazytest_case 'ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'\''my @lines = rea; r b_ @lines; r a_ @lines'\''
' 3<<'LAZYTEST_EOF'
can	you	feel
j	j	j
the	love
k	k
tonight
l
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=402
lazytest_case 'ni i[a x first] i[a x second] \
     i[a y third] i[b y fourth] p'\''r c_ rea'\''
' 3<<'LAZYTEST_EOF'
first	second	third
fourth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=413
lazytest_case 'ni i[a x first] i[a x second] \
     i[a y third] i[b y fourth] p'\''r c_ re {b}'\''
' 3<<'LAZYTEST_EOF'
first	second
third	fourth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=422
lazytest_case 'ni i[a x first] i[a x second] \
     i[a y third] i[b y fourth] p'\''r c_ reb'\''
' 3<<'LAZYTEST_EOF'
first	second
third
fourth
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=438
lazytest_case 'ni ::data[n5] 1p'\''a_ data'\''
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=451
lazytest_case 'ni i[m 1 x] i[m 2 y s t] \
     i[m 3 yo] i[n 5 who] i[n 6 let the dogs] p'\''r b__ rea'\''
' 3<<'LAZYTEST_EOF'
1	x	2	y	s	t	3	yo
5	who	6	let	the	dogs
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=471
lazytest_case 'ni i[1 2 3] p'\''r min F_; r max F_'\''
' 3<<'LAZYTEST_EOF'
1
3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=477
lazytest_case 'ni i[c a b] p'\''r minstr F_; r maxstr F_'\''
' 3<<'LAZYTEST_EOF'
a
c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=496
lazytest_case 'ni i[2 3 4] p'\''r sum(F_), prod(F_), mean(F_)'\''
' 3<<'LAZYTEST_EOF'
9	24	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=505
lazytest_case 'ni i[a c b c c a] p'\''my @uniqs = uniq F_; r sort @uniqs'\''
' 3<<'LAZYTEST_EOF'
a	b	c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=512
lazytest_case 'ni i[a c b c c a] p'\''my %h = %{freqs F_}; 
                      r($_, $h{$_}) for sort keys %h'\''
' 3<<'LAZYTEST_EOF'
a	2
b	1
c	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=527
lazytest_case 'ni i[2 3 4] p'\''r any {$_ > 3} F_; r all {$_ > 3} F_'\''
' 3<<'LAZYTEST_EOF'
1
0
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=537
lazytest_case 'ni i[aa bbb c] p'\''r argmax {length} F_; r argmin {length} F_'\''
' 3<<'LAZYTEST_EOF'
bbb
c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=545
lazytest_case 'ni i[aa bbb c ddd e] p'\''r argmax {length} F_; r argmin {length} F_'\''
' 3<<'LAZYTEST_EOF'
bbb
c
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=556
lazytest_case 'ni 1p'\''cart [10, 20], [1, 2, 3]'\''
' 3<<'LAZYTEST_EOF'
10	1
10	2
10	3
20	1
20	2
20	3
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=581
lazytest_case 'ni i[1 2 3 4 5 6 7] p'\''r take 3, F_; r drop 3, F_'\''
' 3<<'LAZYTEST_EOF'
1	2	3
4	5	6	7
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=589
lazytest_case 'ni i[1 2 3 4 5 6 7] p'\''r take_while {$_ < 3} F_;
                        r drop_while {$_ < 3} F_'\''
' 3<<'LAZYTEST_EOF'
1	2
3	4	5	6	7
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=606
lazytest_case 'ni i[a 1] i[b 2] i[foo bar] \
     p'\''my @lines = rw {1};
       my %h = ab_ @lines; my @sorted_keys = sort keys %h;
       r @sorted_keys; r map {$h{$_}} @sorted_keys'\''
' 3<<'LAZYTEST_EOF'
a	b	foo
1	2	bar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=617
lazytest_case 'ni i[a 1] i[b 2] i[foo bar] \
     p'\''my @lines = rw {1};
       my %h = ab_ @lines; my @sorted_keys = sort keys %h;
       r @sorted_keys; r @h{@sorted_keys}'\''
' 3<<'LAZYTEST_EOF'
a	b	foo
1	2	bar
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=628
lazytest_case 'ni ::passwords[idragon i12345] i[try this] i[123 fails] \
     i[dragon does work] i[12345 also works] \
     i[other ones] i[also fail] \
     rp'\''^{%h = ab_ passwords} exists($h{+a})'\''
' 3<<'LAZYTEST_EOF'
dragon	does	work
12345	also	works
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=646
lazytest_case 'ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] p'\''r acS rea'\''
' 3<<'LAZYTEST_EOF'
x	5
y	13
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=654
lazytest_case 'ni i[y m 4 foo] i[y p 8] i[y n 1 bar] \
     p'\''%h = dcSNN rea; 
       @sorted_keys = kbv_dsc %h;
       r($_, $h{$_}) for @sorted_keys'\''
' 3<<'LAZYTEST_EOF'
foo	4
bar	1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=668
lazytest_case 'ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] \
     p'\''r acS rea'\'' p'\''r kbv_dsc(ab_ rl(3))'\''
' 3<<'LAZYTEST_EOF'
y	x	z
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_3.md'
lazytest_line=675
lazytest_case 'ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] \
     p'\''r acS rea'\'' p'\''r kbv_asc(ab_ rl(3))'\''
' 3<<'LAZYTEST_EOF'
z	x	y
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=35
lazytest_case 'ni ::five[n5] n3p'\''r a, five'\''
' 3<<'LAZYTEST_EOF'
1	12345
2	12345
3	12345
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=43
lazytest_case 'ni --explain ::five[n5] n3p'\''r a, five'\''
' 3<<'LAZYTEST_EOF'
["memory_data_closure","five",[["n",1,6]]]
["n",1,4]
["perl_mapper","r a, five"]
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=56
lazytest_case 'ni ::five[n5] | ni n3p'\''r a, five'\''
' 3<<'LAZYTEST_EOF'
1	five
2	five
3	five
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=272
lazytest_case 'ni ::five[n5] //ni | perl - 1p'\''five'\''
' 3<<'LAZYTEST_EOF'
1
2
3
4
5

LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=493
lazytest_case 'ni n1000 rs5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=547
lazytest_case 'ni i0edd9c94-24d8-4a3e-b8fb-a33c37386ae1 p'\''h2b64 a'\''
' 3<<'LAZYTEST_EOF'
Dt2clCTYSj64+6M8Nzhq4#
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=554
lazytest_case 'ni i0edd9c94-24d8-4a3e-b8fb-a33c37386ae1 p'\''b642h h2b64 a'\''
' 3<<'LAZYTEST_EOF'
0edd9c9424d84a3eb8fba33c37386ae1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_4.md'
lazytest_line=561
lazytest_case 'ni i0edd9c94-24d8-4a3e-b8fb-a33c37386ae1 p'\''hyphenate_uuid b642h h2b64 a'\''
' 3<<'LAZYTEST_EOF'
0edd9c94-24d8-4a3e-b8fb-a33c37386ae1
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
lazytest_case 'ni //license w[n3p'\''a*a'\'']
' 3<<'LAZYTEST_EOF'
ni: https://github.com/spencertipping/ni	1
Copyright (c) 2016 Spencer Tipping | MIT license	4
	9
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
lazytest_line=234
lazytest_case 'ni 1p'\''"a".."e"'\'' p'\''split / /'\'' :letters gA- wn gA
' 3<<'LAZYTEST_EOF'
a	5
b	4
c	3
d	2
e	1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=245
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
lazytest_line=261
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
lazytest_line=278
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
lazytest_line=304
lazytest_case 'ni //license FW Y r10
' 3<<'LAZYTEST_EOF'
0	0	ni
0	1	https
0	2	github
0	3	com
0	4	spencertipping
0	5	ni
1	0	Copyright
1	1	c
1	2	2016
1	3	Spencer
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=321
lazytest_case 'ni //license FW Y r10 X
' 3<<'LAZYTEST_EOF'
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=330
lazytest_case 'ni 1p'\''"a".."l"'\'' Z4
' 3<<'LAZYTEST_EOF'
a	b	c	d
e	f	g	h
i	j	k	l
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=389
lazytest_case 'ni nE4 fAAA ,aA ,sB ,dC r~1
' 3<<'LAZYTEST_EOF'
5000.5	50005000	1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=414
lazytest_case 'ni n3p'\''r map a*$_, 1..3'\'' N'\''x = x + 1'\''
' 3<<'LAZYTEST_EOF'
2	3	4
3	5	7
4	7	10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=421
lazytest_case 'ni n5p'\''r map a . $_, 1..3'\'' N'\''x = x.T'\''
' 3<<'LAZYTEST_EOF'
11	21	31	41	51
12	22	32	42	52
13	23	33	43	53
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=436
lazytest_case 'ni i[1 0] i[1 1] N'\''x = dot(x, x.T)'\''
' 3<<'LAZYTEST_EOF'
1	1
1	2
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=483
lazytest_case 'ni n4m'\''r a, ai + 1'\''
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=494
lazytest_case 'ni //license FWr2m'\''r fields[0..3]'\''
' 3<<'LAZYTEST_EOF'
ni	https	github	com
Copyright	c	2016	Spencer
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_5.md'
lazytest_line=511
lazytest_case 'ni n4fAA l"(r (sr ('\''+ a) ('\''* b)))"
' 3<<'LAZYTEST_EOF'
10	24
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=53
lazytest_case 'ni n5 \>n1.3E5
' 3<<'LAZYTEST_EOF'
n1.3E5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=58
lazytest_case 'ni n1.3E5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=118
lazytest_case 'ni n5p'\''^{@x}; push @x, 2*a; undef; END{r join " and ", @x}'\''
' 3<<'LAZYTEST_EOF'
2 and 4 and 6 and 8 and 10
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=154
lazytest_case 'ni n3p'\''*v = sub {$_[0] x 4}; &v(a)'\''
' 3<<'LAZYTEST_EOF'
1111
2222
3333
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=165
lazytest_case 'ni 1p'\''sub yo {"hi " . $_[0]} yo a'\''
' 3<<'LAZYTEST_EOF'
hi 1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=183
lazytest_case 'ni n1E5p'\''sr {$_[0] + a} 0'\''
' 3<<'LAZYTEST_EOF'
5000050000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=196
lazytest_case 'ni n1E1p'\''r sr {$_[0] + a, $_[1] * a, $_[2] . a} 0, 1, ""'\''
' 3<<'LAZYTEST_EOF'
55	3628800	12345678910
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=220
lazytest_case 'ni n1000p'\''se {$_[0] + a} sub {length}, 0'\''
' 3<<'LAZYTEST_EOF'
45
4905
494550
1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=230
lazytest_case 'ni n1000p'\''r length a, se {$_[0] + a} sub {length}, 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=242
lazytest_case 'ni n1000p'\''sub len($) {length $_}; r len a, se {$_[0] + a} \&len, 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=257
lazytest_case 'ni n1000p'\''r a, length a'\'' p'\''r b, se {$_[0] + a} \&b, 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=268
lazytest_case 'ni n1000p'\''r length a, a'\'' p'\''r a, sea {$_[0] + b} 0'\''
' 3<<'LAZYTEST_EOF'
1	45
2	4905
3	494550
4	1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=281
lazytest_case 'ni n100p'\''my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[3], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=291
lazytest_case 'ni n100p'\''r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=323
lazytest_case 'rm -f numbers                 # prevent ni from reusing any existing file
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=324
lazytest_case 'ni nE6gr4 :numbers
' 3<<'LAZYTEST_EOF'
1
10
100
1000
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=335
lazytest_case 'ni nE6gr4 :numbers O
' 3<<'LAZYTEST_EOF'
1000
100
10
1
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=393
lazytest_case 'ni n10 =[\>ten.txt] z\>ten.gz
' 3<<'LAZYTEST_EOF'
ten.gz
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=404
lazytest_case 'ni --explain n10 =[\>ten.txt] z\>ten.gz
' 3<<'LAZYTEST_EOF'
["n",1,11]
["divert",["file_write","ten.txt"]]
["sh","gzip"]
["file_write","ten.gz"]
LAZYTEST_EOF
lazytest_file='doc/ni_by_example_6.md'
lazytest_line=511
lazytest_case 'ni --explain /usr/share/dict/words rx40 r10 p'\''r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'\''
' 3<<'LAZYTEST_EOF'
["cat","/usr/share/dict/words"]
["row_every",40]
["head","-n",10]
["perl_mapper","r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)"]
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=14
lazytest_case 'ni n5p'\''a * a'\''                 # square some numbers
' 3<<'LAZYTEST_EOF'
1
4
9
16
25
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=26
lazytest_case 'ni ::plfoo[n4p'\''a*a'\''] //:plfoo
' 3<<'LAZYTEST_EOF'
1
4
9
16
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=38
lazytest_case 'ni +p'\''1..5'\''                   # nothing happens here
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=39
lazytest_case 'ni +n1p'\''1..5'\''                 # the single input row causes `p` to run
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=45
lazytest_case 'ni 1p'\''"hello"'\''                # 1 == n1
' 3<<'LAZYTEST_EOF'
hello
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=57
lazytest_case 'ni n4p'\''r a, a + 1'\''                    # generate two columns
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=62
lazytest_case 'ni n4p'\''r a, a + 1'\'' p'\''r a + b'\''         # ... and sum them
' 3<<'LAZYTEST_EOF'
3
5
7
9
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=87
lazytest_case 'ni /etc/passwd F::r3
' 3<<'LAZYTEST_EOF'
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=91
lazytest_case 'ni /etc/passwd F::r3p'\''r F_ 0..3'\''
' 3<<'LAZYTEST_EOF'
root	x	0	0
daemon	x	1	1
bin	x	2	2
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=95
lazytest_case 'ni /etc/passwd F::r3p'\''r F_ 1..3'\''
' 3<<'LAZYTEST_EOF'
x	0	0
x	1	1
x	2	2
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=99
lazytest_case 'ni /etc/passwd F::r3p'\''r scalar F_'\''            # number of fields
' 3<<'LAZYTEST_EOF'
7
7
7
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=109
lazytest_case 'ni /etc/passwd F::r3p'\''r F_ 3..FM'\''
' 3<<'LAZYTEST_EOF'
0	root	/root	/bin/bash
1	daemon	/usr/sbin	/bin/sh
2	bin	/bin	/bin/sh
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=113
lazytest_case 'ni /etc/passwd F::r3p'\''r FR 3'\''         # FR(n) == F_(n..FM)
' 3<<'LAZYTEST_EOF'
0	root	/root	/bin/bash
1	daemon	/usr/sbin	/bin/sh
2	bin	/bin	/bin/sh
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=139
lazytest_case 'ni n2p'\''a, a + 100'\''                    # return without "r"
' 3<<'LAZYTEST_EOF'
1
101
2
102
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=144
lazytest_case 'ni n2p'\''r a, a + 100'\''                  # use "r" for side effect, return ()
' 3<<'LAZYTEST_EOF'
1	101
2	102
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=147
lazytest_case 'ni n3p'\''r $_ for 1..a'\''                 # use r imperatively, implicit return
' 3<<'LAZYTEST_EOF'
1
1
2
1
2
3
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=159
lazytest_case 'ni n3p'\''[a, a+1, a+2]'\''
' 3<<'LAZYTEST_EOF'
1	2	3
2	3	4
3	4	5
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=170
lazytest_case 'cat > functions.pm <<'\''EOF'\''
package ni::pl;       # important! this is the package where your perl code runs
sub normalize
{
  my $sum = sum @_;   # required code can call back into ni functions
  map $_ / ($sum || 1), @_;
}
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=178
lazytest_case 'ni pRfunctions.pm 1p'\''r normalize 1, 2, 5'\''
' 3<<'LAZYTEST_EOF'
0.125	0.25	0.625
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=215
lazytest_case 'nc -l 3001 <<'\''EOF'\'' > /dev/null &
HTTP/1.1 200 OK
Content-Length: 54

package ni::pl;
sub this_worked { r "it worked", @_ }
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=222
lazytest_case 'sleep 1
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=223
lazytest_case 'ni pRhttp://localhost:3001 1p'\''this_worked 5, 6, 7'\''
' 3<<'LAZYTEST_EOF'
it worked	5	6	7
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=240
lazytest_case 'ni n10p'\''r ru {a%4 == 0}'\''              # read forward until a multiple of 4
' 3<<'LAZYTEST_EOF'
1	2	3
4	5	6	7
8	9	10
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=251
lazytest_case 'ni n10p'\''r map a*$_, 1..10'\'' =\>mult-table
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
lazytest_file='doc/perl.md'
lazytest_line=262
lazytest_case 'ni mult-table p'\''r g_ ru {a%4 == 0}'\''   # extract seventh column from each line
' 3<<'LAZYTEST_EOF'
7	14	21
28	35	42	49
56	63	70
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=279
lazytest_case 'ni ::squares[n100p'\''100 - a'\'' p'\''r a, a*a'\''] \
     n5p'\''^{@sq{a_ squares} = b_ squares} $sq{a()}'\''
' 3<<'LAZYTEST_EOF'
1
4
9
16
25
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=293
lazytest_case 'ni ::squares[n100p'\''100 - a'\'' p'\''r a, a*a'\''] n5p'\''^{%sq = ab_ squares} $sq{a()}'\''
' 3<<'LAZYTEST_EOF'
1
4
9
16
25
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=314
lazytest_case 'ni n100p'\''sum rw {1}'\''
' 3<<'LAZYTEST_EOF'
5050
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=316
lazytest_case 'ni n10p'\''prod rw {1}'\''
' 3<<'LAZYTEST_EOF'
3628800
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=318
lazytest_case 'ni n100p'\''mean rw {1}'\''
' 3<<'LAZYTEST_EOF'
50.5
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=326
lazytest_case 'ni n1p'\''cart [1,2], [1,2,3], ["a","b"]'\''
' 3<<'LAZYTEST_EOF'
1	1	a
1	1	b
1	2	a
1	2	b
1	3	a
1	3	b
2	1	a
2	1	b
2	2	a
2	2	b
2	3	a
2	3	b
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=357
lazytest_case 'ni n10000p'\''sr {$_[0] + a} 0'\''
' 3<<'LAZYTEST_EOF'
50005000
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=375
lazytest_case 'ni /etc/passwd F::gGp'\''r g, se {"$_[0]," . a} \&g, ""'\''
' 3<<'LAZYTEST_EOF'
/bin/bash	,root
/bin/false	,syslog
/bin/sh	,backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	,sync
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=396
lazytest_case 'ni n100p'\''my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[2], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=406
lazytest_case 'ni n100p'\''r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'\''
' 3<<'LAZYTEST_EOF'
5050	50.5	1	100
LAZYTEST_EOF
lazytest_file='doc/perl.md'
lazytest_line=434
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
lazytest_file='doc/perl.md'
lazytest_line=482
lazytest_case 'ni /etc/passwd FWpsplit// r/[a-z]/gcx
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
# All of these tests require Docker, so skip if we don't have it
if ! [[ $SKIP_DOCKER ]]; then
cat <<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/pyspark.md'
lazytest_line=13
lazytest_case 'ni Cgettyimages/spark[PL[n10] \<o]
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
fi              # $SKIP_DOCKER
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
lazytest_line=40
lazytest_case 'ni n10 rs3
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=48
lazytest_case 'ni n10000rx4000               # take the 1st of every 4000 rows
' 3<<'LAZYTEST_EOF'
1
4001
8001
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=52
lazytest_case 'ni n10000r.0002               # sample uniformly, P(row) = 0.0002
' 3<<'LAZYTEST_EOF'
1
6823
8921
9509
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=80
lazytest_case 'ni n10000r/[42]000$/
' 3<<'LAZYTEST_EOF'
2000
4000
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=83
lazytest_case 'ni n1000r/[^1]$/r3
' 3<<'LAZYTEST_EOF'
2
3
4
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=98
lazytest_case 'ni n10000rp'\''$_ % 100 == 42'\'' r3
' 3<<'LAZYTEST_EOF'
42
142
242
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=117
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
lazytest_line=133
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
lazytest_line=145
lazytest_case 'ni n10rB | wc -l              # no field B here, so no output
' 3<<'LAZYTEST_EOF'
0
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=150
lazytest_case 'ni n10p'\''r a; ""'\'' rA | wc -l   # remove blank lines
' 3<<'LAZYTEST_EOF'
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=160
lazytest_case 'ni n100 riA[n4 p'\''a*2'\'']        # intersect column A with values from n4 p'\''a*2'\''
' 3<<'LAZYTEST_EOF'
2
4
6
8
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=170
lazytest_case 'ni n10 rIAn5
' 3<<'LAZYTEST_EOF'
6
7
8
9
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=183
lazytest_case 'ni n100n10gr4                 # g = '\''group'\''
' 3<<'LAZYTEST_EOF'
1
1
10
10
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=188
lazytest_case 'ni n100n100gur4               # u = '\''uniq'\''
' 3<<'LAZYTEST_EOF'
1
10
100
11
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=202
lazytest_case 'ni n100or3                    # o = '\''order'\'': sort numeric ascending
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=206
lazytest_case 'ni n100Or3                    # O = '\''reverse order'\''
' 3<<'LAZYTEST_EOF'
100
99
98
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=219
lazytest_case 'ni n100p'\''r a, sin(a), log(a)'\'' > data          # generate multicolumn data
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=220
lazytest_case 'ni data r4
' 3<<'LAZYTEST_EOF'
1	0.841470984807897	0
2	0.909297426825682	0.693147180559945
3	0.141120008059867	1.09861228866811
4	-0.756802495307928	1.38629436111989
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=231
lazytest_case 'ni data oBr4
' 3<<'LAZYTEST_EOF'
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=243
lazytest_case 'ni data oBg r4                # '\''g'\'' is a modifier of B, not another sort
' 3<<'LAZYTEST_EOF'
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=248
lazytest_case 'ni data oB g r4               # '\''g'\'' is a sorting operator
' 3<<'LAZYTEST_EOF'
1	0.841470984807897	0
10	-0.54402111088937	2.30258509299405
100	-0.506365641109759	4.60517018598809
11	-0.999990206550703	2.39789527279837
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=260
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
lazytest_line=274
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
lazytest_line=282
lazytest_case 'ni n10p'\''r "a", a'\'' ggABn- r4
' 3<<'LAZYTEST_EOF'
a	10
a	9
a	8
a	7
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=295
lazytest_case 'ni //license FWpF_ > word-list
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=296
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
lazytest_line=307
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
lazytest_line=318
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
lazytest_line=335
lazytest_case 'ni i[foo bar] i[foo car] i[foo dar] i[that no] i[this yes] \
     j[ i[foo mine] i[not here] i[this OK] i[this yipes] ]
' 3<<'LAZYTEST_EOF'
foo	bar	mine
foo	car	mine
foo	dar	mine
this	yes	OK
this	yes	yipes
LAZYTEST_EOF
lazytest_file='doc/row.md'
lazytest_line=347
lazytest_case 'ni i[M N foo] i[M N bar] i[M O qux] i[X Y cat] i[X Z dog] \
     jAB[ i[M N hi] i[X Y bye] ]
' 3<<'LAZYTEST_EOF'
M	N	foo	hi
M	N	bar	hi
X	Y	cat	bye
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=9
lazytest_case 'ni n5m'\''ai * ai'\''               # square some numbers
' 3<<'LAZYTEST_EOF'
1
4
9
16
25
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=20
lazytest_case 'ni ::rbfoo[n4m'\''ai*ai'\''] //:rbfoo
' 3<<'LAZYTEST_EOF'
1
4
9
16
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=37
lazytest_case 'ni n4m'\''r a, ai + 1'\''                   # generate two columns
' 3<<'LAZYTEST_EOF'
1	2
2	3
3	4
4	5
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=42
lazytest_case 'ni n4m'\''r a, ai + 1'\'' m'\''r ai + bi'\''      # ... and sum them
' 3<<'LAZYTEST_EOF'
3
5
7
9
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=58
lazytest_case 'ni /etc/passwd F::r3
' 3<<'LAZYTEST_EOF'
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=62
lazytest_case 'ni /etc/passwd F::r3m'\''r fields[0..3]'\''
' 3<<'LAZYTEST_EOF'
root	x	0	0
daemon	x	1	1
bin	x	2	2
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=66
lazytest_case 'ni /etc/passwd F::r3m'\''r fields[1..3]'\''
' 3<<'LAZYTEST_EOF'
x	0	0
x	1	1
x	2	2
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=70
lazytest_case 'ni /etc/passwd F::r3m'\''r fields.size'\''
' 3<<'LAZYTEST_EOF'
7
7
7
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=83
lazytest_case 'ni n2m'\''[a, ai + 100]'\''                 # multiple lines
' 3<<'LAZYTEST_EOF'
1
101
2
102
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=88
lazytest_case 'ni n2m'\''r a, ai + 100'\''                 # multiple columns
' 3<<'LAZYTEST_EOF'
1	101
2	102
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=104
lazytest_case 'ni n10m'\''r ru {|l| l.ai%4 == 0}'\''       # read forward until a multiple of 4
' 3<<'LAZYTEST_EOF'
1	2	3
4	5	6	7
8	9	10
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=114
lazytest_case 'ni n10m'\''r (1..10).map {|x| ai*x}'\'' =\>mult-table
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
lazytest_file='doc/ruby.md'
lazytest_line=130
lazytest_case 'ni mult-table m'\''r ru {|l| l.ai%4 == 0}.map(&:g)'\''      # access column G
' 3<<'LAZYTEST_EOF'
7	14	21
28	35	42	49
56	63	70
LAZYTEST_EOF
lazytest_file='doc/ruby.md'
lazytest_line=139
lazytest_case 'ni mult-table m'\''r ru {|l| l.ai%4 == 0}.g'\''
' 3<<'LAZYTEST_EOF'
7	14	21
28	35	42	49
56	63	70
LAZYTEST_EOF
lazytest_file='doc/scale.md'
lazytest_line=6
lazytest_case 'ni nE6 p'\''sin(a/100)'\'' rp'\''a >= 0'\'' =\>slow-sine-table e[wc -l]
' 3<<'LAZYTEST_EOF'
500141
LAZYTEST_EOF
lazytest_file='doc/scale.md'
lazytest_line=15
lazytest_case 'ni nE6 S4[p'\''sin(a/100)'\'' rp'\''a >= 0'\''] =\>parallel-sine-table e[wc -l]
' 3<<'LAZYTEST_EOF'
500141
LAZYTEST_EOF
lazytest_file='doc/scale.md'
lazytest_line=17
lazytest_case 'diff <(ni slow-sine-table o) <(ni parallel-sine-table o) | head -n10
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=11
lazytest_case 'mkdir echo-script
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=12
lazytest_case 'echo echo.pl >> echo-script/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=13
lazytest_case 'echo echo.sh >> echo-script/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=14
lazytest_case 'cat > echo-script/echo.sh <<'\''EOF'\''
#!/bin/sh
echo "$@"
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=18
lazytest_case 'cat > echo-script/echo.pl <<'\''EOF'\''
defshort '\''/echo'\'' => pmap q{script_op '\''echo-script'\'', "./echo.sh $_"},
                    shell_command;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=27
lazytest_case 'ni --lib echo-script echo[1 2 3]
' 3<<'LAZYTEST_EOF'
1 2 3
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=34
lazytest_case 'mkdir -p echo2/bin
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=35
lazytest_case '{ echo echo2.pl; echo bin/echo2; } > echo2/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=36
lazytest_case 'cat > echo2/echo2.pl <<'\''EOF'\''
defshort '\''/echo2'\'' => pmap q{script_op '\''echo2'\'', "bin/echo2 $_"},
                     shell_command;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=40
lazytest_case 'cat > echo2/bin/echo2 <<'\''EOF'\''
#!/bin/sh
echo "$# argument(s)"
echo "$@"
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/script.md'
lazytest_line=52
lazytest_case 'ni --lib echo2 echo2'\''foo bar'\''
' 3<<'LAZYTEST_EOF'
2 argument(s)
foo bar
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=6
lazytest_case 'mkdir sqlite-profile
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=7
lazytest_case 'echo sqlite.pl > sqlite-profile/lib
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=8
lazytest_case 'cat > sqlite-profile/sqlite.pl <<'\''EOF'\''
defoperator sqlite => q{
  my ($db, $query) = @_;
  exec '\''sqlite'\'', '\''-separator'\'', "\t", $db, $query;
};
defsqlprofile S => pmap q{sqlite_op $$_[0], $$_[1]},
                        pseq pc filename, sql_query;
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=21
lazytest_case 'sqlite test.db <<'\''EOF'\''
CREATE TABLE foo(x int, y int);
INSERT INTO foo(x, y) VALUES (1, 2);
INSERT INTO foo(x, y) VALUES (3, 4);
INSERT INTO foo(x, y) VALUES (5, 6);
EOF
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=27
lazytest_case 'ni --lib sqlite-profile QStest.db foo[rx=3]
' 3<<'LAZYTEST_EOF'
3	4
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=29
lazytest_case 'ni --lib sqlite-profile QStest.db foo rx=3
' 3<<'LAZYTEST_EOF'
3	4
LAZYTEST_EOF
lazytest_file='doc/sql.md'
lazytest_line=31
lazytest_case 'ni --lib sqlite-profile QStest.db foo Ox
' 3<<'LAZYTEST_EOF'
5	6
3	4
1	2
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=6
lazytest_case 'echo test > foo
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=7
lazytest_case 'ni foo
' 3<<'LAZYTEST_EOF'
test
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=14
lazytest_case 'ni foo foo
' 3<<'LAZYTEST_EOF'
test
test
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=23
lazytest_case 'echo test | gzip > fooz
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=24
lazytest_case 'ni fooz
' 3<<'LAZYTEST_EOF'
test
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=26
lazytest_case 'cat fooz | ni
' 3<<'LAZYTEST_EOF'
test
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=39
lazytest_case 'ni n4                         # integer generator
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=44
lazytest_case 'ni n04                        # integer generator, zero-based
' 3<<'LAZYTEST_EOF'
0
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=71
lazytest_case 'ni ::word[1p'\''pretty'\''] n3 w[np'\''r word'\'']
' 3<<'LAZYTEST_EOF'
1	pretty
2	pretty
3	pretty
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=82
lazytest_case 'ni ifoo                       # literal text
' 3<<'LAZYTEST_EOF'
foo
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=84
lazytest_case 'ni i[foo bar]                 # literal two-column text
' 3<<'LAZYTEST_EOF'
foo	bar
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=86
lazytest_case 'ni i[ foo[] [bar] ]           # literal two-column text with brackets
' 3<<'LAZYTEST_EOF'
foo[]	[bar]
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=93
lazytest_case 'ni ::word[ipretty] n3 w[np'\''r word'\'']
' 3<<'LAZYTEST_EOF'
1	pretty
2	pretty
3	pretty
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=101
lazytest_case 'ni e'\''seq 4'\''                  # output of shell command "seq 4"
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=111
lazytest_case 'ni 1p'\''hi'\'' +1p'\''there'\''
' 3<<'LAZYTEST_EOF'
hi
there
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=121
lazytest_case 'ni 1p'\''hi'\''1p'\''there'\''
' 3<<'LAZYTEST_EOF'
hi1pthere
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=135
lazytest_case 'ni n3 | sort
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=139
lazytest_case 'ni n3 e'\''sort'\''                 # without +, e acts as a filter
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=143
lazytest_case 'ni n3e'\''sort -r'\''
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=147
lazytest_case 'ni n3e[ sort -r ]             # easy way to quote arguments
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=151
lazytest_case 'ni n3e[sort -r]
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=160
lazytest_case 'ni n3 g       # g = sort
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=164
lazytest_case 'ni n3g        # no need for whitespace
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=168
lazytest_case 'ni n3gA-      # reverse-sort by first field
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=172
lazytest_case 'ni n3O        # NOTE: capital O, not zero; more typical reverse numeric sort
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=192
lazytest_case 'mkdir test-dir
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=193
lazytest_case 'touch test-dir/{a,b,c}
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=194
lazytest_case 'ni e'\''ls test-dir/*'\''                   # e'\'''\'' sends its command through sh -c
' 3<<'LAZYTEST_EOF'
test-dir/a
test-dir/b
test-dir/c
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=198
lazytest_case 'ni e[ls test-dir/*] 2>/dev/null || :  # e[] uses exec() directly; no wildcard expansion
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=199
lazytest_case 'ni e[ ls test-dir/* ]                 # using whitespace avoids this problem
' 3<<'LAZYTEST_EOF'
test-dir/a
test-dir/b
test-dir/c
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=238
lazytest_case '{ echo hello; echo world; } > hw
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=239
lazytest_case 'ni n3 +hw
' 3<<'LAZYTEST_EOF'
1
2
3
hello
world
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=245
lazytest_case 'ni n3 ^hw
' 3<<'LAZYTEST_EOF'
hello
world
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=251
lazytest_case 'ni hw =e[wc -l]               # output from '\''wc -l'\'' is gone
' 3<<'LAZYTEST_EOF'
hello
world
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=260
lazytest_case 'ni n4                         # integer generator
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=265
lazytest_case 'ni n04                        # integer generator, zero-based
' 3<<'LAZYTEST_EOF'
0
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=270
lazytest_case 'ni ifoo                       # literal text
' 3<<'LAZYTEST_EOF'
foo
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=281
lazytest_case 'ni n3 e'\''sort'\''
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=285
lazytest_case 'ni n3e'\''sort -r'\''
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=289
lazytest_case 'ni n3e[sort -r]
' 3<<'LAZYTEST_EOF'
3
2
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=312
lazytest_case 'mkdir test-dir
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=313
lazytest_case 'touch test-dir/{a,b,c}
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=314
lazytest_case 'ni e'\''ls test-dir/*'\''                   # e'\'''\'' sends its command through sh -c
' 3<<'LAZYTEST_EOF'
test-dir/a
test-dir/b
test-dir/c
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=318
lazytest_case 'ni e[ls test-dir/*] 2>/dev/null || :  # e[] uses exec() directly; no wildcard expansion
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=319
lazytest_case 'ni e[ ls test-dir/* ]                 # using whitespace avoids this problem
' 3<<'LAZYTEST_EOF'
test-dir/a
test-dir/b
test-dir/c
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=329
lazytest_case 'ni n3 >file                   # nothing goes to the terminal
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=330
lazytest_case 'ni file
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=339
lazytest_case 'ni n3 \>file2                 # writes the filename to the terminal
' 3<<'LAZYTEST_EOF'
file2
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=341
lazytest_case 'ni file2
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=345
lazytest_case 'ni n3 =\>file3                # eats the filename because \> happens inside =
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=349
lazytest_case 'ni file3
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=359
lazytest_case 'ni n4 \>file3 \<
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=373
lazytest_case '{ echo foo; echo bar; } > file1
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=374
lazytest_case 'echo bif > file2
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=375
lazytest_case 'ni ifile1 ifile2 \<       # regular file-read on multiple files
' 3<<'LAZYTEST_EOF'
foo
bar
bif
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=379
lazytest_case 'ni ifile1 ifile2 W\<      # prepend-file read on multiple files
' 3<<'LAZYTEST_EOF'
file1	foo
file1	bar
file2	bif
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=389
lazytest_case 'ni ifile1 ifile2 W\< p'\''r a.".txt", b'\'' W\>
' 3<<'LAZYTEST_EOF'
file1.txt
file2.txt
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=392
lazytest_case 'cat file1.txt
' 3<<'LAZYTEST_EOF'
foo
bar
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=395
lazytest_case 'cat file2.txt
' 3<<'LAZYTEST_EOF'
bif
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=403
lazytest_case 'ni n3z >file3.gz
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=404
lazytest_case 'zcat file3.gz
' 3<<'LAZYTEST_EOF'
1
2
3
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=413
lazytest_case 'ni igzip z | gzip -dc                 # gzip by default
' 3<<'LAZYTEST_EOF'
gzip
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=415
lazytest_case 'ni igzip zg | gzip -dc                # explicitly specify
' 3<<'LAZYTEST_EOF'
gzip
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=417
lazytest_case 'ni igzip zg9 | gzip -dc               # specify compression level
' 3<<'LAZYTEST_EOF'
gzip
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=419
lazytest_case 'ni ixz zx | xz -dc
' 3<<'LAZYTEST_EOF'
xz
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=421
lazytest_case 'ni ilzo zo | lzop -dc
' 3<<'LAZYTEST_EOF'
lzo
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=423
lazytest_case 'ni ibzip2 zb | bzip2 -dc
' 3<<'LAZYTEST_EOF'
bzip2
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=439
lazytest_case 'ni n4 z zd
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=444
lazytest_case 'ni n4 zd
' 3<<'LAZYTEST_EOF'
1
2
3
4
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=455
lazytest_case 'ni n4 zn | wc -c
' 3<<'LAZYTEST_EOF'
0
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=464
lazytest_case 'ni n1000000gr4
' 3<<'LAZYTEST_EOF'
1
10
100
1000
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=475
lazytest_case 'ni n1000000gr4 :numbers
' 3<<'LAZYTEST_EOF'
1
10
100
1000
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=485
lazytest_case 'ni n1000000gr4 :numbers O
' 3<<'LAZYTEST_EOF'
1000
100
10
1
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=496
lazytest_case 'echo '\''checkpointed'\'' > numbers
' 3<<'LAZYTEST_EOF'
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=497
lazytest_case 'ni n1000000gr4 :numbers O
' 3<<'LAZYTEST_EOF'
checkpointed
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=505
lazytest_case 'ni n100000z :biglist r+5
' 3<<'LAZYTEST_EOF'
99996
99997
99998
99999
100000
LAZYTEST_EOF
lazytest_file='doc/stream.md'
lazytest_line=511
lazytest_case 'ni n100000z :biglist r+5
' 3<<'LAZYTEST_EOF'
99996
99997
99998
99999
100000
LAZYTEST_EOF
lazytest_file='doc/warnings.md'
lazytest_line=14
lazytest_case 'ni n1000000 =\>not-a-million-things r5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/warnings.md'
lazytest_line=20
lazytest_case 'echo $(( $(cat not-a-million-things 2>/dev/null | wc -l) < 1000000 ))
' 3<<'LAZYTEST_EOF'
1
LAZYTEST_EOF
lazytest_file='doc/warnings.md'
lazytest_line=52
lazytest_case 'ni n1000000 =\>a-million-things gr5
' 3<<'LAZYTEST_EOF'
1
10
100
1000
10000
LAZYTEST_EOF
lazytest_file='doc/warnings.md'
lazytest_line=58
lazytest_case 'wc -l < a-million-things
' 3<<'LAZYTEST_EOF'
1000000
LAZYTEST_EOF
lazytest_file='doc/warnings.md'
lazytest_line=66
lazytest_case 'ni n1000000 :a-million-things-2 r5
' 3<<'LAZYTEST_EOF'
1
2
3
4
5
LAZYTEST_EOF
lazytest_file='doc/warnings.md'
lazytest_line=72
lazytest_case 'wc -l < a-million-things-2
' 3<<'LAZYTEST_EOF'
1000000
LAZYTEST_EOF
lazytest_end
