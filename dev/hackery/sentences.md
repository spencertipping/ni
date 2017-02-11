# Insane project
Let's see if it's easy to build a trivial NLP thing so we can use it as a
human-facing RMI protocol. No clue whether this will work.

```sh
$ ni /mnt/t9/data/enwiki.lz4 S24p'/([A-Z][\w ,;-]+?\.)\W/g' S4r/the/ \
     guz4:sentences S48p'++$w{+lc} for /\w+/g;
                         END{print "$_\t$w{$_}\n" for keys %w}; ()' \
     gp'r a, sum b_ rea' x Oz:common-words fA,sAr+1:total-words
466484391
$ ni ::t total-words common-words ,sA Wn p'r a, b / t' rxE2
100     0.464460068075461
200     0.520423183891699
300     0.555983096120359
400     0.583310792922115
500     0.606364861198539
600     0.62603698566197
700     0.642901009736036
800     0.657616929780615
900     0.670801540281334
1000    0.682803613894125
1100    0.693748899735425
1200    0.703763689276369
1300    0.713031840758848
1400    0.721546839495429
1500    0.729500679048444
1600    0.736888283149436
1700    0.743824896812035
1800    0.750397373960579
1900    0.756644395417724
2000    0.762556687561278
2100    0.768094632774111
2200    0.773353674764222
2300    0.778358485311034
2400    0.783120610352855
2500    0.787644378008781
2600    0.791977719143018
2700    0.796115143754081
2800    0.800069629768169
2900    0.803864537023705
3000    0.807526200807006
```

We get almost half the total word usage in just the 100 most common, so let's
start with that. Now the goal is to build context around these words, in
particular by removing all other words so we have sentence templates. Let's
take the first few thousand variant positions, each of which will become a
column in a word matrix.

```sh
$ ni ::cw[common-words fBrE2] \
     sentences S24p'^{%cw = aa_ cw} r map $cw{$_} || "_", split /\W+/' \
     gcOz:layouts p'r scalar(@xs = /_/g), $_' ,sA rE4rp'a < 10000' z:10kv \
     fC.p'^{$i = 0} r join(" ", F_), join ",", map $i++, /_/g' z:indexed
$ ni ::ix indexed ::cw common-words sentences \
     S24p'^{%ix = ab_ ix; %cw = aa_ cw}
          my @w  = split /\W+/;
          my @ns = split /,/, $ix{join " ", map $cw{$_} || "_", @w} || return ();
          my $i  = 0;
          $cw{$w[$_]} || r $w[$_], $ns[$i++] for 0..$#w' \
     gcfBCAz:sparse OCfCABC,sA z:sparse-ordered fB.,zA Oz:grouped rp'a < 1e5' \
     Xz:dense S24[Wn p'r a, l2norm FR 1'] OBz:rownorms

$ ni ::rows[rownorms e'wc -l'] ::colsums[sparse fBC Op'sum b_ rea'] \
     dense S24p'^{@cn = a_ colsums} r map $F[$_] - $cn[$_]/rows, 0..FM' \
     z:densenormed N'u, s, v = linalg.svd(x, full_matrices=False)
                     x = dot(diag(s), u)' z:svd
```
