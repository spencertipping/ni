# Insane project
Let's see if it's easy to build a trivial NLP thing so we can use it as a
human-facing RMI protocol. No clue whether this will work.

```sh
$ ni /mnt/t9/data/enwiki.lz4 S24p'/([A-Z][\w ,;-]+?\.)\W/g' S4r/the/ \
     z4\>sentences
$ ni sentences S48p'++$w{+lc} for /\w+/g;
                    END{print "$_\t$w{$_}\n" for keys %w}; ()' \
     gp'r a, sum b_ rea' x Oz\>common-words
$ ni common-words ,sAr+1
529912512
$ ni common-words ,sA Wn p'r a, b / 529912512' rxE2
100     0.483682919719397
200     0.545154816046314
300     0.581146028875046
400     0.607779621553831
500     0.630126240536853
600     0.649357194268325
700     0.665766089705011
800     0.679974991796382
900     0.692529698185349
1000    0.703975693255569
1100    0.714340290949763
1200    0.723881499895596
1300    0.732678467120248
1400    0.740793846362322
1500    0.748301291289382
1600    0.755294506048576
1700    0.761841862303489
1800    0.768029587495379
1900    0.773901770034069
2000    0.779486312638717
2100    0.784763317685165
2200    0.789744377653042
2300    0.794479127150716
2400    0.798964090132675
2500    0.803198515531561
2600    0.807236668908848
2700    0.811117090588723
2800    0.814825355548502
2900    0.818388970215521
3000    0.821806791004776
```

We get almost half the total word usage in just the 100 most common, so let's
start with that. Now the goal is to build context around these words.

```sh
$ ni ::cw[common-words fBrE2] \
     sentences S24[p'split /\W+/' p'r pl 3' rp'^{%w = aa_ cw} $w{+b}'] \
     gcOz\>3grams
```
