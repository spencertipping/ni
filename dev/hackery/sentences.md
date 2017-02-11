# Insane project
Let's see if it's easy to build a trivial NLP thing so we can use it as a
human-facing RMI protocol. No clue whether this will work.

```sh
r1$ ni /mnt/t9/data/enwiki.lz4 S24p'/([A-Z][\w ,;-]+?\.)\W/g' S4r/the/ \
       z4\>sentences
r1$ ni sentences S24p'++$w{+lc} for /\w+/g;
                      END{print "$_\t$w{$_}" for keys %w}; ()' \
       gp'r a, sum b_ rea' x Oz\>common-words
```
