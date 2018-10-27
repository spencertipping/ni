# Git interop
**TODO:** convert these to unit tests

ni can use git to read commits, trees, and blobs. The entry point,
`git://repopath`, will give you a list of local and remote branches. For
example:

```sh
$ ni git://.
gitcommit://.:refs/heads/archive/concatenative  6971535d4edc37a28740fccd8a3f09a6158cba29
gitcommit://.:refs/heads/archive/cva    7da9f1e77164f5a6c9b93803b5f8114b657c68b8
gitcommit://.:refs/heads/archive/genopt a99830c4ad3c75cf05b2ee025e26a2281bf9e911
gitcommit://.:refs/heads/archive/lisp   4cab9e54a641262dca5beff0d0b4310100bb6c3c
gitcommit://.:refs/heads/archive/native d4b3be5e97418ac9334773e0ff121f4f25333944
gitcommit://.:refs/heads/archive/perl   1343c5d42c1028f4094d4eaa30dbcf6e2e1d221b
gitcommit://.:refs/heads/archive/self-hosting   dd7ffd268c6718a5900d801a3100907512752c27
gitcommit://.:refs/heads/cheatsheet_v1.0        0ec7b3e405837f83106c38aa5e0514371195ce9d
...
```

Reading a commit will give you a TSV of possible facets:

```sh
$ ni git://. r1 fA
gitcommit://.:refs/heads/archive/concatenative

$ ni git://. r1 fA \<
gitcommitmeta://.:refs/heads/archive/concatenative githistory://.:refs/heads/archive/concatenative gitdiff://.:refs/heads/archive/concatenative gittree://.:refs/heads/archive/concatenative gitpdiff://.:refs/heads/archive/concatenative
```

## Commit metadata
```sh
$ ni git://. r1 fA \< fA
gitcommitmeta://.:refs/heads/archive/concatenative

$ ni git://. r1 fA \< fA \<
tree 6ef2f88a860e26b81eca3649b2f81992c285a199
parent ddfafe9307ca18cd059ed8efa1d3cfdbe98a0ffe
author Spencer Tipping <spencer@spencertipping.com> 1427636233 +0000
committer Spencer Tipping <spencer@spencertipping.com> 1427636233 +0000

Fixed a parse bug
```

## Commit history
There are two variants of this URI scheme: `githistory://repo:ref` and
`gitnmhistory://repo:ref`. `gitnmhistory` excludes merge commits.

```sh
$ ni git://. r1 fA \< fB
githistory://.:refs/heads/archive/concatenative

$ ni git://. r1 fA \< fB \<
gitcommit://.:6971535d4edc37a28740fccd8a3f09a6158cba29  spencer@spencertipping.com      1427636233      Fixed a parse bug
gitcommit://.:ddfafe9307ca18cd059ed8efa1d3cfdbe98a0ffe  spencer@spencertipping.com      1427377653      Preliminary tag support for bootstrap interpreter
gitcommit://.:d360dfe6c6183bdaca0552a1a49a3631e955b312  spencer@spencertipping.com      1427377113      Resolved the problem
gitcommit://.:226c1b8d27fc02129981d0f73535f3e69797c8af  spencer@spencertipping.com      1427293623      Minor changes, still thinking about the untyped-data problem. No great ideas yet apart from adding value metadata (which seems wrong).
gitcommit://.:c062e734d23ece6b3137354af30abc3da7320d2a  spencer@spencertipping.com      1427289277      Hrm, untyped
...
```

## Commit diffs
```sh
$ ni git://. r1 fA \< fC
gitdiff://.:refs/heads/archive/concatenative

$ ni git://. r1 fA \< fC \<
diff --git a/src/boot-interpreter.pl b/src/boot-interpreter.pl
index cea0535..e21e314 100644
--- a/src/boot-interpreter.pl
+++ b/src/boot-interpreter.pl
@@ -79,7 +79,7 @@ sub parse {
     next if $k eq 'comment' || $k eq 'ws';
     if ($k eq 'opener') {
       push @stack, [];
-      push @tags, $+{opener} =~ s/.$//r;
+      push @tags, $+{opener};
     } elsif ($k eq 'closer') {
       my $last = pop @stack;
       die "too many closing brackets" unless @stack;
```

## Processed commit diffs
```sh
$ ni git://. r1 fA \< fE \<
src/boot-interpreter.pl 82:82:-       push @tags, $+{opener};
src/boot-interpreter.pl 83:82:+       push @tags, $+{opener} =~ s/.$//r;
```

## Commit trees
```sh
$ ni git://. r1 fA \< fD
gittree://.:refs/heads/archive/concatenative

$ ni git://. r1 fA \< fD \<
gitblob://.:8320be96e5579a9d559f289f759c63d41af263f0    100644  .gitignore
gitblob://.:222d36ccaa470b62600c8e7e55e1b4d3182af1f6    100644  README.md
gitblob://.:f808160232229a181196f3c9b2efd921b7241a7e    100755  build
gittree://.:c3b8b3b4bbd90198f197894341ca83ce0f49f698    040000  doc
gitblob://.:02d46a53c14e141a8ea43bc39667d83d84977034    100755  gen-tests
gitblob://.:8ec3efe84f53125f611a3785f24968d987e3832b    100755  run-tests
gittree://.:225bd52669273772cc5b88165df6cf598cc7c61f    040000  src
gittree://.:b88775b02aace595d17f7c607e874dbcd91c8e62    040000  tools
gitblob://.:32dc3893527c8b87e601003dd40f6e63b520ad86    100755  verify-transcript
```

## Example: find every revision of `dev/tests.sh`
```sh
$ ni git://. fA \< fB \< fAgu \< fD \< rp'c eq "dev"' fA\< rp'c eq "tests.sh"'
gitblob://.:a653261f3582234f8a5875221acbb892550b3b55    100644  tests.sh
gitblob://.:aeef8ad0315c5b00c7472e75bdf15aa1145e4a70    100644  tests.sh
gitblob://.:f2ed4a2c8d1bf4599d8074ebab2973be6d97aa51    100644  tests.sh
gitblob://.:c3e84a5dfefc7a84cd3476dfc11dafa15921dde6    100644  tests.sh
...
```

You could then use `fA\<` or `fA W\<` to get the contents of each.
