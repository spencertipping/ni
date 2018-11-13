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

You can also see history for a specific path by adding a `::<file>` suffix:

```sh
$ ni githistory://.:master::reference     # history for reference/ only
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

Diffs also support `::<path>`:

```sh
$ ni gitdiff://.:master::ni
```

## Processed commit diffs
These diffs fold the filename and position markers into each output line to make
the diff output easier to machine-read.

```sh
$ ni git://. r1 fA \< fE \<
src/boot-interpreter.pl 82:82:-       push @tags, $+{opener};
src/boot-interpreter.pl 83:82:+       push @tags, $+{opener} =~ s/.$//r;

$ ni gitpdiff://.:master..develop::ni
ni      56:56:- 56 core/boot/ni.map
ni      57:56:+ 57 core/boot/ni.map
ni      521:522:-       84 core/boot/common.pl
ni      522:522:+       90 core/boot/common.pl
ni      599:603:-       BEGIN {defparseralias filename   => palt prx 'file://(.+)',
ni      600:603:+       BEGIN {defparseralias computed   => pmap q{eval "(sub {$_})->()"},
ni      600:604:+                                                pn 1, pstr"\$", prx '.*'}
...
```

## Snapshots
`gitsnap://<repo>:<revision>[::<path>]` will give you a recursive listing of all
`gitblob://` entries at that revision. You can use this with `\<` or `W\<` to
read the full state of the tree at that moment.

```sh
$ ni gitsnap://.:master::core r10
gitblob://.:master::core/archive/7z.pl
gitblob://.:master::core/archive/lib
gitblob://.:master::core/archive/tar.pl
gitblob://.:master::core/archive/xlsx.pl
gitblob://.:master::core/archive/zip.pl
gitblob://.:master::core/assert/assert.pl
gitblob://.:master::core/assert/lib
gitblob://.:master::core/binary/binary.pl
gitblob://.:master::core/binary/bytestream.pm
gitblob://.:master::core/binary/bytewriter.pm

# count lines in each file
$ ni gitsnap://.:master::core r10 W\< fAc
40      gitblob://.:master::core/archive/7z.pl
4       gitblob://.:master::core/archive/lib
36      gitblob://.:master::core/archive/tar.pl
69      gitblob://.:master::core/archive/xlsx.pl
30      gitblob://.:master::core/archive/zip.pl
6       gitblob://.:master::core/assert/assert.pl
1       gitblob://.:master::core/assert/lib
53      gitblob://.:master::core/binary/binary.pl
29      gitblob://.:master::core/binary/bytestream.pm
7       gitblob://.:master::core/binary/bytewriter.pm
```

`gitdsnap://<repo>:<revision>[::<path>]` is identical to `gitsnap://`, except
that it resolves each blob directly to an object ID and provides the object's
path in a separate column:

```sh
$ ni gitdsnap://.:develop::core r10
gitblob://.:1cf22488c96c6659f5340c35e04a64080a15ccb3    core/archive/7z.pl
gitblob://.:d8b31ab1df24f45c538a5ef40990276eaf89b91e    core/archive/lib
gitblob://.:4f68ea5bc47097f2d178092d0c5e5fb3cc7833cc    core/archive/tar.pl
gitblob://.:bd1e4474301515a950aa45b7957bd01a2ea422b8    core/archive/xlsx.pl
gitblob://.:3a9556fc41145f3854a471ddf844b53355645463    core/archive/zip.pl
gitblob://.:b9905bc73446ab35a5bf6409d26de9a29928f9b5    core/assert/assert.pl
gitblob://.:10fc2bed290348cd050e49f5cd2c7cf640a10721    core/assert/lib
gitblob://.:deb7b8f2c18981abd5596cb29c2f37b904a91db6    core/binary/binary.pl
gitblob://.:e5e35b6d6a95c0d48e43eda342506b784bedce14    core/binary/bytestream.pm
gitblob://.:8796e5ca26d1d6869ddd79fd981b400fa9f7c9aa    core/binary/bytewriter.pm

# count lines in each file
$ ni gitdsnap://.:develop::core r10 fA W\< fAc
40      gitblob://.:1cf22488c96c6659f5340c35e04a64080a15ccb3
4       gitblob://.:d8b31ab1df24f45c538a5ef40990276eaf89b91e
36      gitblob://.:4f68ea5bc47097f2d178092d0c5e5fb3cc7833cc
69      gitblob://.:bd1e4474301515a950aa45b7957bd01a2ea422b8
30      gitblob://.:3a9556fc41145f3854a471ddf844b53355645463
6       gitblob://.:b9905bc73446ab35a5bf6409d26de9a29928f9b5
1       gitblob://.:10fc2bed290348cd050e49f5cd2c7cf640a10721
53      gitblob://.:deb7b8f2c18981abd5596cb29c2f37b904a91db6
29      gitblob://.:e5e35b6d6a95c0d48e43eda342506b784bedce14
7       gitblob://.:8796e5ca26d1d6869ddd79fd981b400fa9f7c9aa

# count lines in each file, keeping the file path
$ ni gitdsnap://.:develop::core r10 W\<'(split /\t/)[0]' fABc
40      gitblob://.:1cf22488c96c6659f5340c35e04a64080a15ccb3    core/archive/7z.pl
4       gitblob://.:d8b31ab1df24f45c538a5ef40990276eaf89b91e    core/archive/lib
36      gitblob://.:4f68ea5bc47097f2d178092d0c5e5fb3cc7833cc    core/archive/tar.pl
69      gitblob://.:bd1e4474301515a950aa45b7957bd01a2ea422b8    core/archive/xlsx.pl
30      gitblob://.:3a9556fc41145f3854a471ddf844b53355645463    core/archive/zip.pl
6       gitblob://.:b9905bc73446ab35a5bf6409d26de9a29928f9b5    core/assert/assert.pl
1       gitblob://.:10fc2bed290348cd050e49f5cd2c7cf640a10721    core/assert/lib
53      gitblob://.:deb7b8f2c18981abd5596cb29c2f37b904a91db6    core/binary/binary.pl
29      gitblob://.:e5e35b6d6a95c0d48e43eda342506b784bedce14    core/binary/bytestream.pm
7       gitblob://.:8796e5ca26d1d6869ddd79fd981b400fa9f7c9aa    core/binary/bytewriter.pm
```

## Commit trees
You can browse these using object IDs or `::<path>` suffixes.

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

$ ni gittree://.:master::dev
gitblob://.:097b5a219b412618451e2f6a59c9914d77ed018a    100644  dev/README.md
gitblob://.:daf27ce78755ad67049fd5ca454f3c0b93000ab5    100755  dev/bench
gitblob://.:d4ba52a1976a44ff313ac504da4740ca282b3992    100755  dev/bench-baseline-throughput
gitblob://.:11ff59ce25b12ba0247296e82c0638f8451ec693    100755  dev/bench-json
gitblob://.:50f0be3bc8a6cf5cfaab18398e37c6a075f9a8e9    100755  dev/bench-line-processors
gitblob://.:74e397407335af195ff32f5acef147ea4683edb3    100755  dev/bench-lisp
gitblob://.:7f114c5c98a7f22a32eabc8adcef86d2165a5712    100755  dev/bench-perl
gitblob://.:e17ca75137046aeed580cbe498996a6c08734c1f    100755  dev/bench-sort
gitblob://.:6641e5d2692461406c2b4d1a53150330caa2785a    100755  dev/bench-startup
gitblob://.:e69de29bb2d1d6434b8b29ae775ad8c2e48c5391    100644  dev/benchmarks.log
gittree://.:6547dcb96dd3809b838194cf27728ab70f7e9135    040000  dev/hackery
gitblob://.:af1625e313f4683be187eb6134a30629898036f4    100644  dev/internals.md
gitblob://.:2c56b13c4aa670e3741c75be2531a780212deaa8    100644  dev/json.md
gitblob://.:2664c5ba7533c2c29d3b39610557142d970a9ac6    100644  dev/license-for-testing
gitblob://.:77e94adbb0354012debdbb370ec2e53ed26334f3    100755  dev/nfu
gitblob://.:2e51e5aaabe6d7cc30df3099c8f7196ec54de727    100644  dev/ni_dev_for_masochists_1.md
gittree://.:07800bd7cb6d90ae0a5f1c770f67f8d3ead65496    040000  dev/test-data
gitblob://.:46770d04fae01715ec41048e3b157d52645c7e27    100644  dev/tests.sh
gitblob://.:cd2ffd7f8572f40bc0d461cc7c1e780d2f47b75f    100755  dev/unsdoc
```

Similarly, `gitblob://` returns the contents of an object or a path.

```sh
$ ni gitblob://.:master::ni r5
#!/usr/bin/env perl
$ni::is_lib = caller();
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
Copyright (c) 2016-2018 Spencer Tipping | MIT license
```
