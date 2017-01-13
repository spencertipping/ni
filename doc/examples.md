# Examples of ni misuse
All of these use `ni --js` (see [visual.md](visual.md) for a brief overview).
If you're working through these on the command line, you can use `ni
--explain` to help figure out what's going on:

```sh
$ ni --explain //ni FWpF_ plc gc
["meta_image"]
["split_regex","(?^:[^\\w\\n]+)"]
["perl_mapper","F_"]
["perl_mapper","lc"]
["row_sort","-t","\t"]
["count"]
```

**NOTE:** Some of the screenshots here are from older versions of ni, so some
details may have changed since.

## Simple 2D letter/letter co-occurrence matrix
![img](http://spencertipping.com/ni-example-letter-cooccurrence.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2Fusr%2Fshare%2Fdict%2Fwords%20plc%20pr%2F%5Ba-z%5D%2Fg%20pcart%5BF_%5D%2C%5BF_%5D%20p'r%20map%20ord%2C%20F_'%20%2CjAB%2C%22%2C%22vm%22%3A%5B1%2C0%2C0%2C0%2C0%2C1%2C0%2C0%2C0%2C0%2C1%2C0%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.1348179443582629%7D
```

Equivalent ni command:

```sh
$ ni /usr/share/dict/words plc pr/[a-z]/g pcart[F_],[F_] p'r map ord, F_'
```

### How it works
- `/usr/share/dict/words`: cat the file; we get one word per line
- `plc`: short for `p'lc $_'`: lowercase each line
- `pr/[a-z]/g`: short for `p'r /[a-z]/g'`:
  - `/[a-z]/g`: in list context, return every occurrence of a lowercase letter
  - `r(@list)`: join with tabs and print an output line
- `pcart[F_],[F_]`: short for `p'cart [F_], [F_]'`:
  - `F_`: return a list of tab-delimited fields (the lowercase letters from
    `pr/[a-z]/g`)
  - `cart [@xs], [@ys], ...`: Cartesian product of N arrays: in this case,
    returns `[$xi, $yi]` pairs, which represent every combination of lowercase
    letters within this word
  - `cart` returns array references, which ni automatically joins with tabs to
    convert to output rows.

At this point we have each pair of co-occurring letters on a single line,
tab-delimited. The last step is to convert to ASCII:

- `p'r map ord, F_`: convert each tab-delimited field to its ASCII value:
  - `F_`: the list of fields
  - `map ord, @list`: short for `map ord($_), @list`: convert each list element
    to ASCII
  - `r @list`: tab-join and write output line

This gives us a long stream of integer pairs. If we plot them now, we'll get a
bunch of dots on the screen:

![img](http://spencertipping.com/ni-example-letter-cooccurrence-dots.png)

If, however, we move each dot by a random vector chosen from a 0.9x0.9
rectangle, we'll end up with stochastically-shaded squares with visible
boundaries. This is a common thing to do when dot plotting, so ni provides the
"jitter" cell operator.

- `,`: enter cell-transform context (essentially a new namespace for letters)
  - `jAB,`: jitter cells in columns A and B uniformly;
    - `,`: ...by 0.9. ni provides this shorthand because it's common to do
      this. (The alternative would be `,jAB.9`, but that's extra typing with a
      lot of right-hand travel.)

## Simple 3D sine wave
![img](http://spencertipping.com/ni-example-simple-3dsine.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22nE3p'r%20a%2C%20%24_%20for%200..999'%20r'%2F0(%5C%5Ct%7C%24)%2F'%20p'r%20a%2C%20sin(a%2F100)*sin(b%2F100)%2C%20b'%20%2B%5Bid%3A0%2C4%2C0%20id%3A0%2C-4%2C0%20FC%5D%22%2C%22vm%22%3A%5B0.9900140741662761%2C0%2C0.14096855306306652%2C0.038856304985337244%2C-0.043561678568934545%2C0.9510565162951593%2C0.30593117358776106%2C0.03263707571801566%2C-0.13406906098332957%2C-0.3090169943749491%2C0.9415593364597573%2C0%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A0.8750031755957807%7D
```

Equivalent ni command:

```sh
$ ni nE3p'r a, $_ for 0..999' r'/0(\t|$)/' p'r a, sin(a/100)*sin(b/100), b'
```

### How it works
- `nE3`: generate integers from 1 to 1000, inclusive. `E3` is shorthand for
  `1e3`, which is 1000.
- `p'r a, $_ for 0..999'`: for each row, emit 1000 rows, one for each value in
  the second column. This emits the plane of input points for which we'll
  evaluate the product of two sines.
- `r'/0(\t|$)/'`: punches holes in the plane to form a grid. I'll explain this
  below and show what it looks like if we omit it.
- `p'r a, sin(a/100)*sin(b/100), b'`: the plane coordinates are `a` and `b`, so
  for each plane point we emit a 3D point at `<a, f(a, b), b>`.
- `@[i0,4,0 i0,-4,0 FC]`: two extra data points to expand the min/max along
  the Y axis. This results in flatter output.

### `r'/0(\t|$)/'` for the grid
If we preview just `nE3p'r a, $_ for 0..999'`, we'll see a uniform plane:

![img](http://spencertipping.com/ni-example-simple-3dsine-plane.png)

We'd have a grid if every point were divisible by 10 along either the X or the
Y axis. We could use Perl like this: `rp'(a%10==0) || (b%10==0)'`, but an
equivalent assertion is that either of the two numbers ends in a zero. The
regex `/0(\t|$)/` detects this case: the first number is followed by a tab, the
second number by the end of the line.

![img](http://spencertipping.com/ni-example-simple-3dsine-grid.png)

### View scaling
This uses a sub-ni instance to append some data to the stream. We just need two
more data points whose Y coordinates expand the range to [-4, 4] so the
plotting interface scales the sine wave down vertically.

- `+[...]`: append a new stream:
  - `i0,4,0`: append the literal text `0,4,0`
  - `i0,-4,0`: append the literal text `0,-4,0`
  - `FC`: fieldsplit on commas: this turns commas into tabs so the two points
    occupy three columns each.

## Simple ASCII co-occurrence of ni source code
![img](http://spencertipping.com/ni-example-ascii-cooccurrence.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2F%2Fni%20psplit%2F%2F%20pord%20p'r%20pl%203'%20%2CjABC.5%22%2C%22vm%22%3A%5B0.3052176877315164%2C0%2C-0.9522826067380442%2C0.025659824046920767%2C0.15198698116928847%2C0.9871813119337595%2C0.04871360101460337%2C-0.00521898518107074%2C0.9400755930513635%2C-0.15960281128089038%2C0.30130519740018513%2C-0.060644007110305605%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.4140702339178333%7D
```

Equivalent ni command:

```sh
$ ni //ni psplit// pord p'r pl 3'
```

### How it works
- `//ni`: append ni's source code verbatim
- `psplit//`: a compact form of `p'split //'`, which will return each character
  on a separate line.
- `pord`: a compact form of `p'ord $_'`, which will transform each line into
  its ASCII value.
- `p'r pl 3'`: non-destructively read the next three lines and place them on a
  single row. `pl($n)` peeks `n` lines ahead, returning them as an array.

At this point we have triples of ASCII values. The web UI provides some
shading to handle point collisions, but it can only offer so much dynamic
range; typically if you're relying on shading to tell you something, you'll
want to jitter each data point a little. (See the 2D letter/letter
co-occurrence from `/usr/share/dict/words` for an example.)

- `,`: cell transformation context
  - `jABC.5`: jitter columns A, B, and C by a random value whose range is
    centered at 0 and is 0.5 across. ASCII values are integers, so jittering
    each one by 0.5 preserves its identity while adding some spatial resolution
    and better shading to the plot.

## Co-occurrence of manpage words in quasi-donut form
![img](http://spencertipping.com/ni-example-cooccurrence-quasidonut.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2Fusr%2Fshare%2Fman%2Fman1%20%5C%5C%3CFWp'r%20F_(%24_-2..%24_)%20for%202..FM'%20%2ChABzC%20p'r%20prec((a%20%26%200xffff)%20%2B%200xffff%2C%20int((b%20%26%200xffff)%20%2F%200xffff*270))%2C%20c'%20fACB%22%2C%22vm%22%3A%5B-0.9841092569237042%2C0%2C-0.17756398969608223%2C0.07311827956989259%2C0.03298935921795422%2C0.9825897456859218%2C-0.18283624873452747%2C0.4633668730031738%2C0.17447255547845722%2C-0.185788567121162%2C-0.9669756644878278%2C-0.06165651783795804%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A0.6482182956356948%7D
```

Equivalent ni command:

```sh
$ ni /usr/share/man/man1 \<FWp'r F_($_-2..$_) for 2..FM' \
  ,hABzC p'r prec((a & 0xffff) + 0xffff, int((b & 0xffff) / 0xffff*270)), c' \
  fACB
```

### How it works
- `/usr/share/man/man1` produces a list of filenames (all manpages in `man1`)
- `\<` cats each file in a stream of filenames (all manpage text in `man1`)
- `FW` splits on non-word characters
- `p'r F_($_-2..$_) for 2..FM'` is a 3-wide sliding window of words per line:
  - `2..FM` generates the index of the last word in each window
  - `$_-2..$_` generates the indexes of all words in the current window
  - `r F_(@indexes)` outputs a row of fields at numerically-specified
    `@indexes`

A preview of the output at this point:

```sh
$ ni /usr/share/man/man1 \<FWp'r F_($_-2..$_) for 2..FM' r10
        DO      NOT
DO      NOT     MODIFY
NOT     MODIFY  THIS
MODIFY  THIS    FILE
THIS    FILE    It
FILE    It      was
It      was     generated
was     generated       by
generated       by      help2man
by      help2man        1
```

Now we use cell operators to transform things into numbers.

- `,`: the cell operator prefix, which continues until the next CLI argument:
  - `hAB`: 32-bit murmurhash each cell in columns A and B (unbiased)
  - `zC`: assign successive integers to distinct values in column C (biased)

At this point we have a cube of word cooccurrence:

![img](http://spencertipping.com/ni-example-cooccurrence-hhz-cube.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2Fusr%2Fshare%2Fman%2Fman1%20%5C%5C%3CFWp'r%20F_(%24_-2..%24_)%20for%202..FM'%20%2ChABzC%22%2C%22vm%22%3A%5B-0.1455274825751139%2C0%2C-0.9893542094796254%2C-0.04019689987431912%2C0.4626508778259456%2C0.8839247529105698%2C-0.06805289441946762%2C-0.01019337018835886%2C0.874514675155316%2C-0.46762915990349385%2C-0.12863534407689978%2C-0.08768024724094528%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.6594267918484475%7D
```

I haven't implemented axes and range display yet, but `A` and `B` take on the
full range of 32-bit integer values and `C` contains positive integers up to
the number of distinct words we have -- we can calculate it easily, in this
case by just taking the maximum:

```sh
$ ni /usr/share/man/man1 \<FWpF_ ,z Or1
84480
```

Alternatively, we can reverse-sort within the UI and use the preview window:

![img](http://spencertipping.com/ni-example-cooccurrence-hhz-preview.png)

ni provides a quasidonut constructor library in the form of two functions,
`prec(rho, theta) = (x, y)` and `rpol(x, y) = (rho, theta)`. These convert
between rectangular and polar coordinates. In this case we want `prec`, and
here's what we're doing with it:

```pl
(a & 0xffff)            # low 16 bits of the hash (just to truncate; could be pretty much anything)
(a & 0xffff) + 0xffff   # push to the upper half of the range [0, 0x1ffff]: this increases the radius and creates a ring

(b & 0xffff) / 0xffff   # truncate b to the range [0, 1]
... / 0xffff * 270      # now extend to the range [0, 270]: 3/4 of a donut

r prec(rho, theta), c   # output x and y as points on circles, z is preserved
```

From there we just flip into the viewport coordinate system, where Z points
away from the camera.
