# `ni` by Example, Chapter 3 (alpha release)

Welcome to the third part of the tutorial. At this point, you've probably activated a certain amount of `ni` productivity by using Hadoop to broadcast your `ni` scripts across hundreds of cores and thousands of jobs.  You should also have a reasonable high-level understanding of how `ni` is a self-modifying quine, and how this allows it to execute across operating systems.

In this chapter, our goal is to multiply your power with two tools: reducers, and Perl. We'll also discuss some more common I/O operations and demonstrate `ni`'s connections to Python and numpy. 

## Perl for `ni`

This section written for an audience that has never worked with the language before, and from a user's (rather than a developer's) perspective. This tutorial encourages you to learn to use Perl's core functions rather than writing your own. For example, we'll cover the somewhat obscure operation of bit shifting but avoid any more than a brief mention of how to write a function in Perl.

Perl is much-maligned for its syntax; much of that malignancy comes from people whose only exposure to the language is hearing about the [Obfuscated Perl Contest](https://en.wikipedia.org/wiki/Obfuscated_Perl_Contest). Perl is also known for its religious overtones; here `ni` author Spencer Tipping drop the scales from your eyes about hte language in this section from his short intro to the language, [Perl in 10 Minutes](https://github.com/spencertipping/perl-in-ten-minutes). 

>Python, Ruby, and even Javascript were designed to be good languages -- and
just as importantly, to **feel** like good languages. Each embraces the
politically correct notion that values are objects by default, distances itself
from UNIX-as-a-ground-truth, and has a short history that it's willing to
revise or forget. These languages are convenient and inoffensive by principle
because that was the currency that made them viable.
<!--- --->
>Perl is different.
<!--- --->
>In today's world it's a neo-noir character dropped into a Superman comic; but 
that's only true because it changed our collective notion of what an accessible
scripting language should look like. People
often accuse Perl of having no design principles; it's "line noise,"
pragmatic over consistent. This is superficially true, but at a deeper level
Perl is uncompromisingly principled in ways that most other languages aren't.
Perl isn't good; it's complicated, and if you don't know it yet, it will
probably change your idea of what a good language should be.



## Perl Syntax

In truth, Perl has a lot of rules that allow for code to be hacked together quickly and easily; there's no way around learning them (as there is in nice-feeling languages), so let's strap in and get this over with.

### Sigils

So far, we've been using without explanation Perl variables that start with the character `$`. This character is referred to as a sigil, and is not a part of the variable name.  Sigils are used in different ways by the Perl interpreter to increase the language's concision.


```
$x = 5;
@x = 1..10;
%x = (foo => 1, bar => 2);
```

Programmers of inoffensive languages may find the above code shocking and abhorrent (as the author of this text once did). That this code works, and that all of the variables defined within would properly be referred to (at least, in brief) as `x`, can trigger a great deal of cognitive dissonance within a reasonably-skilled but Perl-ignorant programmer.

The question here, and its relevance for `ni`, is not whether this language syntax is useful (it is), but how to open our minds and increase our reading skills to take advantage of Perl's unique qualities.

When a Perl variable is brought into existence, its nature is determined by the sigil that precedes it. The explicit use of sigils allows for more variables to be packed into the same linguistic namespace. When creating a variable:

* `my $x` indicates that `$x` is a scalar, for example a string or a number
* `my @x` indicates that `@x` is a zero-indexed array
* `my %x` indicates that `%x` is a hash

What's even cooler is that, because the syntax for all of these is different, we can use all of these:

```
print $x;         # gets the scalar value of $x
print $x[3];      # gets a scalar value from @x at position 3
print $x{"foo"};  # gets a scalar value from %x associated with the value "foo"
```

At first glance, this is very confusing; all of these values start with `$x`--but note that the calling syntax is different for all three; you get a scalar value (i.e `$`) out of a hash by indexing it with curly braces, you get a scalar value out of an array by indexing it with square brackets, and without either of those, Perl knows that you are referring to the scalar value `$x`. The syntax is a little complicated, but it's not tricky.


### Default Variables
While nice languages make you take pains to indicate default values and variables, Perl is not at all nice in this regard.

```bash
$ ni n15 r-5 p'/^(\d)\d*$/'
6
7
8
9
1
1
1
1
1
1
```

Clearly this regex is operating on each line (the lines input were the integers 6 through 15). The way this works is another piece of Perl's uncompromising commitment to coding efficiency; default variables.

```bash
$ ni n15 r-5 p'$_'
6
7
8
9
10
11
12
13
14
15
```

This has returned to us the exact same value as `$ ni n15 r-5` would have, since `$_` stores the value of the input line.



In fact, the code above is operating on the most important of the Perl default variables, `$_`, which stores the value of the input line. Let's look a little closer:


```bash
$ ni n15 r-5 p'$_ =~ /^(\d)\d*$/'
6
7
8
9
1
1
1
1
1
1
```

Default variables show up all over the place in Perl; we will see them 


## Logical Operations

Logical operators come in two flavors, high-precedence and low-precedence. The low-precedence operators 

### Low Precedence: `and`, `or`, `not`


### High Precedence: `&&`, `||`, `!`



### `if`, `else`, `elsif`, `unless`

#### Block Prefix Form

This form is common to almost every programming language in one form or another;

```

#### Inline Form

#### Ternary Operator `... ? ... : ...`


## Statements, Blocks, and Scope

### Statements and Blocks

- An expression is a series of variables, operators, and method calls that evaluates to a single value. 

- A statement forms a complete unit of execution and is terminated with a semicolon `;`.

- A group zero or more statements group together into a block with braces: `{...}`. 


In Perl, a block has its own scope. Blocks can modify and use variables from a higher scope, or define variables in their own scope using the `my` keyword that are inaccessible from blocks outside.


### `my`: Lexical scoping

Perl is lexically scoped: The Perl keyword `my` restricts the scope of a variable to the current block. 


```bash
$ ni n5 p'my $x = a; {my $x = 100;} ++$x'
2
3
4
5
6
```

Let's take a look at this slightly different example, where we've dropped 

```bash
$ ni n5 p'my $y = a; {$x = 100;} ++$x'
101
101
101
101
101
```

> In `ni` you should always use `my` in your perl mapper variables to avoid their accidental persistence.


```bash
$ ni n5 p'my $y = a; {my $x = 100;} defined $x ? "defined" : "not defined"'
not defined
not defined
not defined
not defined
not defined
```

The major exception to the rule of always using `$my` for your Perl mapper variables is in a begin block, described below.

### `p'^{...} ...'`: BEGIN Block


A begin block is indicated by attaching a caret (`^`) to a block of code (enclosed in `{ }`). Begin blocks are most useful for initializing data structures that will be manipulated, and in particular for converting data closures to Perl data structures, as we will see later in this section.

Inside a begin block, the code is evaluated once and factored over the entire remaining Perl code. 

```bash
$ ni n5p'^{$x = 10} $x += a; r a, $x'
1	11
2	13
3	16
4	20
5	25
```

Note that the value of `$x` is persisted between runs of the perl mapper code. Without the begin block, we would have:

```bash
$ ni n5p'$x = 10; $x += a; r a, $x'
1	11
2	12
3	13
4	14
5	15
```

In this Perl mapper, the value of the globally-scoped `$x` variable is being reset with each line to 10. 


### `perldoc`	
This isn't so much a `ni` tool, but the perl docs are very good, and often better than Google/StackOveflow for syntax help. You can get them at `perldoc -f <function>` at the command line.


### Regular Expressions

Covering regular expressions in their entirety is outside the scope of this tutorial, but we'll cover a few pointers. The [Perl Regex Tutorial](https://perldoc.perl.org/perlretut.html) is excellent.

### Regular Expressions
If you need a regex tutorial, [this one](https://regexone.com) looks good to me. Regular expressions are a huge part of Perl, and there are a number of syntaxes for their use of which you should be aware.

#### Efficient Regex Design

Regexes should be designed to fail quickly, so reduce the number of possible evaluation paths.  The use of anchor tags (`^$`) and avoiding `.` when possible is also a good idea. 

####String Slicing with Regex
I don't like the of Perl's `substr` method because it's asymmetric. to recover most of what I like about from Python's string slicing syntax, I use regex.

```bash
$ ni iabcdefgh p'/^(.*).{4}$/'  #[:-4] in Python
abcd
```

```bash
$ ni iabcdefgh p'/^.{3}(.*)$/' #[3:] in Python
defgh
```

```bash
$ ni iabcdefgh p'/^.*(.{2})$/' #[-2:] in Python
gh
```

#### Using Capture Groups

Capture groups are set off using parentheses; to get them explicitly,  syntactic sugar for the more explicit `my @v = $_ =~ /regex/;`

This will get capture groups and store them in `@v`. Using parentheses around `$v` tells perl to use it in a list context, which will allow you to capture one or more groups.

For example:

```bash
$ ni iabcdefgh p'my @v = /^(.)(.)/; r @v'
a	b
```

```bash
$ ni iabcdefgh p'my ($w) = /^(.)/; r $w'
a
```

```bash
$ ni iabcdefgh p'my ($x, $y) = /^(.)(.)/; r $x, $y'
a	b
```


#### Writing good Regular Expressions

When writing a regular expression, you want them to fail as quickly as possible on strings that don't match.

1. Use anchor tags (`^` and `$`) when possible.
2. Try to limit the number of branching "or" paths in your regular expression.


you're expected to know what strings each of the regular expressions displayed below would match, while this tutorial covers the output values and side effects of these methods.

Instead, we will cover what Perl does.

#### Regex Comparison `=~`

Negative matches to a regex can be performed with `!~`.

#### Regex Barriers

Usually regular expressions are set off using forward slashes, however, this means that forward slashes within a regex will have to be escaped. You can use 

#### Regex Interpolation

$foo = 'house';
    'housecat' =~ /$foo/;      # matches
    'cathouse' =~ /cat$foo/;   # matches
    'housecat' =~ /${foo}cat/; # matches


#### Substitution `s///`, Transliteration `tr///` and `y///`

These operators have a slightly tricky syntax. For example, you can't use these operators the way you'd use capture groups. 

```bash
$ ni iabcdefgh p'tr/a-z/A-Z/'
8
```

```bash
$ ni iabcdefgh p's/abc/ABC/'
1
```

The reason for these somewhat surp The return value of `tr` and `y` is the number of characters that were translated, and the return value of `s` is 0 if no characters were substituted and ` if characters were.
This also will give somewhat-surprising behavior to code like:

```bash
$ ni iabcdefgh p'$v = tr/a-z/A-Z/; $v'
8
```

Instead, these operators work as side-effects.

```bash
$ ni iabcdefgh p'tr/a-z/A-Z/; $_'
ABCDEFGH
```

```bash
$ ni iabcdefgh p's/abc/ABC/; $_'
ABCdefgh
```

However, as you might expect from Perl, there is a syntax that allows `s` to return the value; however, this will not work on Perls before 5.12 or 5.14.


```sh
$ ni iabcdefgh p's/abc/ABC/r'
ABCdefgh
```


## Operations on Arrays

Perl arrays are prefixed with the `@` sigil, and their scalar values can be looked up by prefixing with a `$`, and 

### Building Arrays

Perl arrays can be built up by separating scalars and arrays with 

### `split`

`split` is useful for generating arrays from a string based on regular expressions.

```bash
$ ni iabcd iefgh p'my @v = split /[cf]/; r @v'
ab	d
e	gh
```


### `for`
Perl has several syntaxes for `for` loops; the most explicit syntax is very much like C or Java. `for` takes an initialization

#### Block Prefix Syntax

```bash
$ ni iabcdefgh p'my $string= a; for (my $i=0; $i < length $string; $i++) {r substr($string, $i, 1) x 2;}'
aa
bb
cc
dd
ee
ff
gg
hh
```

In the above code, `substr($string, $i, 1)` is used to get 1 character from `$string` at position `$i`.  Perl also has a for syntax allowing you to define a variable name. 

```bash
$ ni iabcdefgh p'for my $letter(split //, $_) {r $letter x 2}'
aa
bb
cc
dd
ee
ff
gg
hh
```


#### Inline Syntax

Finally, there is an even more parsimonious postfix syntax that is useful for 


```bash
$ ni iabcdefgh p'r $_ x 2 for split //'
aa
bb
cc
dd
ee
ff
gg
hh
```

This again uses complicated syntax. This time we have essentially a block of code *not wrapped in braces*. The array on which it operates is the same `split //` (syntactic sugar for `split //, $_`) from last time. The author's personal preference is to use the postfix syntax, but to wrap the block in curly braces:


```bash
$ ni iabcdefgh p'{r($_ x 2)} for split //'
aa
bb
cc
dd
ee
ff
gg
hh
```

#### `next` and `last`
The keyword `next` is used to skip to the next iteration of the loop, similar to `continue` in Python, Java, or C.

```bash
$ ni iabcdefgh p'for my $letter(split //, $_) {if($letter eq "b") {next;} r $letter x 2}'
aa
cc
dd
ee
ff
gg
hh
```

`last` in Perl is similar to `break` in other programming languages.


```bash
$ ni iabcdefgh p'for my $letter(split //, $_) {r $letter x 2; last if $letter ge "c"}'
aa
bb
cc
```

Note that these 


### `join`
The opposite of `split` is `join`. It takes a string rather than an expression, and intersperses the string between the 

```bash
$ ni iabcd iefgh p'join "__", split //'
a__b__c__d
e__f__g__h
```


### `map`
`map` is in many ways similar to `for`; in exchange for some flexibility that `for` loops offer, `map` statements often have better performance through vectorization, and they are often more intuitive to read. `map` takes two arguments, a block of code and a perl array, and returns an array.

```bash
$ ni iabcdefgh p'map {$_ x 2} split //'
aa
bb
cc
dd
ee
ff
gg
hh
```

`map` uses a **lexically scoped** default variable `$_`. Because `$_` within the block is lexically scoped to the block, the `$_` in the block doesn't change the value of `$_` outside the block. For example:

```bash
$ ni iabcdefgh p'my @v = map {$_ x 2} split //; r $_'
abcdefgh
```


`map` can also take an expression rather than a block, for example:

```bash
ni iabcdefgh p'map /^[aeg]/, split //'
1
1
1
```




### `grep`

`grep` is a useful Unix command-line tool that is also useful for filtering Perl arrays. `grep` and `map` have similar syntax; grep can take an expression, for example:

```bash
$ ni iabcdefgh p'my @v = grep /^[acgh]/, map {$_ x 2} split //, $_; @v'
aa
cc
gg
hh
```

`grep` also takes a block of code, as in:

```bash
$ ni iabcdefgh p'my @v = grep { ord(substr($_, 0, 1)) % 2 == 0} map {$_ x 2 } split //, $_; @v'
bb
dd
ff
hh
```

Note that the expression syntax requires a comma following the expression, whereas the block syntax does not. Expect to mess this up a lot. That's not a _nice_ syntax, but Perl isn't nice. 





## Buffered Readahead and Multiline Selection

So far, we have only seen many ways to operate on data, but only one way to reduce it, the counting operator `c`. Buffered readahead allows us to perform operations on many lines at onceThese operations are used to convert columnar data into arrays of lines.  

### `rl`, `rw`, `ru`, `re`: Buffered Readahead

In general, the types of reductions that can be done with buffered readahead and multiline reducers can also be done with streaming reduce (discussed in a later chapter); however, the syntax for  buffered readahead is often much simpler. Generating arrays of lines using readahead operations is a common motif in `ni` scripts:

* `rl(n)`: read `n` lines
  * `@lines = rl(n)`: `n` is optional, and if it is not given, only one line will be read.
* `rw`: read while
  * `@lines = rw {condition}`: read lines while a condition is met
* `ru`: read until
  * `@lines = ru {condition}`: read lines until a condition is met
* `re`: read equal
  * `@lines = re {condition}`: read lines while the value of the condition is equal.


```
$ ni n10 p'r rl 3'
1	2	3
4	5	6
7	8	9
10
```

```bash
$ ni n10p'r rw {a < 7}'
1	2	3	4	5	6
7
8
9
10
```

```bash
$ ni n10p'r ru {a % 4 == 0}'
1	2	3
4	5	6	7
8	9	10
```

```bash
$ ni n10p'r re {int(a**2/30)}'
1	2	3	4	5
6	7
8	9
10
```


### `a_` through `l_`: Multiline Selection operations 
You have now seen two ways to generate arrays of lines: buffered readahead and data closures. In order to access data from a specific column of a line array, you will need to use multiline operators `a_` through `l_`, which are the multiline analogs to the line-based operators `a/a()` through `l/l()`.


An important thing to keep in mind when using buffered readahead with multiline reducers is that you must save. For example:

```bash
$ ni i{a..d}{x..z} F// fB. p'@lines = re {a}; r a, b_ @lines'
b	x	y	z
c	x	y	z
d	x	y	z
	x	y	z
```

* `$ ni ::data[n5] 1p'a(data)'` and `$ ni ::data[n5] 1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.
* `$ ni ::data[n5] 1p'a_ data'` works, because `a_` operates on each line.

```bash
$ ni ::data[n5] 1p'a_ data'
1
2
3
4
5
```

### Reduction via buffered readahead


```bash
$ ni 1p'cart [10, 20], [1, 2, 3]' p'sum b_ re {a}'
6
6
```

```bash
$ ni 1p'cart [10, 20], [1, 2, 3]' p'sum b_ re {a % 10}'
12
```


### `a__` through `l__`: Select-to-end-of-line
These are useful for collecting data with an unknown shape.

```bash
$ ni i[m 1 x] i[m 2 y s t] i[m 3 yo] p'r b__ rea'
1	x	2	y	s	t	3	yo
```

In particular, these operations can be used in conjunction with Hadoop streaming to join together data that have been computed separately.



## `ni` Perl Extensions

`ni` is based on Perl 5.08; as a result, many of the modules you might be familiar with are generally not used, in favor of `ni`-specific perl extensions.


### Array Processors

#### `min`, `max`, `minstr`, `maxstr`

#### `any`, `all`, `sum`, `prod`, `mean`

#### `uniq`

#### `argmax`, `argmin`

#### `freqs`

### `cart`: Cartesian Product
To generate examples for our buffered readahead, we'll take a short detour the builtin `ni` operation `cart`.

```bash
$ ni 1p'cart [10, 20], [1, 2, 3]'
10	1
10	2
10	3
20	1
20	2
20	3
```

The output of `cart` will have the first column varying the least, the second column varying the second least, etc. so that the value of the last column will change for every row if its values are all distinct.



Note that `cart` takeas array references (in square brackets), and returns array references. `ni` will interpret these array references as rows, and expand them. Thus `r`, when applied to `cart`, will likely not produce your desired results.

```
$ ni 1p'r cart [1], ["a", "b", "c"]'
ARRAY(0x7ff2bb109568)   ARRAY(0x7ff2ba8c93c8)   ARRAY(0x7ff2bb109b80)
```


### `p'%h = <key_col><val_col>_ @lines'`: Hash constructor
Hash constructors are useful for filtering large datasets without having to invoke an expensive sort or an HDFS join. The hash constructor is also a useful demonstration of both multiline selection and begin blocks.

* Generate a list of things you want to filter, and put it in a data closure. `::ids[list_of_ids]`
* Convert the data closure to a hash using a begin block (`^{%id_hash = ab_ ids}`)
* Filter another dataset (`ids_and_data`) using the hash (`exists($id_hash{a()})`)
* `$ ni ::ids[list_of_ids] ids_and_data rp'^{%id_hash = ab_ ids} exists($id_hash{a()})'` 

### `p'%h = <key_col><val_col>S @lines'`: Accumulator hash constructor

This is useful for doing reduction on data you've already reduced; for example, you've counted the number of neighborhoods in each city in each country and now want to count the number of neighborhoods in each country.

```bash 
$ ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] p'r acS rea'
x	5
y	13
```

If you have ragged data, where a value may not exist for a particular column, a convience method `SNN` gives you the non-null values. See the next section for the convenience methods `kbv_dsc`.

```bash 
$ ni i[y m 4 foo] i[y p 8] i[y n 1 bar] p'%h = dcSNN rea; @sorted_keys = kbv_dsc %h; r($_, $h{$_}) for @sorted_keys''
foo	4	
bar	1
```


### `p'kbv_asc %h'` and `p'kbv_dsc %h'`: Sort hash keys by value

This is syntactic sugar for Perl's sort function applied to keys of a hash.

```bash 
$ ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] p'r acS rea' p'r kbv_dsc(ab_ rl(3))'
y	x	z
```


```bash 
$ ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] p'r acS rea' p'r kbv_asc(ab_ rl(3))'
z	x	y
```

## Common Tricks

### Default Values

There are no key errors in Perl.

### Text is numbers

In any nice langauge, strings and numbers are different data types, and trying to use one as another (without an explicit cast) raises an error. Take a look at the following example:

```bash
$ ni 1p'my $v1="5"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
15	555	5 golden rings
```

It's unsurprising that the Perl infix `x` operator (string duplication) and the Perl infix `.` operator (string concatenation) work, but if you come from a friendly language that just wants you to be sure you're doing the right thing, the idea that you can multiply a string by a number and get a number is frustrating. But it gets worse.

```bash
$ ni 1p'my $v2="4.3" * "6.7"; r $v2'
28.81
```

You can perform floating point multiplication on two string variables with zero consequences.  This is complicated, but not ambiguous; if it is possible to cast the two strings (silently) to numbers, then Perl will do that for you automatically. Strings that start with valid numbers are cast to the longest  component parseable as a float or integer, and strings that do not are cast to zero when used in an arithmetic context.

```bash
$ ni 1p'my $v1="hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
0	hihihi	hi golden rings
```

```bash
$ ni 1p'my $v1="3.14hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
9.42	3.14hi3.14hi3.14hi	3.14hi golden rings
```

```bash
$ ni 1p'my $v1="3.1E17"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
930000000000000000	3.1E173.1E173.1E17	3.1E17 golden rings
```


### Barewords are strings

In Perl, a "bareword" is an sequence of characters with no sigil prefix, for example:

```bash
$ ni n3p'r a, one'
1	one
2	one
3	one
```

Whereas in almost any other language, a syntax error or name error would be raised on referencing a variable that does not exist--in this case, `one`--  Perl gives the programmer a great deal of freedom to be concise. `one` has not been defined, so Perl assumes you know what you're doing and interprets it as a string. Perl assumes you are a great programmer, and in doing so, allows you to rise to the challenge.

This has an important implication for hash lookups; we can use an unquoted string to look up terms, for example:

```bash
$ ni 1p'my %h = ("foo" => 32); $h{foo}'
32
```

However, the default behavior is for a hash to look up the value of the string the bareword represents.

As a result, there's a conflict when using the output of a function with no arguments (for example, `a`):

```
$ ni ifoo p'my %h = ("foo" => 32); $h{a}'
```

Returns nothing; in fact it is looking for the value with the key `"a"`.

```
$ ni ifoo p'my %h = ("foo" => 32, "a" => "hello"); $h{a}'
hello
```

In this case we prefix with a single `+` to indicate that the bareword in braces should be interpreted as a function.


```bash
$ ni ifoo p'my %h = ("foo" => 32); $h{+a}'
32
```




### String Operators

#### String Comparison: `eq`, `ge`, `gt`, `le`, `lt`

Because strings are interpreted as numbers, if you use the numeric comparator operators, strings will be cast to numbers and compared.

```bash
ni 1p' "ab" == "cd" ? "equal" : "not equal"'
equal
```

You need to use the specific string-comparison opeartors instead.

```bash
ni 1p' "ab" eq "cd" ? "equal" : "not equal"'
not equal
```


#### `substr`

`substr($s, $offset, $length)`: The behavior of `substr` is a little tricky if you have a Python background:

  * If `$offset` is positive, then the start position of the substring will be that position in the string, starting from an index of zero;
  * If `$offset` is negative, then the start position of the substring will be `$offset` charaters from the end of the string.
  * If `$length` is positive, the output substring will take (up to) `$length` characters.
  * If `$length` is negative, the output substring will take characters from `$offset` to the `$length` characters from the end of the string.

#### Using regular expressions versus `substr` 




### Bitwise Operators

#### Bit Shifts `<<` and `>>`

#### Bitwise `&` and `|`

### Hash subroutines

* `keys %h` get keys from hash
* `values %h`: get values from hash

### Useful variables

#### `%ENV`: Hash of Environment Variables

This is way easier to access than in any comparable language. To get the value of the variable you want, remember to dereference with `$ENV{varname}`. Also, it's good to note here that you can more than likely get away with referencing the variable you want using a bareword (unquoted string) rather than a quoted string.





## JSON I/O

We'll spend the rest of this chapter discussing JSON and directory I/O; the former is fundamental to a lot of the types of operations that `ni` is good at, the latter will expand your understanding of `ni`'s internal workings.

### `D:<field1>,:<field2>...`: JSON Destructure

`ni` implements a very fast JSON parser that is great at pulling out string and numeric fields.  As of this writing (2017-01-16), the JSON destructurer does not support list-based fields in JSON.

### `p'json_encode <hash or array reference>`: JSON Encode

The syntax of the row to JSON instructions is similar to hash construction syntax in Ruby. 
  
```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' =\>jsons \
     D:type,:word
```
### Other JSON parsing methods
Some aspects of JSON parsing are not quite there yet, so you may want to use (also very fast) raw Perl to destructure your JSONs.



## Directory I/O
The contents of this section 

Start by making some data:

```
$ rm -rf dir_test && mkdir dir_test
$ echo "hello" > dir_test/hi
$ echo "you" > dir_test/there
```

Let's start with `$ ni test`

```
dir_test/hi
dir_test/there
```

`ni` has converted the folder into a stream of the names of files (and directories) inside it. You can thus access the files inside a directory using `\<`.

`$ ni dir_test \<`

yields:

```
hello
you
```

`ni` also works with the bash expansion operator `*`.

For example, `ni dir_test/*` yields:

```
hello
you
```

`ni` is able to go to the files directly becasue it is applies bash expansion first; bash expansion generates the file paths, which `ni` is then able to interpret.

```
$ ni --explain dir_test/*
["cat","dir_test/hi"]
["cat","dir_test/there"]
```

## `ni` Philosophy and Style

If you've made it this far in the tutorial, you now have enough tools to be extremely productive in `ni`. If you're ready to get off the crazy ride of this tutorial and get to work, here's a great point to stop. Before you go, though, it will help to take a few minutes to think through `ni`'s philosophy and style, and how those two intersect.

### `ni` optimizes at-will programmer productivity
Many data science projects start with a request that you have never seen before; in this case, it's often easier to start from very little and be able to build rapidly, than it is to have a huge personal library of Python scripts and Jupyter notebooks that you can bend to fit the task at hand.

`ni` is great at generating throwaway code, and this code can be made production-ready through `ni` scripting, discussed in the next chapter.

### Mastery counts

Just because you can read a Python script and understand what it does at a basic level does not mean you can code in Python, and Python can trick very intelligent people into thinking they know what they're doing even when they don't. The opposite is true of `ni`. It will be inherently obvious when you don't know something in `ni`, because if you don't know something, it'll be likely that the part of the spell you're reading won't make sense.

This is a gruff approach to programming, but it's not unfriendly. `ni` doesn't allow you to just get by--your only option is mastering `ni` one piece at a time.

### Outsource hard jobs to more appropriate tools

`ni` is a domain-specific language; its domain is processing single lines and chunks of data that fit in memory

* Because of this philosophy, `ni` is fantastic for data munging and cleaning.
* Because of this philosophy, large-scale sorting is not a `ni`ic operation, while gzip compression is.
* Because of this philosophy, `ni` relies heavily on Hadoop for big data processing. Without Hadoop, most sufficiently complicated operations become infeasible from a runtime perspective once the amount of data exceeds a couple of gigabytes uncompressed.

Some jobs that are difficult for `ni`, and some ways to resolve them:

* Sorting
  * Challenge: Requires buffering of entire stream (possibly to disk, which is slow)
  * Solution: Hadoop Streaming will do much of the heavy lifting for you in any sort, and will distribute the computation so it's easy.
* Matrix Multiplication
  * Challenge: Difficult to implement in a streaming context
  * Solution: Numpy operations 
* SQL Joins
  * Challenge: SQL joins can take more than one line
  * Solution:
    * Small Data: There are several options here. You can use `N'...'` and do the join in numpy, you can use `N'...'` with `import pandas as pd` to do the join in pandas, or you can pipe data in and out of `join -t $'\t'`
    * Large Data: HDFS Joins
* Iterating a `ni` process over directories where the directory provides contextual information about its contents.
  * Challenge: This is something `ni` can probably do, but I'm not sure how to do it offhand.
  * Solution: Write out the `ni` spell for the critical part, embed the spell in a script written in Ruby/Python, and call it using `bash -c`.



