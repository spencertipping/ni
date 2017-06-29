# `ni` by Example, Chapter 2 Part 1 (alpha release)

Welcome to the second part of the tutorial. At this point, you know a little `ni` syntax, but you might not be able to do anything useful. In this chapter, our goal is to multiply your power to operate on a single machine by covering all of the Perl syntax you need to work effectively with `ni`.

## Perl for `ni`

This section written for an audience that has never worked with the language before, and from a user's (rather than a developer's) perspective. This tutorial encourages you to learn to use Perl's core functions rather than writing your own. For example, we'll cover the somewhat obscure operation of bit shifting but avoid discussing how to write a Perl subroutine, which (while easy) is unnecessary for most workflows.


From the Perl Syntax docs:
> Many of Perl's syntactic elements are optional. Rather than requiring you to put parentheses around every function call and declare every variable, you can often leave such explicit elements off and Perl will figure out what you meant. This is known as **Do What I Mean**, abbreviated **DWIM**. It allows programmers to be lazy and to code in a style with which they are comfortable.

## `p'...'`: Map Perl over lines
When you think of writing a simple data processing program in Python, Ruby, C, or even Perl, think about how many keystrokes are spent loading libraries that are used mostly implicitly; and if they're not loaded, the program won't run.

Even the act of writing a script that reads from standard input and writes to standard output, maybe compiling it, and then calling it with arguments from the command line requires a lot of task-switching.  

`ni` removes all of that; the moment you type `p'...'`, you're dropped directly into the middle of your Perl main subroutine, with `$_` already set implicitly to the incoming line of the input stream.


### `r()`: Emit row

Up to this point we have not discussed how or what the Perl operator returns; it turns out that this is less intuitive than one might expect.

Let's take a look at the ouput of our script when we take out the `r` from inside the Perl mapper.

```
ni /usr/share/dict/words rx40 r10 p'substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'
aba
iss
ed
aba
sta
rdize
abb
rev
iature
abd
uct
...
...
...
```

Without `r()`, every value separated by a comma is **returned** on its own row; these returned rows are then sent to the output stream.

The `r()` operator, on the other hand, **returns the empty list**. It works by printing the values separated by columns to a single tab-delimited row of the output stream.

Now that the practical differences between `r()` and `p'...'` have been explained, we can examine the differences in their use that are entailed.  

Clearly, If the desired output of the Perl mapper is two or more columns per row of stream data, you must use `r()`. If the desired output of the perl mapper step is a single column per row, you could either use `r()` or not.  The more concise statement leaving out `r()` is preferred.

When it is clear from context (as above), `r()'` can be referred to as  `r`, which is how it is more commonly written in practice. This differs from the take-rows operator (also called `r`).


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


## Flow Control

### Truthiness in Perl

Recall that Perl has no specific boolean type. Recalling from Part 1, the number 0, the string `"0"`, the empty list `()`, the empty string`""`, and the keyword `undef` are all falsey; everything else evaluates to true.

### `if`, `else`, `elsif`

#### Block Prefix Form

This form is common to almost every programming language in one form or another;

```bash
$ ni i37 p'if (a == 2) { r "input was 2" } elsif (a =~ /^[Qq]/ ) { r "input started with a Q" } else { r "I dunno" }'
I dunno
```

#### Inline Form

`if` also has an inline form, which provides a readable syntax for lines that should be evaluated conditionally.

```bash
$ ni 1p'my $x = 5; r "x = 5" if $x == 5'
x = 5
```


#### `unless`

`unless EXPR` is equivalent to `if !EXPR`, and it has the same syntax.

```bash
$ ni 1p'my $x = 5; r "x = 5" unless $x != 5'
x = 5
```

#### Ternary Operator `... ? ... : ...`

The ternary operator is used to select between options based on the truth value of a statement or variable.

```bash
$ ni 1p'my $x = 5; $x == 5 ? "x = 5" : "x != 5"' | cat
x = 5
```

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

Let's take a look at this slightly different example, where we've dropped the `my` keyword within the block.

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


### `%ENV`: Hash of Environment Variables

Environment variables are much easier to access and modify in Perl compared than in any comparable language. To get the value of the variable you want, remember to dereference with `$ENV{varname}`.

## Regular Expressions

Covering regular expressions in their entirety is outside the scope of this tutorial, but we'll cover a few pointers. The [Perl Regex Tutorial](https://perldoc.perl.org/perlretut.html) is excellent.


#### Writing Good Regular Expressions

When writing a regular expression, you want them to fail as quickly as possible on strings that don't match.

1. Use anchor tags (`^` and `$`) when possible.
2. Try to limit the number of branching "or" paths in your regular expression.


#### Special Characters 

The following is (very minimally) adapted from the `perldoc`:

\d matches a digit, not just [0-9] but also digits from non-Roman scripts
\s matches a whitespace character, the set [\ \t\r\n\f] and others
\w matches a word character (alphanumeric or _), not just [0-9a-zA-Z_] but also digits and characters from non-Roman scripts
\D is a negated \d; it represents any other character than a digit, or [^\d]
\S is a negated \s; it represents any non-whitespace character [^\s]
\W is a negated \w; it represents any non-word character [^\w]
The period '.' matches any character but "\n" (unless the modifier //s is in effect, as explained below).

#### Regex Matching `=~`

```bash
$ ni ihello p'a =~ /^h/ ? "match" : "no match"'
match
```

Negative matches to a regex can be performed with `!~`.

```bash
$ ni ihello p'a !~ /^h/ ? "negative match" : "positive match"'
positive match
```

#### Using Capture Groups

Capture groups are set off using parentheses; to get them explicitly:

```bash
$ ni iabcdefgh p'my @v = a =~ /^(.)(.)/; r @v'
a	b
```

There is also a syntactic sugar using the default variable `$_`.
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

#### Regex Barriers

Usually regular expressions are set off using forward slashes, however, this means that forward slashes within a regex will have to be escaped. You can use `m` and another character when you need to match forward slashes.

```bash
$ ni i/usr/bin p'm#^/usr/(.*)$#'
bin
```


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

There are other syntaxes for `s///` and `tr///`, but many of them do not work with Perl 5.8, which is the earliest Perl version with which `ni` is designed to work.


#### Regex Interpolation

Perl scalars can be interpolated into regular expressions, like this:

```bash
$ ni 1p'$foo = "house"; "housecat" =~ /$foo/ ? "match" : "no match"'
match
```

```bash
$ ni 1p'$foo = "house"; "cathouse" =~ /cat$foo/ ? "match" : "no match"'
match
```

```bash
$ ni 1p'$foo = "house"; "housecat" =~ /${foo}cat/ ? "match" : "no match"'
match
```


## Operations on Arrays

Perl arrays are prefixed with the `@` sigil, and their scalar values can be looked up by prefixing with a `$`, and indexing using square brackets.

### `split`

`split` is useful for generating arrays from a string based on regular expressions.

```bash
$ ni iabcd iefgh p'my @v = split /[cf]/; r @v'
ab	d
e	gh
```


### `for`
Perl has several syntaxes for `for` loops; the most explicit syntax is very much like C or Java. `for` takes an initialization and a block of code to be run for each value of the initialization.

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

Finally, there is an even more parsimonious postfix syntax that is useful for short repetitive operations.


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

Note the way that `last` and `next` combine with `if` and `unless` for readable syntax. 


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

`map` can also be used with a capturing regular expression:

```sh
$ ni i[/usr/bin /usr/tmp]  p'r map m#^/usr/(.*)$#, F_' | cat
bin	tmp
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
$ ni iabcdefgh p'my @v = grep { ord(substr($_, 0, 1)) % 2 == 0} map { $_ x 2 } split //, $_; @v'
bb
dd
ff
hh
```

Note that the expression syntax requires a comma following the expression, whereas the block syntax does not. Expect to mess this up a lot. That's not a _nice_ syntax, but Perl isn't nice. 


### Operations on mulitple arrays and scalars

You can feed multiple lists and scalars into `grep`, `map`, and `for`, by setting them off with commas; there is no need to construct a new array containing all of the elements you want to operate over.

```bash
$ ni 1p'my @x = (1, 2); my $y = "yo"; my @z = ("good", "bye"); map {$_ x 2} @x, $y, @z'
11
22
yoyo
goodgood
byebye
```


## Common Tricks

### Default Values

There are no key errors or index errors in Perl. In the spirit of "Do What I Mean", you're allowed to leave variables undefined and initialize those values later.

```bash
$ ni 1p'my $x; ++$x'
1
```

```bash
$ ni 1p'my @x; $x[3] = "yo"; r @x'
			yo
```



### `defined` and `exists`

Because there are no key errors, `exists` is used to check the presence of a hash element or array element.

```bash
$ ni 1p'my @x; $x[3] = "yo"; exists $x[0] ? "yes" : "no"'
no
```

```bash
$ ni 1p'my %h = {"u" => "ok"}; exists $h["me"] ? "yes" : "no"'
no
```

The scalar analog of `exists` is `defined`, which checks if a scalar has been assigned a value.

```bash
$ ni 1p'my $x; defined $x ? "yes" : "no"'
no
```

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

Because text is numbers in Perl, some string operators must be defined specifically.

#### String Concatenation
String concatenation is done with `.` in Perl rather than `+` in other languages. Because text is numbers, using `+` will attempt to do addition on the numeric values of the strings. For example:

```bash
$ ni 1p'u + v'
0
```

```bash
$ ni 1p'u . v' 
uv
```


#### String Interpolation

In Perl, string interpolation occurs between double-quoted strings. 

```bash
$ ni 1p'my $x = "foo"; r "$x bar"'
foo bar
```

If the interpolated variable could be confused, it can be wrapped in curly braces.

```bash
$ ni 1p'my $x = "foo"; r "${x}bar"'
foobar
```

You can also use the results of functions, however, these need to be prefixed with a backslash to tell Perl to evaluate the function.

```bash
$ ni ifoo p'r "${\a}bar"'
foobar
```

Because `ni` is written in Perl, Perl mappers are set off with single quotes (otherwise  the parser would try to interpolate variable declarations). Moral of the story: 

> DO NOT USE single-quoted strings in `ni`.



#### String Comparison: `eq`, `ge`, `gt`, `le`, `lt`

Because strings are interpreted as numbers, if you use the numeric comparator operators, strings will be cast to numbers and compared.

```bash
$ ni 1p' "ab" == "cd" ? "equal" : "not equal"'
equal
```

You need to use the specific string-comparison opeartors instead.

```bash
$ ni 1p' "ab" eq "cd" ? "equal" : "not equal"'
not equal
```


#### `substr`

`substr($s, $offset, $length)`: The behavior of `substr` is a little tricky if you have a Python background:

  * If `$offset` is positive, then the start position of the substring will be that position in the string, starting from an index of zero;
  * If `$offset` is negative, then the start position of the substring will be `$offset` charaters from the end of the string.
  * If `$length` is positive, the output substring will take (up to) `$length` characters.
  * If `$length` is negative, the output substring will take characters from `$offset` to the `$length` characters from the end of the string.
  


#### Using regular expressions versus `substr` 

`substr` does exactly one thing, and it is quite fast at doing it. If you need a fixed-length substring, you'll likely have higher performance with `substr` compared to a regex; 


### Logical Operations

Logical operators come in two flavors, high-precedence and low-precedence. The low-precedence operators `and`, `or`, `not`, and their high-precedence C-like counterparts `&&`, `||`, `!`.

In general, there is little difference between these operations, but tricky errors may arise. In general, it is safe and syntactically pleasing to use the low-precedence operators between statements, and to use the high-precedence operators with scalars and expressions.

```
open my $fh, '<', $filename || die "A horrible death!"; ## Fails
open my $fh, '<', $filename or die "A horrible death!"; ## Succeeds
```



### Bitwise Operators

Bit shifting is useful because it is an incredibly fast operation; it's very useful for building hash functions and reducing complexity or computational burden. 


#### Bit Shifts `<<` and `>>`

Bit shifting to the right is (in general) the equivalent of taking the integer part of dividing by the specified power of 2.

```bash
$ ni 1p'125 >> 3'
15
```

```bash
$ ni 1p'int(125/2**3)'
15
```

Bit shifting to the left is equivalent to multiplying by the specified power of 2.


```bash
$ ni 1p'125 << 3'
1000
```

The exception to this rule is when the number of bits to shift is larger than the size of the registers in the underlying hardware, but this 

#### Hexadecimal Numbers

You can write numbers in hexadecimal (base-16) in Perl without specifying a type:

```bash
$ ni 1p'r 0x3 + 0xa, 0x3 + 0xA'
13	13
```

#### Bitwise `&` and `|`

These operations can be useful for unwinding loops in programs; like bit shifts, they're very fast and convenient for running on binary-like data.

```bash
$ ni 1p'r 0x3 & 0xa, 0x3 | 0xa'
2	11
```

They also work on decimal numbers


```bash
$ ni 1p'r 3 & 10, 3 | 10'
2	11
```


### Hash subroutines

* `keys %h`: Returns the list of keys from a hash
* `values %h`: Returns the list of values from a hash



## Conclusion

Perl is much-maligned for its syntax; much of that malignancy comes from people whose only exposure to the language is hearing about the [Obfuscated Perl Contest](https://en.wikipedia.org/wiki/Obfuscated_Perl_Contest). Perl is also known for its religious overtones; here `ni` author Spencer Tipping drop the scales from your eyes about the language in this section from his short intro to the language, [Perl in 10 Minutes](https://github.com/spencertipping/perl-in-ten-minutes). 


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









