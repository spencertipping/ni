# `ni` by Example, Chapter 2 (beta release)

Welcome to the second part of the tutorial. At this point, you know a little `ni` syntax, but you might not be able to do anything useful. In this chapter, our goal is to multiply your power to operate on a single machine by covering all of the Perl syntax you need to work effectively with `ni`.

## Perl for `ni`

This section written for an audience that has never worked with Perl before, and from a user's (rather than a developer's) perspective. To that end, you are encouraged to focus on using Perl's core functions rather than writing your own. For example, we'll cover the somewhat obscure operation of bit shifting but avoid discussing how to write a Perl subroutine, which (while easy) is unnecessary for most workflows.


## Perl Syntax

Perl has a lot of rules that allow for code to be hacked together quickly and easily; there's no way around learning them (as there is in nice-feeling languages), so let's strap in and get this over with.

From the Perl Syntax [docs](https://perldoc.perl.org/perlsyn.html):

> Many of Perl's syntactic elements are optional. Rather than requiring you to put parentheses around every function call and declare every variable, you can often leave such explicit elements off and Perl will figure out what you meant. This is known as **Do What I Mean**, abbreviated **DWIM**. It allows programmers to be lazy and to code in a style with which they are comfortable.


### Sigils

If you've seen a little bit of Perl, one of the most intimidating parts of getting started is its use of odd characters (like `$`, `@`, and `%`) to start variable names. These characters are referred to as sigils and are used by the Perl interpreter to increase the language's concision.


```sh
$x = 5;
@x = 1..10;
%x = (foo => 1, bar => 2);
```

Coming from a Python/Ruby/C/Java/Lisp background, this probably feels wrong. That this code works, and that all of the variables defined within would properly be referred to (at least, in brief) as `x`, can trigger a great deal of cognitive dissonance within a reasonably-skilled but Perl-ignorant programmer.

The question here, and its relevance for `ni`, is not whether this language syntax is useful (it is), but how to open our minds and increase our reading skills to take advantage of Perl's unique qualities.

When a Perl variable is brought into existence, its nature is determined by the sigil that precedes it. The explicit use of sigils allows for more variables to be packed into the same linguistic namespace. When creating a variable:

* `$x` indicates the variable `$x` is a scalar, for example a string or a number.
* `@x` indicates the variable `@x` is an array.
* `%x` indicates the variable `%x` is a hash.

Because the syntax for all of these is different, we can use all of these

```
print $x;         # get the scalar value of $x
print $x[3];      # get the scalar from @x at index 3
print $x{"foo"};  # get the scalar from %x associated with the key "foo"
```

At first glance, this is adds to the confusion; all of these values start with `$x`. In fact, it's very explicit, you just need to learn the rules. You're completely in control of telling the interpreter what to do. 

In the case of `$x[3]`, you've told the Perl interpreter to get a scalar value by using the prefixed `$` sigil; based on the square brackets used to as a postfix index, Perl knows that you're referring to an array, since square brackets are used to index arrays.

Similarly, using curly braces tells Perl to look in a hash, and without either of those, the Perl interpreter assumes you are referring to the scalar value `$x`. The syntax is a little complicated, but it's not tricky. With a little practice, this will be second nature.


## Default Variables and Values

### The Default Variable `$_`

We can start our discussion with a pretty normal-looking Perl function:

```bash
$ ni i1 i10 i100 i1000 p'r a, length(a)'
1	1
10	2
100	3
1000	4
```

What's going on here is pretty clear: we're using the Perl builtin `length` function to count the number of digits in each of the numbers in our input line. Looks straightforward.

What's shocking is that you can also write the function this way:

```bash
$ ni i1 i10 i100 i1000 p'r a, length'
1	1
10	2
100	3
1000	4
```

Suddenly, the `length` function, which looks like it _should_ have an argument, doesn't have one. And it still works just fine.  What's actually happening here is that Perl is using a default variable, called `$_`. 

What can we say about the type of `$_`? Since it's prefixed with a `$` sigil, we know it's a scalar value, and its name is `_`. In `ni`, `$_` is set to the whole incoming line from the stream (minus the trailing newline).


```bash
$ ni i1 i10 i100 i1000 p'$_'
1
10
100
1000
```

Many Perl functions will operate on `$_` if they are not given an argument; we'll revisit this in the section about regular expressions below, as well as when we discuss `for` loops.

### Default Values

There are no key errors or index errors in Perl. In the spirit of "Do What I Mean", you're allowed to leave variables undefined and initialize values later.

```bash
$ ni 1p'$x; ++$x'
1
```

```bash
$ ni 1p'@x; $x[3] = "yo"; r @x'
			yo
```


### `defined` and `exists`

Because there are no key errors in Perl, `exists` is used to check the presence of a hash element or array element.

```bash
$ ni 1p'@x; $x[3] = "yo"; exists $x[0] ? "yes" : "no"'
no
```

```bash
$ ni 1p'%h = {"u" => "ok"}; exists $h["me"] ? "yes" : "no"'
no
```

The scalar analog of `exists` is `defined`, which checks if a scalar has been assigned a value.

```bash
$ ni 1p'$x; defined $x ? "yes" : "no"'
no
```

## Statements, Blocks, and Scope

### Statements and Blocks

- An expression is a series of variables, operators, and method calls that evaluates to a single value. 

- A statement forms a complete unit of execution and is terminated with a semicolon `;`.

- A group zero or more statements group together into a block with braces: `{...}`. 


In Perl, a block has its own scope. Blocks can modify and use variables from a containing scope, or define variables in their own scope using the `my` keyword that are inaccessible from blocks outside.


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

`my` can also assign values to and from an array, for example:

```bash
$ ni i[foo bar] p'my ($first, $second) = F_; r $second, $first'
bar	foo
```

`my` can also assign anything left over to an array;

```bash
$ ni i[foo bar baz qux qal] p'my ($first, $second, @rest) = F_; r $second, $first; r @rest'
bar	foo
baz	qux	qal
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

Begin blocks combine with the `rp'...'` function well to filtering a stream, for example:

```bash
$ ni i[faygo cola] i[orange juice] i[grape soda] i[orange crush] i[hawaiian punch] rp'^{%good_flavors = ("orange" => 1, "grape" => 1)} my $flavor = a; $good_flavors{$flavor}'
orange	juice
grape	soda
orange	crush
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
$ ni i5 p'r "input = 5" if a == 5'
input = 5
```


#### `unless`

`unless EXPR` is equivalent to `if !EXPR`, and it has the same syntax.

```bash
$ ni i5 p'r "input = 5" unless a != 5'
input = 5
```

#### Ternary Operator `... ? ... : ...`

The ternary operator is used to select between options based on the truth value of a statement or variable.

```bash
$ ni i6 p'a == 5 ? "input = 5" : "input != 5"'
input != 5
```

### `die`

`die` is used to raise an error; it's not used much in `ni`, but can be useful for debugging important workflows.

```sh
$ ni n5 p'a < 3 ? r a : die "too big"'
too big at - line 2972, <STDIN> line 3.
1
2
```


## Regular Expressions

Covering regular expressions in their entirety is outside the scope of this tutorial, but Perl and regex are tightly bound, and we'll cover a few pointers. The [Perl Regex Tutorial](https://perldoc.perl.org/perlretut.html) is excellent.


### Writing Good Regular Expressions

When writing a regular expression, you want them to fail as quickly as possible on strings that don't match.

1. `^` and `$` are anchor tags that represent the start and end of the string, respectively. Use them when possible. 
2. Try to limit the number of branching "or" paths in your regular expression.


### Special Characters 

The following is (very minimally) adapted from the `perldoc`:

* `\d` matches a digit, not just `[0-9]` but also digits from non-Roman scripts
* `\s` matches a whitespace character, the set `[\ \t\r\n\f]` and others
* `\w` matches a word character (alphanumeric or underscore), not just `[0-9a-zA-Z_]` but also digits and characters from non-Roman scripts
* `\D` is a negated `\d`; it represents any other character than a digit, or `[^\d]`
* `\S` is a negated `\s`; it represents any non-whitespace character `[^\s]`
* `\W` is a negated `\w`; it represents any non-word character `[^\w]`
* `.` matches any character except newlines

### Regex Matching `=~` and `!~`

To match a string to a regex, use `=~`.

```bash
$ ni ihello p'a =~ /^h/ ? "match" : "no match"'
match
```

Negative matches to a regex can be performed with `!~`.

```bash
$ ni igoodbye p'a !~ /^h/ ? "negative match" : "positive match"'
negative match
```

### Using Capture Groups

Capture groups are set off using parentheses; to get them explicitly:

```bash
$ ni iabcdefgh p'my @v = a =~ /^(.)(.)/; r @v'
a	b
```



### Regex Barriers

Usually regular expressions are set off using forward slashes, however, this means that forward slashes within a regex will have to be escaped. You can use `m` and another character when you need to match forward slashes.

```bash
$ ni i/usr/bin p'm#^/usr/(.*)$#'
bin
```


### Substitution `s///`, Transliteration `tr///` and `y///`

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


### Regex Interpolation

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

## String Operators

### String Concatenation
String concatenation is done with `.` in Perl rather than `+` common in other languages. We'll discuss why `+` can't be used later, but you may want to test it out now to see what happens when you try to add two strings with `+`.


```bash
$ ni 1p'"foo" . "bar"' 
foobar
```


### String Interpolation

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



### String Comparison: `eq`, `ge`, `gt`, `le`, `lt`

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


### `substr`

`substr($s, $offset, $length)`: The behavior of `substr` is a little tricky if you have a Python background:

  * If `$offset` is positive, then the start position of the substring will be that position in the string, starting from an index of zero;
  * If `$offset` is negative, then the start position of the substring will be `$offset` charaters from the end of the string.
  * If `$length` is positive, the output substring will take (up to) `$length` characters.
  * If `$length` is negative, the output substring will take characters from `$offset` to the `$length` characters from the end of the string.
  
```bash
$ ni iabcdefgh p'r substr(a, 3), substr(a, 0, 3), substr(a, -2), substr(a, 0, -2)'
defgh	abc	gh	abcdef
```


### Using regular expressions versus `substr` 

`substr` does exactly one thing, and it is quite fast at doing it. If all you need is a single fixed-length substring, you'll likely have higher performance with `substr` compared to a regex; otherwise, well-constructed regular expressions are a better and more flexible choice.

```bash
$ ni iabcdefgh p'r /(.{5})$/, /^(.{3})/, /(.{2})$/, /^(.*)../'
defgh	abc	gh	abcdef
```


## Operations on Arrays

Perl arrays are prefixed with the `@` sigil, and their scalar values can be looked up by prefixing with a `$`, and indexing using square brackets.

### Array Constructors

#### Explicit constructors

Arrays are written in parentheses, with elements set off by commas.

```bash
$ ni 1p'my @arr = (1, 2, 3); r @arr'
1	2	3
```

#### `qw`: Quote Word

Writing lots of double quotes around strings is kind of a pain; Perl implements an operator called `qw` which allows you to build a list with a minimum of keystrokes:

```bash
$ ni 1p'my @arr = qw(1 2 3); r @arr'
1	2	3
```


#### `split`

`split` is another way for generating arrays from a string based on regular expressions.

```bash
$ ni iabcd iefgh p'my @v = split /[cf]/; r @v'
ab	d
e	gh
```

#### Array references (OPTIONAL)
Many other languages use square brackets; in Perl, these are used for array references:

```sh
$ ni 1p'my @arr = [1, 2, 3]; r @arr'
ARRAY(0x7fa7e4184818)
```

This code has built a length-1 array containing an array reference; if you really wanted to create an array reference, you'd more likely do it explicitly.

```sh
$ ni 1p'my $arr_ref = [1, 2, 3]; r $arr_ref'
ARRAY(0x7fa7e4184818)
```

To dereference the reference, use the appropriate sigil:

```bash
$ ni 1p'my $arr_ref = [1, 2, 3]; r @$arr_ref'
1	2	3
```

Back to the first example, to dereference the array reference we've (probably unintentionally) wrapped in an array, do:


```bash
$ ni 1p'my @arr = [1, 2, 3]; r @{$arr[0]}'
1	2	3
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

You do not need to explicitly name a loop variable, as we have done above; if you don't, the loop variable will be put into a variable called `$_`:

```bash
$ ni iabcdefgh p'for(split //, $_) {r $_ x 2}'
aa
bb
cc
dd
ee
ff
gg
hh
```

Using your knowledge of blocks and scoping, you should know what will happen if we add `r $_` to the command: `$ ni iabcdefgh p'for(split //, $_) {r $_ x 2} r $_'`. Figure it out? Let's check.

```bash
$ ni iabcdefgh p'for(split //, $_) {r $_ x 2} r $_'
aa
bb
cc
dd
ee
ff
gg
hh
abcdefgh
```

`$_` in the loop block is scoped to the loop; it does not affect the `$_` scoped to the top-level Perl mapper, which is printed by `r $_`.


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
$ ni i[/usr/bin /usr/tmp]  p'r map m#^/usr/(.*)$#, F_'
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


### `sort` and `reverse`

`reverse` reverses an array:

```bash
$ ni 1p'my @arr = (3, 5, 1); r reverse @arr'
1	5	3
```

`sort` with no arugments will sort an array in ascending order

```bash
$ ni 1p'my @arr = (3, 5, 1); r sort @arr'
1	3	5
```

```bash
$ ni 1p'@arr = qw[ foo bar baz ]; r sort @arr'
bar	baz	foo
```

To do a reverse sort, compose the functions together like this:

```bash
$ ni 1p'@arr = qw[ foo bar baz ]; r reverse sort @arr'
foo	baz	bar
```

**CAVEAT** `sort` is a little bit finnicky. If you run:

```bash
$ ni i[romeo juliet rosencrantz guildenstern] p'r sort F_'
F_
```

That's not what we want, so what's going on?

In Perl, if you pass a function to `sort`, it will try to use that function to do the sort. Perl also expects that function to be in a particular form. In this case, we've really confused the hell out of Perl, so it returns that nonsense.

We can fix this by giving Perl a hint to execute the function `F_`:

```bash
$ ni i[romeo juliet rosencrantz guildenstern] p'r sort +F_'
guildenstern	juliet	romeo	rosencrantz
```

This syntax is a little tricky for my taste; I prefer the more explicit:

```bash
$ ni i[romeo juliet rosencrantz guildenstern] p'my @arr = F_; r sort @arr'
guildenstern	juliet	romeo	rosencrantz
```

> To avoid any potential nastiness, only apply `sort` directly to arrays you've computed somewhere else.

### Custom Sorting (OPTIONAL)
You can implement a custom sort by passing a block to `sort`.

Let's say you wanted to sort by the length of the string, rather than the order:


```bash
$ ni i[romeo juliet rosencrantz guildenstern hero leander] p'my @arr = F_; r sort {length $a <=> length $b } F_'
hero	romeo	juliet	leander	rosencrantz	guildenstern
```

To reverse the sort, reverse `$b` and `$a`.

```bash
$ ni i[romeo juliet rosencrantz guildenstern hero leander] p'my @arr = F_; r sort {length $b <=> length $a } F_'
guildenstern	rosencrantz	leander	juliet	romeo	hero
```

More details are available in the [perldocs](https://perldoc.perl.org/functions/sort.html).


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



## Perl Hashes

### Hash construction

The easiest way to build up a hash is using the associative arrow syntax:

```bash
$ ni 1p'my %h = ("x" => 1); r $h{"x"}'
1
```

You can also make hashes using the normal list syntax, but this is less clear.

```bash
$ ni 1p'my %h = ("x", 1); r $h{"x"}'
1
```


Hashes can also be cast directly to and from arrays.

```bash
$ ni 1p'my @arr = qw[foo 1 bar 2]; my %h = @arr; r $h{"bar"}'
2
```

### `keys`, `values`

These, quite unsurprisingly, return the keys and values of a hash. 

```bash
$ ni 1p'my %h = ("foo" => 1, "bar" => 2); r sort keys %h'
bar	foo
```

The function `values` returns values in the same order as the hash's `keys`.

```bash
$ ni 1p'my %h = ("foo" => 1, "bar" => 2); @ks = keys %h; @vs = values %h; my $same_order = 1; for(my $i = 0; $i <= $#ks; $i++) { if($h{$ks[i]} != $vs[i]) {$same_order = 0; last} } r $same_order ? "Same order" : "Different order"'
Same order
```

### `%ENV`: Hash of Environment Variables

Environment variables are much easier to access and modify in Perl than in any comparable language. To get the value of the variable you want, remember to dereference with `$ENV{varname}`.

```sh
$ ni 1p'%ENV'
...
LOGNAME
bilow
_system_type
Darwin
SHELL
/bin/bash
...
```


## Logical and Bitwise Operations

### Logical Operations

Logical operators come in two flavors, high-precedence and low-precedence. The low-precedence operators `and`, `or`, `not`, and their high-precedence C-like counterparts `&&`, `||`, `!`.

In general, there is little difference between these operations, but tricky errors may arise. In general, it is safe and syntactically pleasing to use the low-precedence operators between statements, and to use the high-precedence operators with scalars and expressions.

```sh
open my $fh, '<', $filename || die "A horrible death!"; ## Fails
open my $fh, '<', $filename or die "A horrible death!"; ## Succeeds
```


### Bit Shifts `<<` and `>>`

Bit shifting is useful because it is an incredibly fast operation; it's very useful for building hash functions and reducing computational burden. 

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

The exception to this rule is when the number of bits to shift is larger than the number of bits of the registers in the underlying hardware. Be careful when using bit shifts with large numbers, and using bit shifts on numbers that might be negative. The results can be quite unexpected.

```sh
$ ni 1p'r 18 << 63, -1 >> 10'
0	18014398509481983
```

### Hexadecimal Numbers

You can write numbers in hexadecimal (base-16) in Perl without specifying a type:

```bash
$ ni 1p'r 0x3 + 0xa, 0x3 + 0xA'
13	13
```

### Bitwise `&` and `|`

These operations can be useful for unwinding loops in programs; like bit shifts, they're very fast and convenient for running on binary-like data.

```bash
$ ni 1p'r 0x3 & 0xa, 0x3 | 0xa'
2	11
```

They also work on decimal numbers:


```bash
$ ni 1p'r 3 & 10, 3 | 10'
2	11
```

## Common Tricks


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

```sh
$ ni ifoo p'my %h = ("foo" => 32); $h{a}'
```

Returns nothing; in fact it is looking for the value with the key `"a"`.

```bash
$ ni ifoo p'my %h = ("foo" => 32, "a" => "hello"); $h{a}'
hello
```

In this case we prefix with a single `+` to indicate that the bareword in braces should be interpreted as a function.


```bash
$ ni ifoo p'my %h = ("foo" => 32); $h{+a}'
32
```

### `perldoc`	
Since `ni` sits on top of Perl, so we can take credit for the excellent Perl docs, which are often better than Google/StackOveflow for syntax help. You can get them at `perldoc -f <function>` at the command line.

```sh
$ perldoc -f my
...
       my EXPR
       my TYPE EXPR
       my EXPR : ATTRS
       my TYPE EXPR : ATTRS
               A "my" declares the listed variables to be local (lexically) to
               the enclosing block, file, or "eval".  If more than one value
               is listed, the list must be placed in parentheses.
...
```



## Conclusion

Perl is much-maligned for its syntax; much of that malignancy comes from people whose only exposure to the language is hearing about the [Obfuscated Perl Contest](https://en.wikipedia.org/wiki/Obfuscated_Perl_Contest). Perl is also known for its religious overtones; here `ni` author Spencer Tipping drop the scales from your eyes about the language in this section from his short intro to the language, [Perl in 10 Minutes](https://github.com/spencertipping/perl-in-ten-minutes). 


>Python, Ruby, and even Javascript were designed to be good languages -- and just as importantly, to **feel** like good languages. Each embraces the politically correct notion that values are objects by default, distances itself from UNIX-as-a-ground-truth, and has a short history that it's willing to revise or forget. These languages are convenient and inoffensive by principle because that was the currency that made them viable.

>Perl is different.

>In today's world it's a neo-noir character dropped into a Superman comic; but that's only true because it changed our collective notion of what an accessible scripting language should look like. People often accuse Perl of having no design principles; it's "line noise," pragmatic over consistent. This is superficially true, but at a deeper level Perl is uncompromisingly principled in ways that most other languages aren't. Perl isn't good; it's complicated, and if you don't know it yet, it will probably change your idea of what a good language should be.









