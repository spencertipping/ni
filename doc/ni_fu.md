# `ni`-fu

`ni`-fu is a companion piece to the [cheatsheet](cheatsheet_op.md). The cheatsheet serves as a reference to `ni` operators, while `ni`-fu serves as a reference for `ni` programming in general, covering topics like configuration, plotting, debugging, and coding best practices.

## How do I write `ni`?

`ni` is a stream-processing language, and each piece of `ni` code (which we call a spell) is a pipeline composed of operators (which maybe we should call "runes" or "glyphs" or something, but we don't). 

This makes it very easy to see `ni` code very easy to debug, even for a beginner.

All you need to do to see a `ni` spell in action is to run each piece from beginning to end.

For example, consider the spell:

`$ ni n3 fAA p'r alph a, a' OB`

If you don't know what this code does, split it up at whitespace between operators, yielding:

```
n3
fAA
p'r alph a, a'
OB
```

To see how this pipeline works, first run it using just the first operator:

```
$ ni n3
1
2
3
```

Pretty straightforward. Now, run the pipeline once again, using the first two operators:

```
$ ni n3 
1	1
2	2
3	3
```

A little more interesting; if you didn't know before, you can use `f` to duplicate columns in the stream. Once more, using the first 3 operators:


```
$ ni n3 fAA p p'r alph a, b'
A	1
B	2
C	3
```

The perl mapper `p'r alph a, b'` printed the Nth capital letter of the alphabet and 

And finally, putting everything together:

```
$ ni n3 fAA p'r alph a, b' OB
C	3
B	2
A	1
```

`OB` sorted the data in reverse numerical order based on column B.

You can see each stage of the pipeline did one transformation to its input data. Thus, if you have a spell whose output you don't understand or know is incorrect, by stepping through the operators, you should be able to see where the data transformation goes wrong.


## What should I know about `ni` configuration?

`ni` has dozens of configuration variables, which can be modified in your `~/.bash_profile`. A few of the most important ones to know about are covered here:

* `NI_PAGER`
  * The default `ni` pager is `less`. Generally, this is a safe and sensible choice. However, when using `ni` inside a Jupyter notebook, for exmaple, `less` will block the notebook, and you'll want to use `cat` or `head -n1000` as your pager, which won't block.
* `NI_NO_MONITOR`
  * The `ni` monitor occasionally causes issues (that are MapReduce-related, if I recall correctly). After working with `ni` for several months, I got a good feel for how fast each operation was and turned off the monitor.
* `NI_HADOOP_MAPREDUCE_CONF` 
  * This is a good place to store your Hadoop MapReduce Job defaults. The format of the 
* `NI_HADOOP_STREAMING_JAR`
  * You'll want to have some control over the Hadoop Streaming JAR that `ni` uses.

## What is `ni` is bad at?

`ni` is good at a lot of things; it's fast, it runs everywhere, it spins up Hadoop Streaming jobs easily, interacts in ways that are highly intuitive with the command line, and much more. However, `ni` has weaknesses of which you should also be aware.

### Processing arbitrary text
  * Newlines in text **will** result in unexpected behavior, since `ni` will interpret the newiline as the beginning of a new line of the stream.
  * On the other hand, `ni` is blazing fast and very flexible when working with structured text.

### Arbitrary JSON operations
  * `ni` _can_ do arbitrary JSON operations, but they are slow.
  * On the other hand, `ni` is blazing fast at doing **very specific** JSON operations.

### Sorting data on a single machine
  * This is not so much a weakness of `ni` as it is a weakness of your machine. It takes an excruciating amount of time to sort a gigabyte compared to processing it. 
  * When you have large quantities of data that need to be sorted, you should use `ni`'s very intuitive MapReduce bindings.

### Fancy Math and Stats
  * `ni` includes `numpy` bindings, but remember that this introduces a dependency on system configuration that should generally be avoided.
  * `ni` is data-processing and data-cleaning first, not math-first language; Spark with MLLib is probably the most appropriate tool if you're looking to do large matrix operations.
  * `ni` does a good job of finding counts and sums, but things like complicated averages, standard deviation, etc. are better done in Python or R.
* That's it. I think. For now.

## Basic `ni` Philosophy and Style

### `ni` is written for concision, fast testing, and productivity.

* Because `ni` processes streams of data in a streaming way, you should be able to build pipelines up from front-to-back, checking the integrity of the output at each step, for example by taking some rows using `r30` and/or sinking the output to a file.
* As a result of this, `ni` spells can be developed completely in the command line; there is:
  * No compilation (in the most common sense).
  * No need for a text editor.
  * No need to switch windows.
  * Nothing stopping you from joyful hacking.
* If you find a common operation is taking a while, there's probably a faster way to do it. Ask. It may require more Perl, though.

### Everything is optional *unless its absence would create ambiguity*.

* Consider the following three `ni` spells, all with the same output.
  * Explicit: `ni n10p'r "HELLO"' p'r lc(a)'`
  * Concise: `ni n10p'HELLO' p'lc a'`
  * Compact: `ni n10pHELLO plc`
* The technical details behind the `ni` parser are out of scope for this tutorial, but note the `ni` parser doesn't **need** quotes around perl snippets if their meaning is clear. It's a matter of programmer style whether you prefer to use them, but over time you'll be able to read both paradigms and likely end up preferring compact code when the meaning is clear.
* `ni` carries over some of the philosophy of implicit and default values from Perl, so you don't need to tell the lowercase function `lc` on what to operate; it will operate on the entire input line.



### `ni` is not meant to be easy to read for those who do not speak `ni`.

* This principle naturally follows from the two, but it is worth articulating to understand why the `ni` learning curve is steep. If you come to `ni` from a Python/Ruby background, with their almost English-like syntax, you may find `ni` unfriendly at first. 
* If you're coming from a heavy-featured scripting language, try to take joy in the speed of development that `ni` provides. `ni`'s row and column selection operations are much easier to discuss than `pandas`' `.loc` and `.ix`, for example.
* Just as most `ni` spells are built front-to-back, they are best understood by `ni` learners back-to-front. By repeatedly clipping operations off the end, you can see the entire sequence of intermediate outputs of the processing pipeline, and the magic of `ni`--building complex processing pipelines from single letters--becomes clear.
* `ni` is magic. Magic is not meant to be understood by the uninitiated. That's why wizards live in towers and why `ni` snippets are properly referred to as spells.


## Understanding the `ni` monitor
More details [here](monitor.md). Overall:

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step


## Plotting with `ni --js`
Check out the [examples](examples.md) for some examples of cool, interactive `ni` plotting.

### Formula Bar
You can enter ni formulas into the top bar (without the explicit `ni` call).

### Controls

- D : Distance
  - D represents the distance from the camera to the origin
- R : Rotation
  - The first R component is the angle between the image and the plane of the image and the plane of the screen
  - The second R component is the rotation of the image within the plane of the screenin degrees
- x : Dimensional Scaling
  - The first component controls scaling in the direction of the width of the screen;
  - The second component controls scaling in the direction of the depth of the screen;
  - The third component controls scaling in the direction of the height of the screen.

### Viewing a 3D plot to 2D

Set the second x component to 0 to flatten the image's depth dimension; then
set the first R component to 0 and the second R component to 90 to show a
front-facing view of your plot.
