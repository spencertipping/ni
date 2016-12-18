#Debugging

As a result of `ni`'s concision, even experienced developers can have difficulty identifying errors. 

##Parsing Errors

###Bracketed operators

If you get an error involving a bracketed operator and you're sure you've written everything correctly, try inserting whitespace after the opening bracket and before the closing bracket.

###Hadoop Streaming

`$ ni n1 HS [p'r a'] _ _`
gives the error: `ni: failed to parse starting here (ni --dev/parse to trace):
  _`
  
```
<HUGE NUMBER OF LINES OF PARSE TRACE>
 ni::parse [[/series], n1, HS, [pr a], _, _] = [[[n, 1, 2], [hadoop_streaming, [], [[perl_mapper, r a]], undef]], _]
```
The hadoop streaming parser interprets the next character after the `S` in `HS` as the 

##Compilation Errors
Generally these will stem from an error with some of your Perl. Ask someone for help.

##Hadoop Streaming Errors
The first thing to do when hunting for errors in hadoop streaming jobs is to make sure that your mapper, combiner, and reducer run outside of an `HS` context. If they do, then 

###Hadoop Streaming .jar too large
When `ni` runs hadoop streaming jobs, it sends **itself and all data** to the job server. If you have a large data closure, that closure will be included in the payload sent to the job server. If your closure is larger than a couple hundred megabytes, you should try a different approach to the problem (e.g. use an HDFS join [coming soon to `ni`, use [nfu]`nfu` for now]).


##Job is too slow
`ni` is a stream-processing language; every job should be fast (or at least, start spitting out output fast).  See [optimization.md](optimization.md) for tips on speeding up slow jobs.


##Erroneous Output
The other important type of error to debug are ones that run completely but give the wrong output. Here are some commmon pitfalls of these 

###Sorting doesn't work how I want
As of 2016-12-18, sorting works in a way that is unintuitive on **certain** operating systems. Specifically, `ni`'s `g` operator ignores blanks (i.e. separators between columns) on certain operating systems, and n general, you will want to 

###Referencing the wrong column after re-indexing

Running `ni n5 p'r a, a, a, a' fBCD p'r b*3 + c*2 + d'` gives multiples of 5, and not multiples of 6 as you might expect. The reason is that once `fBCD` is run, the columns are re-indexed such that the column that was `B` would now be accessed by `A` (or `a`) in a Perl context. 

Note that `ni` allows you to use columns that are undefined. For example, `ni n5 xC` will give you data with two empty columns followed by the input stream.

###Spelling Errors
When developing `ni` pipelines, it is often the case that the name of one data closures will change in one place but not another.

###Quoted HDFS Paths for Hadoop Streaming
Note that when using a HDFS paths for Hadoop Streaming jobs, you __must*__  quote the input path from 

*Okay, it might work if you don't quote the path, but it'll take [forever](optimization.md).

###Perl 
If something's going wrong in a Perl context, but it looks right, try being more explicit (for example, using `b()` rather than `b` to reference the second field)