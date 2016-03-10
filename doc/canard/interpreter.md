# Interpreter logic
The canard interpreter state is always defined by three lists, each of which is
used as a stack. The lists are `d` (data stack), `c` (continuation stack), and
`r` (resolver stack). Here's an example of how they work evaluating the list
`[+ 3 4]`:

```
d = []     c = [[+ 3 4]]  r = ...
d = [4]    c = [[+ 3]]    r = ...
d = [4 3]  c = [[+]]      r = ...
d = [7]    c = [[]]       r = ...
```

I've left `r` opaque for now. Operationally, canard unpacks the topmost
continuation one symbol at a time, uses `r` to resolve symbols, and updates the
data stack as it goes. Here's exactly how the interpreter works:

```c
// NB: argument ordering is cons(tail, head)
while (true)
{
  // Remove nil continuation entries
  while (c != nil && head(c) == nil) c = tail(c);

  // Grab the next continuation entry
  command = head(c);
  c       = tail(c);

  // If it's a list, unpack it; otherwise just use it
  if (type(command) == CONS)
  {
    command = head(command);
    c       = cons(c, tail(command));
  }

  // Now we have the next instruction as "command", so figure out how to
  // execute it
  switch (type(command))
  {
    case NUMBER:
      // Reference to native function: execute it directly
      (*native_functions[command])(d, c, r);
      break;

    case SYMBOL:
      // Request a symbol resolution by pushing the symbol onto "d" and pushing
      // the topmost resolver as a continuation. We also schedule an eval here
      // to run the resolved value, since the symbol needs to be executed.
      d = cons(d, command);
      c = cons(cons(c, EVAL_NATIVE), head(r));
      break;

    // Everything else is self-quoting.
    default:
      d = cons(d, command);
      break;
  }
}
```

Canard has some interesting properties as a result of this design:

1. You can resolve symbols in arbitrary ways; the resolver is just a function.
2. Functions are at liberty to transform their continuations, which is how most
   macros are written.
3. The interpreter state can be quoted and inspected, which makes it trivial to
   virtualize the interpreter.

ni makes extensive use of (1) and (2).
