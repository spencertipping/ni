# Classes and metaclasses
This documentation assumes that you understand how Smalltalk-80 metaclasses
work (Ruby eigenclasses might do it, but they're fairly different). The
[wikipedia article on metaclasses](https://en.wikipedia.org/wiki/Metaclass) has
a good explanation of the different paradigms.

## Overall hierarchy
**TODO:** How do `/b/X` things fit into this picture?

```
/c/object    : /mc/object
/c/behavior  : /mc/behavior  << /c/object
                              + /b/named_persistent
                              + /b/named
                              + /b/TODO

/c/branch    : /mc/branch    << /c/behavior + ...
/c/tag       : /mc/tag       << /c/behavior + ...
/c/slice     : /mc/slice     << /c/behavior + ...
/c/class     : /mc/class     << /c/branch + ...
/c/metaclass : /mc/metaclass << /c/branch + ...
```
