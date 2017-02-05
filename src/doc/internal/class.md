# Classes and metaclasses
This documentation assumes that you understand how Smalltalk-80 metaclasses
work (Ruby eigenclasses might do it, but they're fairly different). The
[wikipedia article on metaclasses](https://en.wikipedia.org/wiki/Metaclass) has
a good explanation of the different paradigms.

## Overall hierarchy
"Instance of" is indicated by dashed lines (`- - - ->`), "superclass of",
which in ni means "includes slices from", is indicated by solid lines
(`---->`). I've also abbreviated `/class` to `/c`, `/behavior` to `/b`, and
`/metaclass` to `/mc`.

```
           +-------------------+
           V                   |
    /b/slice- - - >  /mc/slice-+          # this pattern is repeated for
            \             \    |          # /b/named_persistent and the other
             +              - -|- +       # behaviors mentioned below
             |                 |  |
   /b/branch-|- - > /mc/branch-+
            \|            \    |  |
             +              - -|- +
             |                 |  |
      /b/tag-|- - - >  /mc/tag-+
            \|            \       |
             |              - - - +
             |                    |
             |              + - - +
             |              V
             +------> /c/behavior - - - > /mc/behavior - - - - - - - - +
                      | ^  |   ^                                       |
                        |  |   +-----------------------------------+
                      | |  |                                       |   |
                        |  +--> /b/named_persistent --> /b/named   |
                      | |  +--> /b/mapped_to_package               |   |
                        |  +--> /b/ni_namespaced                   |
                      | |  +--> /b/lifecycle                       |   |
                        |                                          |
                      V |                                          |   |
                  /c/class < - - - > /mc/class --------------------+
                                             ^                         |
                                             + - - - - - - - - - - - - +
```
