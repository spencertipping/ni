# Core libraries for the ni image
Loaded in this order:

- `behavior`: self-descriptive objects that build Perl classes
- `class`: Smalltalk-80 metaclasses defined using behaviors
- `doc`: structured documentation and unit testing
- `fn`: a redefinition of function objects from `../boot`
- `image`: code to serialize objects into ni images
- `self`: a redefinition of the ni image from `../boot`, plus main code

Documentation is intentionally sparse in `behavior` and `class`; we don't have
a way to persist it yet, so nothing we write there would end up being available
to ni in a compiled image. I make up for this in `doc`, which goes back and
documents them. Everything after that is documented when it's defined.
