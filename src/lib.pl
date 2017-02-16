do "src/lib/$_" or die "$@ $!" for qw/behavior class doc self/;

ni('ni:/lib/doc')->new('/lib')
  ->description(
    q[Bootstrapping code for the core abstractions in ni, and almost everything
      about its introspection. This includes definitions for documentation,
      unit tests, classes, support for basic image generation, etc -- and when
      possible, it's written with some awareness of downstream use cases (for
      instance, image serialization and RMI share logic).]);

ni->extend("src/lib/$_") for
  qw/ fn
      json
      test
      quote
      image /;
