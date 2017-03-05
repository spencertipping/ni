do "src/lib/$_" or die "$@ $!" for qw/behavior class util doc self/;

ni('ni:/module')->new('/lib')
  ->doc
  ->description(
    q[Bootstrapping code for the core abstractions in ni, and almost everything
      about its introspection. This includes definitions for documentation,
      unit tests, classes, support for basic image generation, etc -- and when
      possible, it's written with some awareness of downstream use cases (for
      instance, image serialization and RMI share logic).],

    q[/lib is the place where things don't quite work yet, so the code here is
      written differently from other modules.]);

ni->extend("src/lib/$_") for
  qw/ fn
      json
      test
      behavior_doc
      class_doc
      doc_doc
      quote
      image
      future /;
