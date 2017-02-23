ni('ni:/module')->new('/semantic')
  ->doc
  ->description(
    q[Opportunities to assign real-world semantics to objects. This is a
      collection of behaviors that don't necessarily imply a Perl-level
      protocol, but which may end up meaning something at some point.]);

ni->extend("src/semantic/$_") for
  qw/ dimension
      task /;
