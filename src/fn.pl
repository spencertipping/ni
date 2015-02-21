BEGIN {ni::extend {
  # Memoized function compilation
  package ni;
  %ni::compiled_functions = ();

  sub expand_function_shorthands {
    my ($code) = @_;
    $code =~ s/%(\d+)/\$_[$1]/g;

    # JSON shortcuts
    1 while $code =~ s/([a-zA-Z0-9_\)\}\]\?\$])
                       \.
                       ([\$_a-zA-Z](?:-[0-9\w\?\$]|[0-9_\w?\$])*)
                      /$1\->{'$2'}/x;
    $code;
  }

  sub compile {
    return $_[0] if ref $_[0] eq 'CODE';
    return eval "sub {\n" . expand_function_shorthands($_[0]) . "\n}";
  }
}}
