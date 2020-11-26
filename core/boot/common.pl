# Regex parsing.
# Sometimes we'll have an operator that takes a regex, which is subject to the
# CLI reader problem the same way code arguments are. Rather than try to infer
# brackets the same way, we just require that regexes are terminated with /
# (which should be ok because that's also how they typically start).

BEGIN {defparseralias regex => pmap q{s/\/$//; $_}, prx qr{^(?:[^\\/]+|\\.)*/}}

docparser regex => q{Regular expression, delimited by slashes};


docparser generic_code => <<'_';
Counts brackets outside quoted strings, which in our case are '' and "".
Doesn't look for regular expressions because these vary by language; but this
parser should be able to handle most straightforward languages with quoted
string literals and backslash escapes.
_

defparser 'generic_code', '',
  q{my ($self, $code, @xs) = @_;
    return ($code, '', @xs) unless $code =~ /\]$/;
    (my $tcode = $code) =~ s/"([^"\\\\]+|\\\\.)"|'([^'\\\\]+|\\\\.)'//g;
    my $balance = length(sgr $tcode, qr/[^[]/, '') - length(sgr $tcode, qr/[^]]/, '');
    $balance ? (substr($code, 0, $balance), substr($code, $balance), @xs)
             : ($code, '', @xs)};


# Basic CLI types.
# Some common argument formats for various commands, sometimes transformed for
# specific cases. These are documented somewhere in `doc/`.

# A parsed column spec is an N-element array: [floor, cols...]. `floor` indicates
# the first column that would be selected by a `.` ("the rest").

BEGIN {defparseralias neval => pmap q{eval}, prx '=([^]=]+)'}
BEGIN {defparseralias integer => palt pmap(q{int},       neval),
                                      pmap(q{10 ** $_},  prx 'E(-?[\d_]+)'),
                                      pmap(q{1 << $_},   prx 'B([\d_]+)'),
                                      pmap(q{0 + "0$_"}, prx 'x[_0-9a-fA-F]+'),
                                      pmap(q{0 + $_},    prx '-?[1-9][\d_]*(?:[eE][\d_]+)?'),
                                                         pstr '0'}
BEGIN {defparseralias float => pmap q{0 + $_},
                               pcond q{length},
                               prx '-?(?:[\d_]+(?:\.[\d_]*)?|[\d_]*\.[\d_]+)(?:[eE][-+]?[\d_]+)?'}
BEGIN {defparseralias number => palt neval, float, integer}

BEGIN {defparseralias colspec1      => palt pn(1, pstr '#', integer),
                                            pmap q{ord() - 65}, prx '[A-Z]';
       defparseralias colspec_rest  => pmap q{-1}, pstr '.'}
BEGIN {defparseralias colspec_range => pmap q{[$$_[0] .. $$_[2]]},
                                       pseq colspec1, pstr '-', colspec1}
BEGIN {defparseralias colspec_fixed => pmap q{[max(@$_) + 1, @$_]},
                                       pmap q{[map ref() ? @$_ : $_, @$_]},
                                       prep pn(1, popt pstr ',',
                                                  palt(colspec_range, colspec1)), 1}
BEGIN {defparseralias colspec => pmap q{[max(@$_) + 1, @$_]},
                                 pmap q{[map ref() ? @$_ : $_, @$_]},
                                 prep pn(1, popt pstr ',',
                                            palt(colspec_range, colspec1, colspec_rest)), 1}

docparser neval => q{An expression evaluated by Perl; e.g. =3+4 for 7};
docparser colspec1 => q{A way to identify a single column; either A-Z or #N};
docparser colspec_rest => <<'_';
"The rest of the columns": everything to the right of the rightmost
explicitly-specified column
_

docparser colspec_range => q{A range of columns, e.g. A-Q or #10-#20};
docparser colspec_fixed => q{A set of definite columns; disallows '.' ("the rest")};
docparser colspec => q{A set of columns, possibly including '.' ("the rest")};

# Filenames, in general.
# Typically filenames won't include bracket characters, though they might include
# just about everything else. Two possibilities there: if we need special stuff,
# there's the `file://` prefix; otherwise we assume the non-bracket
# interpretation.
#
# There are cases where it's useful to compute the name of a file. If a filename
# begins with $, we evaluate the rest of it with perl to calculate its name.

BEGIN {defparseralias computed   => prx '^\$.*'}
BEGIN {defparseralias filename   => palt computed,
                                         prx 'file://(.+)',
                                         prx '\.?/(?:[^/]|$)[^]]*',
                                         pcond q{/^[\/\.]$/ or -e and length > 1}, prx '[^][]+'}
BEGIN {defparseralias nefilename => palt filename, prx '[^][]+'}

docparser filename   => q{The name of an existing file};
docparser nefilename => q{The name of a possibly-nonexisting file};
