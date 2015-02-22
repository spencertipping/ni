defdata 'globfile', sub { ref $_[0] eq 'GLOB' },
                    sub { ni::io::fh->new($_[0]) };
