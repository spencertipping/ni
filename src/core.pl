#!/usr/bin/env perl
BEGIN {eval($ni::bootcode = <<'end')}
use B::Deparse;
$ni::deparser = B::Deparse->new;
@ni::code = ();
sub ni::self {
  join "\n", "#!/usr/bin/env perl",
             "BEGIN {eval \$ni::bootcode = <<'end'}\n${ni::bootcode}end",
             @ni::code;
}
sub ni::extend(&) {
  my $s = $ni::deparser->coderef2text($_[0]);
  push @ni::code, "BEGIN {ni::extend $s}";
  $_[0]->();
}
end
