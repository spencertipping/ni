#!/usr/bin/env perl
eval($ni::bootcode = <<'end');
$ni::selfdata = join '', <DATA>;
sub ni::self {
  join "\n", "#!/usr/bin/env perl",
             "eval(\$ni::bootcode = <<'end');\n${ni::bootcode}end",
             'die $@ if $@;',
             "__DATA__",
             $ni::selfdata;
}
eval $ni::selfdata;
die $@ if $@;
end
die $@ if $@;
__DATA__
