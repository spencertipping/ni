{

use v5.14;
no strict 'refs';
package ni;
sub ni;
sub ::ni;

sub self {
  join "\n", "#!/usr/bin/env perl",
             q{$ni::selfcode = '';},
             q{$ni::selfcode .= ($_ = <DATA>) until /^__END__$/;},
             q{eval $ni::selfcode;},
             q{die $@ if $@;},
             "__DATA__",
             $ni::selfcode,
             "__END__";
}

use POSIX qw/:sys_wait_h/;

$SIG{CHLD} = sub {
  local ($!, $?);
  waitpid -1, WNOHANG;
};
