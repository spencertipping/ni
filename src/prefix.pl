{

use v5.14;
no strict 'refs';
package ni;
sub ni;
sub ::ni;

DEBUG
use Carp;
$Carp::Verbose = 1;
DEBUG_END

sub self {
  join "\n", "#!/usr/bin/env perl",
             q{$ni::selfcode = '';},
             q{$ni::selfcode .= ($_ = <DATA>) until /^__END__$/;},
             q{$ni::data_fh = \*DATA;},
             q{eval $ni::selfcode;},
             q{die $@ if $@;},
             "__DATA__",
             $ni::selfcode,
             map "NI_MODULE $$_[0]\n$$_[1]\nNI_MODULE_END", @ni::modules;
}

use POSIX qw/:sys_wait_h/;

$SIG{CHLD} = sub {
  local ($!, $?);
  waitpid -1, WNOHANG;
};
