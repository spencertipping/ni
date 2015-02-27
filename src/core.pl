#!/usr/bin/env perl
$ni::selfcode = '';
$ni::selfcode .= ($_ = <DATA>) until /^__END__$/;
eval $ni::selfcode;
die $@ if $@;
__DATA__
