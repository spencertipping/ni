#!/usr/bin/env perl
$ni::selfcode = '';
$ni::selfcode .= ($_ = <DATA>) until /^__END__$/;
$ni::data_fh = \*DATA;
eval $ni::selfcode;
die $@ if $@;
1;
__DATA__
