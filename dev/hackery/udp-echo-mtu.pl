#!/usr/bin/env perl
use strict;
use warnings;
use Socket;
use Digest::MD5 qw/md5/;
use Time::HiRes qw/time/;

socket UDP, PF_INET, SOCK_DGRAM, getprotobyname 'udp';
bind UDP, sockaddr_in $ARGV[0], INADDR_ANY
  or die "bind: $!";

my $sendto = sockaddr_in 8888, inet_aton $ENV{EC2};
for (1..16384) {
  next if $_ % 256;
  my $s = ' ' x $_;
  my $h1 = md5 $s;
  send UDP, $s, 0, $sendto or die "send: $!";
  print STDERR "sent $_ bytes\n";
  my $t1 = time;
  recv UDP, $_, 16384, 0 or die "recv: $!";
  my $t2 = time;
  my $h2 = md5 $_;
  print STDERR "got reply: " . length . " bytes in " . int(($t2 - $t1)*1_000_000) . "μs\n";
  print STDERR "##############\nHASH MISMATCH\n########\n\n" unless $h1 eq $h2;
}
