#!/usr/bin/perl
#
# udp.pl - listen on UDP port and reply with stuff

use strict;
use warnings;
use Socket;

die "Usage: udp.pl <port>" unless @ARGV;

socket UDP, PF_INET, SOCK_DGRAM, getprotobyname "udp";
bind UDP, sockaddr_in $ARGV[0], INADDR_ANY;
my $a;
send UDP, $_, 0, $a while $a = recv UDP, $_, 8192, 0;
