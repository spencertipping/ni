#!/usr/bin/env perl
use strict;
use warnings;
use Socket;

use constant sockfile => "unix-dgram.sock";
unlink sockfile if -e sockfile;
unlink sockfile . ".client" if -e sockfile . ".client";

socket my $server, PF_UNIX, SOCK_DGRAM, 0 or die "[server] socket: $!";
bind $server, sockaddr_un sockfile        or die "[server] bind: $!";

socket my $client, PF_UNIX, SOCK_DGRAM, 0 or die "[client] socket: $!";
bind $client, sockaddr_un sockfile . ".client" or die "[client] bind: $!";
connect $client, sockaddr_un sockfile     or die "[client] connect: $!";

send $client, "foo", 0 or die "[client] send failed: $!";
defined(my $sender = recv $server, $_, 8, 0) or die "[server] recv failed: $!";
printf "got %d byte(s) from %s: '%s'\n", length, $sender, $_;

send $server, "got it", 0, $sender or die "[server] send failed: $!";
defined(my $sender2 = recv $client, $_, 8, 0) or die "[client] recv failed: $!";
printf "got %d byte(s) back from %s: '%s'\n", length, $sender2, $_;

close $client;
close $server;
