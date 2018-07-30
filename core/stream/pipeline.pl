# Pipeline construction.
# A way to build a shell pipeline in-process by consing a transformation onto
# this process's standard input. This will cause a fork to happen, and the forked
# PID is returned.

# I define some system functions with a `c` prefix: these are checked system
# calls that will die with a helpful message if anything fails.

no warnings 'io';

use Errno qw/EINTR/;
use POSIX qw/dup2/;

sub cdup2 {defined dup2 $_[0], $_[1] or die "ni: dup2(@_) failed: $!"}
sub cpipe {pipe $_[0], $_[1] or die "ni: pipe failed: $!"}
sub cfork {my $pid = fork; die "ni: fork failed: $!" unless defined $pid; $pid}

sub sh($) {exec 'sh', '-c', $_[0]}

sub nuke_stdin() {
  open STDIN, '</dev/null';
  if (fileno STDIN) {
    cdup2 fileno STDIN, 0;
    open STDIN, '<&=0';
  }
}

# Safe reads/writes.
# This is required because older versions of Perl don't automatically retry
# interrupted reads/writes. We run the risk of interruption because we have a
# SIGCHLD handler. nfu lost data on older versions of Perl because it failed to
# handle this case properly.

sub saferead($$$;$) {
  my $n;
  do {
    return $n if defined($n = sysread $_[0], $_[1], $_[2], $_[3] || 0);
  } while $!{EINTR};
  return undef;
}

sub safewrite($$;$$) {
  my $n;
  do {
    return $n if defined($n = syswrite $_[0], $_[1], $_[2] || length $_[1], $_[3] || 0);
  } while $!{EINTR};
  return undef;
}

sub saferead_exactly($$$;$) {
  my ($r, $n) = (0, 0);
  while ($r < $_[2]) {
    return undef unless $n = saferead $_[0], $_[1], $_[2] - $r, ($_[3] || 0) + $r;
    $r += $n;
  }
  $r;
}

sub safewrite_exactly($$) {
  my ($w, $n) = (0, 0);
  while ($w < length $_[1]) {
    return undef unless $n = safewrite $_[0], $_[1], length($_[1]) - $w, $w;
    $w += $n;
  }
  $w;
}


# Process construction.
# A few functions, depending on what you want to do:

# | siproc(&): fork into block, return pipe FH to block's STDIN.
#   soproc(&): fork into block, return pipe FH from block's STDOUT.
#   sicons(&): fork into block, connect its STDOUT to our STDIN.
#   socons(&): fork into block, connect our STDOUT to its STDIN.

# NOTE: forkopen does something strange and noteworthy. You'll notice that it's
# reopening STDIN and STDOUT from FDs, which seems redundant. This is required
# because Perl filehandles aren't the same as OS-level file descriptors, and ni
# deals with both in different ways.

# In particular, ni closes STDIN (the filehandle) if the input comes from a
# terminal, since presumably the user doesn't intend to type their input in
# manually. This needs to happen before any exec() from a forked filter process.
# But this creates a problem: if we later reactivate fd 0, which we do by moving
# file descriptors from a pipe. We have to do this at the fd level so exec()
# works correctly (since exec doesn't know anything about Perl filehandles, just
# fds). Anyway, despite the fact that fd 0 is newly activated by an sicons {}
# operation, Perl's STDIN filehandle will think it's closed and return no data.

# So that's why we do these redundant open STDIN and STDOUT operations. At some
# point I might bypass Perl's IO layer altogether and use POSIX calls, but at the
# moment that seems like more trouble than it's worth.

sub forkopen {
  my ($fd, $f, @args) = @_;
  cpipe my $r, my $w;
  my ($ret, $child) = ($r, $w)[$fd ^ 1, $fd];
  my $pid;
  if ($pid = cfork) {
    close $child;
    return ni::procfh->new($ret, $pid);
  } else {
    close $ret;
    cdup2 fileno $child, $fd;
    close $child;
    open STDIN,  '<&=0' if $fd == 0;
    open STDOUT, '>&=1' if $fd == 1;
    &$f(@args);
    exit;
  }
}

sub sioproc(&@) {
  my ($f, @args) = @_;
  cpipe my $proc_in, my $w;
  cpipe my $r, my $proc_out;
  my $pid;
  if ($pid = cfork) {
    close $proc_in;
    close $proc_out;
    return ($w, ni::procfh->new($r, $pid));
  } else {
    close $w;
    close $r;
    cdup2 fileno $proc_in, 0;
    cdup2 fileno $proc_out, 1;
    close $proc_in;
    close $proc_out;
    open STDIN,  '<&=0';
    open STDOUT, '>&=1';
    &$f(@args);
    exit;
  }
}

sub siproc(&@) {forkopen 0, @_}
sub soproc(&@) {forkopen 1, @_}

sub sicons(&@) {
  my ($f, @args) = @_;
  my $fh = soproc {&$f(@args)};
  cdup2 fileno $fh, 0;
  close $fh;
  open STDIN, '<&=0';
  $fh;
}

sub socons(&@) {
  my ($f, @args) = @_;
  my $fh = siproc {&$f(@args)};
  cdup2 fileno $fh, 1;
  close $fh;
  open STDOUT, '>&=1';
  $fh;
}

# Stream functions.
# These are called by pipelines to simplify things. For example, a common
# operation is to append the output of some data-producing command:

# | $ ni . .              # lists current directory twice

# If you do this, ni will create a pipeline that uses stream wrappers to
# concatenate the second `ls` output (despite the fact that technically it's a
# shell pipe).

sub sforward($$) {local $_; safewrite_exactly $_[1], $_ while saferead $_[0], $_, 8192}
sub stee($$$)    {local $_; safewrite_exactly($_[1], $_), safewrite_exactly($_[2], $_) while saferead $_[0], $_, 8192}
sub sio()        {sforward \*STDIN, \*STDOUT}

sub srfile($) {open my $fh, '<', $_[0] or die "ni: srfile $_[0]: $!"; $fh}
sub swfile($) {open my $fh, '>', $_[0] or die "ni: swfile $_[0]: $!"; $fh}

# Compressed stream support.
# This provides a stdin filter you can use to read the contents of a compressed
# stream as though it weren't compressed. It's implemented as a filter process so
# we don't need to rely on file extensions.

# We detect the following file formats:

# | gzip:  1f 8b
#   bzip2: BZh[1-9]    (sometimes \0 instead of the digit)
#   lzo:   89 4c 5a 4f
#   lz4:   04 22 4d 18
#   xz:    fd 37 7a 58 5a
#   xlsx:  50 4b 03 04 14 00 06 00 (actually this is all MS OOXML)
#   xls:   d0 cf 11 e0 a1 b1 1a e1

# Decoding works by reading enough to decode the magic, then forwarding data
# into the appropriate decoding process (or doing nothing if we don't know what
# the data is).

sub sdecode(;$) {
  local $_;
  return unless saferead \*STDIN, $_, 8192;

  my $decoder = /^\x1f\x8b/             ? "gzip -dc || cat"
              : /^BZh[1-9\0]/           ? "pbzip2 -dc || bzip2 -dc || cat"
              : /^\x89\x4c\x5a\x4f/     ? "lzop -dc || cat"
              : /^\x04\x22\x4d\x18/     ? "lz4 -dc || cat"
              : /^\xfd\x37\x7a\x58\x5a/ ? "xz -dc || cat" 
              : /^\x50\x4b\x03\x04\x14\x00\x06\x00/ ? "python -c \"${excel_cat->()}\""
              : /^\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1/ ? "python -c \"${excel_cat->()}\""
              : undef;

  if (defined $decoder) {
    my $o = siproc {exec $decoder};
    safewrite_exactly $o, $_;
    sforward \*STDIN, $o;
    close $o;
    $o->await;
  } else {
    safewrite_exactly \*STDOUT, $_;
    sio;
  }
}

use constant excel_cat => gen pydent q{
  import pandas as pd
  from sys import stdin, stdout
  stdout = stdout.buffer
  df = pd.read_excel(stdin)
  for _idx, row in df.iterrows():
    stdout.write(bytes("\t".join([str(x) for x in row][1:]), "utf-8"))
    stdout.flush()};

# File/directory cat.
# cat exists to turn filesystem objects into text. Files are emitted and
# directories are turned into readable listings. Files are automatically
# decompressed and globs are automatically deglobbed.

sub glob_expand($) {-e($_[0]) ? $_[0] : glob $_[0]}

sub scat {
  local $| = 1;
  for my $f (map glob_expand($_), @_) {
    if (-d $f) {
      opendir my $d, $f or die "ni_cat: failed to opendir $f: $!";
      print "$f/$_\n" for sort grep !/^\.\.?$/, readdir $d;
      closedir $d;
    } else {
      my $d = siproc {sdecode};
      sforward srfile $f, $d;
      close $d;
      $d->await;
    }
  }
}
