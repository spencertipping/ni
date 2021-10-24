use POSIX ();

our $pager_fh;
our $handling_pipe;

defconfenv 'pager', NI_PAGER => 'less -F -u';

sub child_status_ok($) {
  $_[0] == 0                    # ok exit status
    or ($_[0] & 127) == 13      # killed by sigpipe
    or ($_[0] & 127) == 15      # killed by sigterm (probably from us)
}

$ni::main_operator = sub {
  my @children;

  # Awkward TMPDIR fix for mac OSX
  delete $ENV{TMPDIR} unless -d $ENV{TMPDIR} and -w $ENV{TMPDIR};

  if (-t STDIN) {
    nuke_stdin;
  } else {
    # Fix for bugs/2016.0918.replicated-garbage.md: forcibly flush the STDIN
    # buffer so no child process gets bogus data.
    cdup2 0, 3;
    close STDIN;
    cdup2 3, 0;
    POSIX::close 3;
    open STDIN, '<&=0' or die "ni: failed to reopen STDIN: $!";

    push @children, sicons {sdecode};
  }

  my @ops = apply_meta_operators @_;
  @$_ and push @children, sicons {srand(srand() ^ $$); operate @$_} for @ops;

  if (-t STDOUT) {
    $pager_fh = siproc {exec conf 'pager' or
                        exec 'less' or exec 'more' or sio};
    sforward \*STDIN, $pager_fh;
    close $pager_fh;
    $pager_fh->await;
    ni::procfh::kill_children 'TERM';
  } else {
    sio;
  }

  my $exit_status = 0;
  for (@children) {
    my $status = $_->await;
    next if child_status_ok $status;
    print STDERR "ni: nonzero exit status for child process $_\n";
    $exit_status = 1;
  }
  $handling_pipe ? 0 : $exit_status;
};

# Pagers and kill signals.
# `less` resets a number of terminal settings, including character buffering and
# no-echo. If we kill it directly with a signal, it will exit without restoring a
# shell-friendly terminal state, requiring the user to run `reset` to fix it. So
# in an interrupt context we try to give the pager a chance to exit gracefully by
# closing its input stream and having the user use `q` or similar.

$SIG{PIPE} = sub {
  $handling_pipe = 1;
  close $pager_fh if $pager_fh;
  ni::procfh::kill_children 'TERM';
  exit 0;
};

$SIG{TERM} =
$SIG{HUP}  = sub {
  close $pager_fh if $pager_fh;
  ni::procfh::kill_children 'TERM';
  exit 1;
};

$SIG{INT} = sub {
  if ($pager_fh) {
    close $pager_fh;
    $pager_fh->await;
  }
  ni::procfh::kill_children 'TERM';
  exit 1;
};
