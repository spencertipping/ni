ni('ni:/module')->new('/io')
  ->doc
  ->description(
    q[An implementation of IO in terms of system-level FDs. We need this for a
      few reasons, three of them being that (1) old versions of Perl don't
      correctly handle interrupted system calls, (2) we want tighter control
      over which FDs are closed at what times, and (3) we want to be able to
      "unread" things -- push back against the read buffer (or use a custom
      read format in general).]);

ni->extend("src/io/$_") for
  qw/ object
      buffer
      cat
      exec
      fd
      file
      null
      pid
      str
      transfer /;