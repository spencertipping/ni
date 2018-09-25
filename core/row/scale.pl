# Row-based process scaling.
# Allows you to bypass process bottlenecks by distributing rows across multiple
# workers.

BEGIN {defshort '/S', defalt 'scalealt', 'row scaling alternation list'}

# Fixed scaling.
# The simplest option: specify N workers and a lambda, and the lambda will be
# replicated that many times. Incoming data is broken into chunks of rows and
# written to any worker that's available.

# Implementation-wise here's how this works. We're distributing a single stream
# of data to potentially a lot of subprocesses, each of which might be quite
# fast. So we need to optimize this aggressively, which in this case consists of
# the following:

# | 1. We maintain an input queue for each worker for reasons described below.
#   2. We minimize the overhead involved in line-splitting blocks, avoiding it
#      entirely for most of the input data.
#   3. We keep writes to workers small enough that they won't block.

# Visually, here's what we're doing:

# | worker 1 <- [block 1] [block 2] [block 3] [block 4] [part of block 5]
#   worker 2 <- [rest of part 5] [block 6] [block 7] [block 8] [part of block 9]
#   worker 3 <- ...

# The key here is that we can avoid line-splitting for any two consecutive blocks
# we send to the same worker, so we want every queue-fill operation to consume a
# large number of blocks. This leads to a possibly counterintuitive heuristic: we
# deliberately let queues run low before refilling them.

# Reading from the workers' stdout is exactly the same: we enqueue a bunch of
# blocks and then line-merge once the queues are full enough.

# A note about blocking: the scale operator goes to some lengths to avoid
# blocking on the workers, but it's fine and expected for it to block on its own
# stdin and stdout. The only consideration there is that we try to interleave
# worker and stdin/stdout blocking; this increases the likelihood that we'll
# saturate source and/or sink processes.

# TODO: refactor this to make the pieces available elsewhere. Should probably end
# up with various "combine streams vertically/horizontally/etc" library
# functions.

defconfenv 'scale/ibuf', NI_SCALE_INPUT_BUFFER  => 32768;
defconfenv 'scale/obuf', NI_SCALE_OUTPUT_BUFFER => 32768;

defoperator row_fixed_scale => q{
  my $ibuf = conf 'scale/ibuf';
  my $obuf = conf 'scale/obuf';

  sub new_ref() {\(my $x = '')}

  my ($n, $f) = @_;
  conf_set monitor => 0;

  my ($iqueue, $oqueue) = (64, 64);

  my (@wi, @wo);
  my ($wb, $rb, $w, $r);
  my ($ib, $ob, $ibtmp, $obtmp);
  for (1..$n) {
    my ($i, $o) = sioproc {
      setpriority 0, 0, $n >> 2;
      &$ni::main_operator(flatten_operators $f);
      exit;
    };
    push @wi, $i;
    push @wo, $o;
    vec($wb, fileno $i, 1) = 1;
    vec($rb, fileno $o, 1) = 1;
  }

  vec($ib, fileno STDIN,  1) = 1;
  vec($ob, fileno STDOUT, 1) = 1;

  my $stdout_reader = siproc {
    my @bufs;
    my $buf_limit = $oqueue * $n;
    my @stdout = map [], @wo;
    my @outqueue;
    my $b;
    my $stdout = \*STDOUT;

    close $_ for @wi;

    while ($n) {
      until (@outqueue < $oqueue * $n) {
        safewrite $stdout, ${$b = shift @outqueue};
        push @bufs, $b unless @bufs >= $buf_limit;
      }

      select $r = $rb, undef, undef, undef;
      for my $i (0..$#wo) {
        next unless defined $wo[$i];
        next unless vec $r, fileno $wo[$i], 1;

        while (@outqueue and select undef, $obtmp = $ob, undef, 0) {
          safewrite $stdout, ${$b = shift @outqueue};
          push @bufs, $b unless @bufs >= $buf_limit;
        }
        my $so = $stdout[$i];
        if (saferead $wo[$i], ${$b = pop(@bufs) || new_ref}, $obuf) {
          push @$so, $b;
          my $np;
          if (@$so >= $oqueue and 0 <= ($np = rindex $$b, "\n")) {
            push @outqueue, @$so[0..$#{$so} - 1];
            push @outqueue, \(my $x = substr $$b, 0, $np + 1);
            $$b = substr $$b, $np + 1;
            @$so = ($b);
          }
        } else {
          --$n;
          vec($rb, fileno $wo[$i], 1) = 0;
          close $wo[$i];
          push @outqueue, @$so;
          $stdout[$i] = $wo[$i] = undef;
        }
      }
    }

    safewrite $stdout, $$_ for @outqueue;
  };

  close $stdout_reader;
  close $_ for @wo;

  {
    my @bufs;
    my $buf_limit = $iqueue * $n;
    my @stdin = map [], @wi;
    my @queue;
    my $eof;
    my $b;
    my $stdin = \*STDIN;

    until (!@queue && $eof) {
      select undef, $w = $wb, undef, undef;
      for my $i (0..$#wi) {
        next unless vec $w, fileno $wi[$i], 1;

        my $si = $stdin[$i];
        if (@$si * 4 < $iqueue) {
          # Commit to refilling this stdin queue, which means we need to write
          # exclusively to this one until we find a line break.
          push @$si, shift @queue while @$si < $iqueue and @queue;
          while (@queue or not $eof) {
            unless ($b = $queue[0]) {
              last if $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, $ibuf;
              push @queue, $b;
            }

            my $np;
            if (0 <= ($np = rindex $$b, "\n")) {
              push @$si, \(my $x = substr $$b, 0, $np + 1);
              $$b = substr $$b, $np + 1;
              last;
            } else {
              push @$si, shift @queue;
            }
          }
        }

        $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, $ibuf
        or push @queue, $b
          while @queue < $iqueue * $n and !$eof
            and select $ibtmp = $ib, undef, undef, 0;

        if (@$si) {
          safewrite $wi[$i], ${$b = shift @$si};
          push @bufs, $b unless @bufs >= $buf_limit;
        }

        $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, $ibuf
        or push @queue, $b
          while @queue < $iqueue * $n and !$eof
            and select $ibtmp = $ib, undef, undef, 0;
      }
    }

    # Run out the individual queues.
    for my $i (0..$#wi) {
      safewrite $wi[$i], $$_ for @{$stdin[$i]};
      close $wi[$i];
    }
  }

  $_->await for @wo;
  $stdout_reader->await;
};

defscalealt pmap q{row_fixed_scale_op @$_}, pseq integer, _qfn;
