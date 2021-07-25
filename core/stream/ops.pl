# Streaming data sources.
# Common ways to read data, most notably from files and directories. Also
# included are numeric generators, shell commands, etc.


sub eval_filename($) { eval "(sub{$_[0]})->()" }

sub filename_read($)
{
  # Return the logical filename for the specified thing.
  my $f = shift;
  return eval_filename $1 if $f =~ /^\$(.*)/;
  $f;
}

sub filename_write($)
{
  # Process a filename whose purpose is to be written into. This is like
  # reading, but we have a special case for undef, which becomes a new anonymous
  # tempfile.
  defined $_[0]
    ? filename_read $_[0]
    : resource_tmp('file://');
}

BEGIN {
  defparseralias multiword    => pn 1, prx '\[',  prep(prc '[\s\S]*[^]]', 1), prx '\]';
  defparseralias multiword_ws => pn 1, prc '\[$', prep(pnx '\]$',         1), prx '\]$';

  defparser 'super_brackets', '', q{
    my ($self, @xs) = @_;
    return () unless $xs[0] =~ s/^(\^[^[]*)\[//;
    my $superness = $1;
    my @r;
    push @r, shift @xs while @xs && $xs[0] !~ s/^(\Q$superness\E)\]//;
    $1 eq $superness ? (\@r, @xs) : ();
  };
}

BEGIN {
  defparseralias shell_command => palt pmap(q{shell_quote @$_}, super_brackets),
                                       pmap(q{shell_quote @$_}, multiword_ws),
                                       pmap(q{shell_quote @$_}, multiword),
                                       prx '[^][]+';

  defparseralias shell_arg => palt pmap(q{shell_quote @$_}, super_brackets),
                                   pmap(q{shell_quote @$_}, multiword_ws),
                                   pmap(q{shell_quote @$_}, multiword),
                                   pmap q{shell_quote $_},  prx '[^][]+';

  defparseralias id_text => palt pmap(q{join "\t", @$_}, super_brackets),
                                 pmap(q{join "\t", @$_}, multiword_ws),
                                 pmap(q{join "\t", @$_}, multiword),
                                 prx '[^][]+';
}

defoperator constant => q{my ($x) = @_; sio; print "$x\n" while 1};
defoperator echo     => q{my ($x) = @_; sio; print "$x\n"};
defoperator sh       => q{my ($c) = @_; sh $c};

defshort '/e', pmap q{sh_op $_}, shell_command;

# Cat meta-operator.
# We don't want 'cat' to be a regular operator because of how shell wildcards
# work. If you say something like `ni *`, it's going to get a bunch of filenames,
# each of which will fork out to another ni process. Most of these ni processes
# will just be copying stdin to stdout, a huge waste if there are a lot of files.

# We get around this by making `cat` a meta-operator that merges adjacent cat
# operations into a single `cat_multi`.

defoperator cat_multi => q{sio; weval q{scat $_} for @_};

defmetaoperator cat => q{
  my ($args, $left, $right) = @_;
  my ($f) = @$args;
  $f = $f->() if ref $f eq 'CODE';
  my $i = -1;
  ++$i while $i+1 < @$right && $$right[$i+1][0] eq 'cat';
  ($left, [cat_multi_op($f, $i > -1 ? map $$_[1], @$right[0..$i] : ()),
           @$right[$i+1..$#{$right}]]);
};

docparser multiword => <<'_';
A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
metacharacters within the arguments won't be expanded). If you use this form,
no ARGV entry can end in a closing bracket; otherwise ni will assume you wanted
to close the list.
_

docparser multiword_ws => <<'_';
A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
metacharacters within the arguments won't be expanded). Whitespace is required
around both brackets.
_

docparser shell_command => q{A quoted or bracketed shell command};

docoperator cat  => q{Append contents of a file or resource};
docoperator echo => q{Append text verbatim};
docoperator sh   => q{Filter stream through a shell command};

# Note that we generate numbers internally rather than shelling out to `seq`
# (which is ~20x faster than Perl for the purpose, incidentally). This is
# deliberate: certain versions of `seq` generate floating-point numbers after a
# point, which can cause unexpected results and loss of precision.

defoperator n => q{
  my ($l, $u) = @_;
  sio; for (my $i = $l; $u < 0 || $i < $u; ++$i) {print "$i\n"};
};

docoperator n => q{Append consecutive integers within a range};

defshort '/n',  pmap q{n_op 1, defined $_ ? $_ + 1 : -1}, popt number;
defshort '/n0', pmap q{n_op 0, defined $_ ? $_ : -1}, popt number;

defoperator nmod => q{
  my ($mod) = @_;
  sio;
  for (my $i = 0; ; $i = ($i + 1) % $mod) {print "$i\n"}
};

defshort '/n%', pmap q{nmod_op $_}, integer;

defshort '/i',  pmap q{echo_op $_},     id_text;
defshort '/k',  pmap q{constant_op $_}, id_text;

defshort '/1', pmap q{n_op 1, 2}, pnone;

deflong '/fs', pmap q{cat_op $_}, filename;

docshort '/n' => q{Append integers 1..N, or 1..infinity if N is unspecified};
docshort '/n0' => q{Append integers 0..N-1, or 0..infinity if N is unspecified};
docshort '/i' => q{Identity: append literal text};
docshort '/e' => q{Exec shell command as a filter for the current stream};

docshort '/1' => q{Alias for 'n1'};

doclong '/fs' => q{Append things that appear to be files};

# Stream mixing/forking.
# Append, prepend, divert.

defoperator append => q{my @xs = @_; sio; exec_ni @xs};
docoperator append => q{Append another ni stream to this one};

defoperator prepend => q{
  my @xs = @_;
  close(my $fh = siproc {exec_ni @xs});
  $fh->await;
  sio;
};
docoperator prepend => q{Prepend a ni stream to this one};

defoperator sink_null => q{
  my $s = conf('pipeline/io-size');
  1 while saferead \*STDIN, $_, $s;
};
docoperator sink_null => q{Consume stream and produce nothing};

defoperator divert => q{
  my @xs = @_;
  my $fh = siproc {close STDOUT; exec_ni @xs, sink_null_op};
  stee \*STDIN, $fh, \*STDOUT;
  close $fh;
  $fh->await;
};
docoperator divert => q{Duplicate this stream into a ni pipeline, discarding that pipeline's output};

defshort '/+', pmap q{append_op  @$_}, _qfn;
defshort '/^', pmap q{prepend_op @$_}, _qfn;
defshort '/=', pmap q{divert_op  @$_}, _qfn;

# Interleaving.
# Append/prepend will block one of the two data sources until the other
# completes. Sometimes, though, you want to stream both at once. Interleaving
# makes that possible, and you can optionally specify the mixture ratio, which is
# the number of interleaved rows per input row. (Negative numbers are interpreted
# as reciprocals, so -2 means two stdin rows for every interleaved.)

defoperator interleave => q{
  my ($ratio, $lambda) = @_;
  my $fh = soproc {close STDIN; exec_ni @$lambda};

  if ($ratio) {
    $ratio = 1/-$ratio if $ratio < 0;
    my ($n1, $n2) = (0, 0);
    while (1) {
      ++$n1, defined($_ = <STDIN>) || goto done, print while $n1 <= $n2 * $ratio;
      ++$n2, defined($_ = <$fh>)   || goto done, print while $n1 >= $n2 * $ratio;
    }
  } else {
    my $rmask;
    my ($stdin_ok,  $ni_ok) = (1, 1);
    my ($stdin_buf, $ni_buf);
    while ($stdin_ok || $ni_ok) {
      vec($rmask, fileno STDIN, 1) = $stdin_ok;
      vec($rmask, fileno $fh,   1) = $ni_ok;
      my $n = select my $rout = $rmask, undef, undef, 0.01;
      if (vec $rout, fileno STDIN, 1) {
        $stdin_ok = !!saferead \*STDIN, $stdin_buf, 1048576, length $stdin_buf;
        my $i = 1 + rindex $stdin_buf, "\n";
        if ($i) {
          safewrite \*STDOUT, substr $stdin_buf, 0, $i;
          $stdin_buf = substr $stdin_buf, $i;
        }
      }
      if (vec $rout, fileno $fh, 1) {
        $ni_ok = !!saferead $fh, $ni_buf, 1048576, length $ni_buf;
        my $i = 1 + rindex $ni_buf, "\n";
        if ($i) {
          safewrite \*STDOUT, substr $ni_buf, 0, $i;
          $ni_buf = substr $ni_buf, $i;
        }
      }
    }
  }

  done:
  close $fh;
  $fh->await;
};

defshort '/%', pmap q{interleave_op @$_}, pseq popt number, _qfn;

# Sinking.
# We can sink data into a file just as easily as we can read from it. This is
# done with the `>` operator, which is typically written as `\>`. The difference
# between this and the shell's > operator is that \> outputs the filename; this
# lets you invert the operation with the nullary \< operator.

defoperator file_read => q{chomp, weval q{scat $_} while <STDIN>};
defoperator file_read_and_nuke => q{
  while (<STDIN>)
  {
    chomp;
    weval q{scat $_};
    resource_nuke($_);
  }
};

defshort '/<#' => pmap q{
  requires_dangermode('file_read_and_nuke (written \\<#)');
  file_read_and_nuke_op;
}, pnone;

defoperator file_write => q{
  my $file = filename_write(shift);
  my $fh = swfile $file;
  sforward \*STDIN, $fh;
  close $fh;
  $fh->await if $fh->can('await');
  print "$file\n";
};

defshort '/>', pmap q{file_write_op $_}, popt nefilename;
defshort '/<', pmap q{file_read_op},     pnone;

defoperator pipe_write => q{
  use POSIX qw/mkfifo/;
  my ($fname) = @_;
  $fname = substr resource_tmp('pipe://'), 7 unless defined $fname;
  mkfifo $fname, 0700 or die "ni pipe_write: mkfifo($fname) failed: $!";

  $|++;
  print "pipe://$fname\n";
  POSIX::close(fileno STDOUT);

  unless (cfork)
  {
    open my $fh, '>', $fname or die "ni pipe_write: open(>$fname) failed: $!";
    sforward \*STDIN, $fh;
    close $fh;
    unlink $fname if -p $fname;
  }
};

defshort '/|', pmap q{pipe_write_op $_}, popt nefilename;

defoperator file_prepend_name_read => q{
  my ($colspec, $transform) = @_;
  $colspec   = [1, 0] unless defined $colspec;
  $transform = defined $transform ? eval "sub {local \$_ = shift; $transform}"
                                  : sub {shift};

  my ($maxcol, $coli) = @$colspec;
  my $file;
  while (defined($file = <STDIN>))
  {
    chomp $file;
    $_     = &$transform((split /\t/, $file, $maxcol)[$coli]);
    my $fh = soproc {weval q{scat $_}};
    chomp, print "$file\t$_\n" while <$fh>;
    close $fh;
    $fh->await;
  }
};

defshort '/W<', pmap q{file_prepend_name_read_op @$_},
                pseq popt colspec1, popt generic_code;

defoperator file_prepend_name_number_read => q{
  my ($colspec, $transform) = @_;
  $colspec   = [1, 0] unless defined $colspec;
  $transform = defined $transform ? eval "sub {local \$_ = shift; $transform}"
                                  : sub {shift};

  my ($maxcol, $coli) = @$colspec;
  my $file;
  while (defined($file = <STDIN>))
  {
    chomp $file;
    $_       = &$transform((split /\t/, $file, $maxcol)[$coli]);
    my $fh   = soproc {weval q{scat $_}};
    my $line = 0;
    ++$line, chomp, print "$file\t$line\t$_\n" while <$fh>;
    close $fh;
    $fh->await;
  }
};

defshort '/Wn<', pmap q{file_prepend_name_number_read_op @$_},
                 pseq popt colspec1, popt generic_code;

defoperator file_prepend_name_write => q{
  my ($lambda) = @_;
  my $file     = undef;
  my $fh       = undef;

  $lambda = undef if ref $lambda && !@$lambda;

  while (<STDIN>)
  {
    my ($fname, $l) = /^([^\t\n]+)\t([\s\S]*)/;
    ($fname, $l) = ($file, "\n") unless defined $fname;
    if (!defined $file or $fname ne $file)
    {
      close $fh, $fh->can('await') && $fh->await if defined $fh;
      print "$file\n" if defined $file;
      $file = $fname;

      # NB: swfile has much lower startup overhead than exec_ni(), so use that
      # unless we have a lambda that requires slower operation.
      $fh = defined $lambda
        ? siproc {exec_ni(@$lambda, file_write_op($file), sink_null_op)}
        : swfile $file;
    }
    print $fh $l;
  }

  close $fh, $fh->can('await') && $fh->await if defined $fh;
  print "$file\n" if defined $file;
};

defshort '/W>', pmap q{file_prepend_name_write_op $_}, popt _qfn;

# Random-access sharding.
# Similar to prepended-name write, but splits data into a finite number of
# shards deterministically, i.e. we keep filehandles open.
#
# This may fail with "too many open files" if you have too many shards. If you
# have that problem, you can nest sharding operators by specifying intermediate
# partitions.

defoperator sharded_write => q{
  my ($lambda) = @_;
  my %fhs;

  while (<STDIN>)
  {
    my $i    = index $_, "\t";
    next if $i == -1;
    my $file = substr $_, 0, $i;
    my $fh   = $fhs{$file} ||= defined $lambda
      ? siproc {exec_ni(@$lambda, file_write_op $file)}
      : swfile $file;
    print $fh substr $_, $i + 1;
  }

  close $_ for values %fhs;
  $_->can('await') && $_->await for values %fhs;
};

defshort '/S>', pmap q{sharded_write_op $_}, popt _qfn;

# Deterministic stream multiply.
# Just like S\>, but relies on the streams to write things. It's highly
# recommended that you drop to files and then combine to avoid data corruption;
# e.g. ni ... \*[... z\>] \<z\>combined. (This operator doesn't line-merge like
# horizontal scaling.)

defoperator sharded_fork => q{
  my ($lambda) = @_;
  my %fhs;
  while (<STDIN>)
  {
    my $i     = index $_, "\t";
    next if $i == -1;
    my $shard = substr $_, 0, $i;
    my $fh    = $fhs{$shard} ||= siproc {exec_ni(@$lambda)};
    print $fh substr $_, $i + 1;
  }
};

defshort '/*', pmap q{sharded_fork_op $_}, _qfn;

# Resource stream encoding.
# This makes it possible to serialize a directory structure into a single stream.
# ni uses this format internally to store its k/v state.

defoperator encode_resource_stream => q{
  my @xs;
  while (<STDIN>) {
    chomp;
    my $s = rfc $_;
    my $line_count = @xs = split /\n/, "$s ";
    print "$line_count $_\n", $s, "\n";
  }
};

defshort '/>\'R', pmap q{encode_resource_stream_op}, pnone;

# Compression and decoding.
# Sometimes you want to emit compressed data, which you can do with the `Z`
# operator. It defaults to gzip, but you can also specify xz, lzo, lz4, zstd, or
# bzip2 by adding a suffix. You can decode a stream in any of these formats
# using `zd` (though in most cases ni will automatically decode compressed
# formats).

our %compressors = qw/
  g gzip
  z zstd
  x xz
  o lzop
  4 lz4
  b bzip2 /;

# Detect parallel compressors: pigz for gzip, and pbzip2 for bzip2. Each of
# these is much faster than its serial variant if multiple processors are
# available.
$compressors{g} = 'pigz'   unless system "which pigz   > /dev/null 2>&1";
$compressors{b} = 'pbzip2' unless system "which pbzip2 > /dev/null 2>&1";

BEGIN {defparseralias compressor_name => prx '[gzxo4b]'}
BEGIN {
  defparseralias compressor_spec =>
    pmap q{my ($c, $level) = @$_;
           $c = $ni::compressors{$c || 'g'};
           defined $level ? sh_op "$c -$level" : sh_op $c},
    pseq popt compressor_name, popt integer;
}

defoperator decode => q{sdecode};

defshort '/z',  compressor_spec;
defshort '/zn', pk sink_null_op();
defshort '/zd', pk decode_op();
