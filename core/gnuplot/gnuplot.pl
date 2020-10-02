# Gnuplot interop.
# An operator that sends output to a gnuplot process.

use Scalar::Util qw/looks_like_number/;

BEGIN {defdsp gnuplot_code_prefixalt => 'prefixes for gnuplot code';
       defparseralias gnuplot_colspec => palt colspec1, pmap q{undef}, pstr ':'}
BEGIN {defparseralias gnuplot_code =>
         pmap q{join "", map ref($_) ? @$_ : $_, @$_},
              pseq prep('dsp/gnuplot_code_prefixalt'),
                   popt generic_code}

defoperator stream_to_gnuplot => q{
  my ($col, $command) = @_;
  exec 'gnuplot', '-e', $command unless defined $col;
  my ($k, $fh) = (undef, undef);
  while (<STDIN>) {
    chomp;
    my @fs = split /\t/, $_, $col + 2;
    my $rk = join "\t", @fs[0..$col];
    if (!defined $k or $k ne $rk) {
      if (defined $fh) {
        close $fh;
        $fh->await;
      }
      $k  = $rk;
      $fh = siproc {exec 'gnuplot', '-e', "KEY='$k';$command"};
    }
    print $fh join("\t", @fs[$col+1..$#fs]) . "\n";
  }
};

defshort '/G', pmap q{stream_to_gnuplot_op @$_},
               pseq gnuplot_colspec, gnuplot_code;

# Some convenient shorthands for gnuplot -- things like interactive plotting,
# setting up JPEG export, etc.

BEGIN {defparseralias gnuplot_terminal_size =>
         pmap q{defined $_ ? "size " . join ',', @$_ : ""},
         popt pn [0, 2], integer, prx('[x,]'), integer}

defgnuplot_code_prefixalt J  => pmap q{"set terminal jpeg $_;"}, gnuplot_terminal_size;
defgnuplot_code_prefixalt PC => pmap q{"set terminal pngcairo $_;"}, gnuplot_terminal_size;
defgnuplot_code_prefixalt P  => pmap q{"set terminal png $_;"}, gnuplot_terminal_size;

defgnuplot_code_prefixalt X => pmap q{"set terminal x11 persist;"}, popt pstr 'P';
defgnuplot_code_prefixalt Q => pmap q{"set terminal qt persist;"}, popt pstr 'P';
defgnuplot_code_prefixalt W => pmap q{"set terminal wx persist;"}, popt pstr 'P';

defgnuplot_code_prefixalt '%l' => pk 'plot "-" with lines ';
defgnuplot_code_prefixalt '%d' => pk 'plot "-" with dots ';
defgnuplot_code_prefixalt '%i' => pk 'plot "-" with impulses ';
defgnuplot_code_prefixalt '%v' => pk 'plot "-" with vectors ';

defgnuplot_code_prefixalt '%t' => pmap q{"title '$_'"}, generic_code;
defgnuplot_code_prefixalt '%u' => pmap q{"using $_"},   generic_code;

# FFMPEG movie assembly.
# You can use the companion operator `GF` to take a stream of jpeg images from a
# partitioned gnuplot process and assemble a movie. `GF` accepts shell arguments
# for ffmpeg to follow `-f image2pipe -i -`.
#
# GF^ inverts GF, outputting PNG images from a video specified on stdin.
#
# NOTE: these options are deprecated in favor of IV (images-to-video) and VI
# (video-to-images).

defshort '/GF',  pmap q{sh_op "ffmpeg -f image2pipe -i - $_"}, popt shell_command;
defshort '/GF^', pmap q{sh_op "ffmpeg -i - $_ -f image2pipe -c:v png -"},
                      popt shell_command;

# Auto-multiplotting
# If you have a TSV containing multiple columns, we can put them all into a
# single plot.
#
# Getting the data into gnuplot isn't quite trivial. We need to transpose the
# stream column-wise, which involves buffering everything up front.

defoperator gnuplot_all =>
q{
  my $code_prefix = shift || "set terminal wx persist";
  my @col_vectors;
  my @col_titles;

  chomp(my $l = <STDIN>);
  my @fs = split /\t/, $l;
  if (grep !looks_like_number($_), @fs)
  {
    # First line is a header
    @col_titles = @fs[1..$#fs];
  }
  else
  {
    # First line is data
    @col_titles  = ("A".."Z")[1..$#fs];
    @col_vectors = map [$_], @fs;
  }

  while (<STDIN>)
  {
    chomp;
    my @fs = split /\t/;
    push @{$col_vectors[$_] ||= []}, 0+$fs[$_] for 0..$#fs;
  }

  # NB: col A is implicitly the shared X coordinate
  my $code = $code_prefix
    . ";plot "
    . join",", map "\"-\" with lines title \"$_\"", @col_titles;

  my $xs = shift @col_vectors;
  my $fh = siproc {exec 'gnuplot', '-e', $code};
  for my $v (@col_vectors)
  {
    print $fh "$$xs[$_]\t$$v[$_]\n" for 0..$#$v;
    print $fh "e\n";
  }
  close $fh;
  $fh->await;
};

# NB: using G* instead of G% because it has better keyboard ergonomics on QWERTY
defshort '/G*', pmap q{gnuplot_all_op $_}, gnuplot_code;
