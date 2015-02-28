NI_MODULE gnuplot

use POSIX qw/setsid/;

our %gnuplot_shorthands = (
  '%l' => ' with lines',
  '%d' => ' with dots',
  '%i' => ' with impulses',
  '%u' => ' using ',
  '%t' => ' title ',
);

sub expand_gnuplot_shorthands {
  my ($s) = @_;
  $s =~ s/$_/$gnuplot_shorthands{$_}/g for keys %gnuplot_shorthands;
  $s;
}

sub gnuplot_writer_io {
  my ($script, @options) = @_;
  $script = expand_gnuplot_shorthands $script;

  my $into = ni_filename;
  ni_file "[gnuplot @options]",
          undef,
          sub {
            my $in = ni_pipe;
            unless (fork) {
              $in->close_writer;
              $in > $into;
              system 'gnuplot', '-persist', '-e',
                     $script =~ s/DATA/"$into"/gr, @options;

              # Ugh, egregious double-fork to clean up tempfile later on. No
              # idea how to get gnuplot to block properly and then exit when
              # the user closes the plot window.
              setsid;
              close STDIN;
              close STDOUT;
              unless (fork) {
                sleep 3600;
                unlink "$into";
              }
              exit;
            }
            $in->close_reader;
            $in->writer_fh;
          };
}

defdata 'gnuplot', sub { $_[0] =~ s/^gnuplot:// },
  sub {
    my ($stuff) = @_;
    $stuff = "plot DATA $stuff" unless $stuff =~ /DATA/;
    gnuplot_writer_io $stuff;
  };

NI_MODULE_END
