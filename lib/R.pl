NI_MODULE R

use File::Temp qw/tmpnam/;

our $r_device_init   = $ENV{NI_R_DEVICE}      // 'png("FILE")';
our $display_program = $ENV{NI_IMAGE_DISPLAY} // 'display';

our %r_shorthands = (
  # TODO
);

sub expand_r_shorthands {
  my ($s) = @_;
  $s =~ s/$_/$r_shorthands{$_}/g for keys %r_shorthands;
  $s;
}

sub r_writer_io {
  my ($r_eval_code, $no_automatic_import) = @_;
  my $tempimage = tmpnam . '.png';

  $r_eval_code = expand_r_shorthands $r_eval_code;

  # Set the visual device to the PNG prior to any user code.
  $r_eval_code = "$r_device_init; $r_eval_code" =~ s/FILE/$tempimage/gr;

  # Import a TSV
  $r_eval_code = 'data <- read.table(file("stdin"), sep="\t"); ' . $r_eval_code
    unless $no_automatic_import;
  ni_process(shell_quote('R', '--slave', '-e', $r_eval_code)
             . " && $display_program $tempimage"
             . " && rm $tempimage",
             undef,
             \*STDERR);
}

defdata 'R', sub { $_[0] =~ s/^R:// }, \&r_writer_io;

NI_MODULE_END
