NI_MODULE R

use File::Temp qw/tmpnam/;

our $r_device_init   = $ENV{NI_R_DEVICE}      // 'pdf("FILE")';
our $display_program = $ENV{NI_IMAGE_DISPLAY} // 'xdg-open';

our %r_shorthands = (
  '%i' => 'data <- read.table(file("stdin"), sep="\t"); ',
);

sub expand_r_shorthands {
  my ($s) = @_;
  $s =~ s/$_/$r_shorthands{$_}/g for keys %r_shorthands;
  $s;
}

sub r_reader_io {
  my ($r_eval_code) = @_;
  $r_eval_code = expand_r_shorthands $r_eval_code;
  $r_eval_code = "write.table((function () {$r_eval_code})(), '', "
               . "quote=FALSE, sep='\\t', col.names=NA)";
  ni_process shell_quote('R', '--slave', '-e', $r_eval_code),
             undef,
             undef;
}

sub r_writer_io {
  my ($r_eval_code, $no_automatic_import) = @_;
  my $tempimage = tmpnam . '.pdf';

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

defdata 'R', sub { $_[0] =~ s/^R:// },
  sub {
    my ($r_code) = @_;
    ni_file "[R $r_code]", sub { r_reader_io($r_code)->reader_fh },
                           sub { r_writer_io($r_code)->writer_fh };
  };

NI_MODULE_END
