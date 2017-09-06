# Checkpoint files.
# You can break a long pipeline into a series of smaller files using
# checkpointing, whose operator is `:`. The idea is to cache intermediate
# results. A checkpoint specifies a file.

# Checkpoints are fully buffered before emitting output.

sub checkpoint_create($$) {
  my $name = "/";
  $name = "$_[0].part." . noise_str 8 while -e $name;
  my $fh = swfile $name;
  sforward sni(@{$_[1]}), $fh;
  rename $name, $_[0];
}

defoperator checkpoint => q{
  my ($file, $generator) = @_;
  sio;
  checkpoint_create $file, $generator unless -r $file;
  scat $file;
};

defmetaoperator inline_checkpoint => q{
  my ($args, $left, $right) = @_;
  my ($file) = @$args;
  ([], [checkpoint_op($file, $left), @$right]);
};

defshort '/:', pmap q{inline_checkpoint_op $_}, pc nefilename;
