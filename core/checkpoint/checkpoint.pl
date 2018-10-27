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

sub checkpoint_needs_regen($$)
{
  my ($f, $deps) = @_;
  return 0 unless defined $deps;
  my $f_mtime = (stat $f)[9];
  grep -e && (stat)[9] > $f_mtime, @$deps;
}

defoperator checkpoint => q{
  my ($file, $deps, $generator) = @_;
  sio;
  checkpoint_create $file, $generator
    if ! -r $file || checkpoint_needs_regen($file, $deps);
  scat $file;
};

defmetaoperator inline_checkpoint => q{
  my ($args, $left, $right) = @_;
  my ($file, $deps) = @$args;
  ([], [checkpoint_op($file, $deps, $left), @$right]);
};

defoperator identity => q{sio};

BEGIN
{
  defparseralias nefilelist => palt super_brackets, multiword_ws, multiword;
}

defshort '/:', pmap q{$_ ? inline_checkpoint_op @$_
                         : identity_op},
               popt pseq pc nefilename, popt nefilelist;
