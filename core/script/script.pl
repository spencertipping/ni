# Scripting support.
# This lets you define a library of scripts or other random utilities (possibly
# with dependent files) and gives you a way to invoke those scripts from a custom
# operator. See doc/script.md for details about how this works.

sub export_lib_to_path {
  local $_;
  my ($lib, $path) = @_;
  $path ||= uri_path resource_tmp 'file://';
  mkdir $path or die "ni: could not create $path/ for library export: $!";
  my @l     = lib_entries $lib, $ni::self{"$lib/lib"};
  my @files = map sr($_, qr|^\Q$lib/\E|, "$path/"), @l;
  wf "$path/ni", image;
  wf $files[$_], $ni::self{$l[$_]} for 0..$#l;
  chmod 0755, "$path/ni", @files;
  $path;
}

sub rm_rf($) {
  local $SIG{CHLD} = 'DEFAULT';
  system 'rm', '-rf', $_[0];
}

defoperator script => q{
  my ($lib, $cmd) = @_;
  my $tmpdir = export_lib_to_path $lib;
  my $runner = siproc {
    chdir $tmpdir;
    sh $cmd;
  };
  sforward \*STDIN, $runner;
  close $runner;
  $runner->await;
  rm_rf $tmpdir;
};
