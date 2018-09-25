# Git interop
# Allows you to use git repositories as data sources for ni

sub git_dir($) { -d "$_[0]/.git" ? "$_[0]/.git" : $_[0] }

# Main entry point: repo -> branches/tags
# git:///path/to/repo
defresource 'git',
  read => q{
    my $path = git_dir $_[1];
    (my $outpath = $path) =~ s/\/\.git$//;
    soproc {
      my $format = "--format=gitcommit://$outpath:%(refname)\t%(objectname)";
      sh shell_quote(git => "--git-dir=$path", "for-each-ref", $format)};
  };

# Commits: emit options
defresource 'gitcommit',
  read => q{
    my $pathref = $_[1];
    soproc {
      print join("\t", map "$_://$pathref",
                 qw/ gitcommitmeta githistory gitdiff gittree /), "\n";
    };
  };

defresource 'gitcommitmeta',
  read => q{
    my ($path, $ref) = $_[1] =~ /(.*):([^:]+)$/;
    $path = git_dir $path;
    soproc {sh shell_quote
      git => "--git-dir=$path", "cat-file", "commit", $ref};
  };

defresource 'githistory',
  read => q{
    my ($path, $ref) = $_[1] =~ /(.*):([^:]+)$/;
    $path = git_dir $path;
    (my $outpath = $path) =~ s/\/\.git$//;
    soproc {sh shell_quote
      git => "--git-dir=$path", "log",
             "--format=gitcommit://$outpath:%H\t%an\t%at\t%s", $ref};
  };

defresource 'gitdiff',
  read => q{
    my ($path, $refs) = $_[1] =~ /(.*):([^:]+)$/;
    my @refs          = split /\.\./, $refs, 2;
    unshift @refs, "$refs[0]^" if @refs < 2;
    $path = git_dir $path;
    soproc {sh shell_quote git => "--git-dir=$path", "diff", @refs};
  };

# Tree/blob objects: behave just like directories/files normally
defresource 'gittree',
  read => q{
    my ($path, $ref) = $_[1] =~ /(.*):([^:]+)$/;
    $path = git_dir $path;
    (my $outpath = $path) =~ s/\/\.git$//;
    soproc {
      for (`git --git-dir='$path' ls-tree '$ref'`)
      {
        chomp(my ($mode, $type, $id, $name) = split /\h/, $_, 4);
        print "git$type://$outpath:$id\t$mode\t$name\n";
      }
    };
  };

defresource 'gitblob',
  read => q{
    my ($path, $ref) = $_[1] =~ /(.*):([^:]+)$/;
    $path = git_dir $path;
    (my $outpath = $path) =~ s/\/\.git$//;
    soproc {sh shell_quote git => "--git-dir=$path", 'cat-file', 'blob', $ref};
  };
