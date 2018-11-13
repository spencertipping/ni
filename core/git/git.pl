# Git interop
# Allows you to use git repositories as data sources for ni

sub git_dir($) { -d "$_[0]/.git" ? "$_[0]/.git" : $_[0] }

sub git_parse_pathref($)
{
  my ($path, $ref, $extra) = shift =~ /^(.*[^:]):([^:]+)(?:::(.*))?$/;
  $path = git_dir $path;
  (my $outpath = $path) =~ s/\/.git$//;
  ($outpath, $path, $ref, $extra);
}

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
                 qw/ gitcommitmeta githistory gitdiff gittree gitpdiff
                     gitnmhistory gitsnap gitdelta /), "\n";
    };
  };

defresource 'gitcommitmeta',
  read => q{
    my (undef, $path, $ref, undef) = git_parse_pathref $_[1];
    soproc {sh shell_quote
      git => "--git-dir=$path", "cat-file", "commit", $ref};
  };

defresource 'githistory',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    soproc {sh shell_quote
      git => "--git-dir=$path", "log",
             "--format=gitcommit://$outpath:%H\t%an:%ae\t%at\t%s", $ref,
             (defined $file ? ("--", $file) : ())};
  };

defresource 'gitnmhistory',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    soproc {sh shell_quote
      git => "--git-dir=$path", "log", "--no-merges",
             "--format=gitcommit://$outpath:%H\t%ae\t%at\t%s", $ref,
             (defined $file ? ("--", $file) : ())};
  };

defresource 'gitdiff',
  read => q{
    my (undef, $path, $refs, $file) = git_parse_pathref $_[1];
    my @refs = split /\.\./, $refs, 2;
    if (@refs < 2)
    {
      my $parent_cmd = shell_quote git => "--git-dir=$path",
                         "show", "--format=%P", "-s", $refs[0];
      my @parents = grep length, split(/\s+/, `$parent_cmd`),
                                 '4b825dc642cb6eb9a060e54bf8d69288fbee4904';
      unshift @refs, $parents[0];
    }
    soproc {sh shell_quote git => "--git-dir=$path", "diff", @refs,
                                  (defined $file ? ("--", $file) : ())};
  };

defresource 'gitpdiff',
  read => q{
    my (undef, $path, $refs, $file) = git_parse_pathref $_[1];
    my @refs = split /\.\./, $refs, 2;
    if (@refs < 2)
    {
      my $parent_cmd = shell_quote git => "--git-dir=$path",
                         "show", "--format=%P", "-s", $refs[0];
      my @parents = grep length, split(/\s+/, `$parent_cmd`),
                                 '4b825dc642cb6eb9a060e54bf8d69288fbee4904';
      unshift @refs, $parents[0];
    }
    soproc
    {
      my $gd = soproc {sh
        shell_quote git => "--git-dir=$path", "diff", "-U0", @refs,
                           (defined $file ? ("--", $file) : ())};
      my ($file, $lline, $rline) = (undef, 0, 0);
      while (<$gd>)
      {
        next unless /^index/;
        $file = $1 if <$gd> =~ /^--- a\/(.*)/;
        $file = $1 if <$gd> =~ /^\+\+\+ b\/(.*)/;
        last unless defined($_ = <$gd>);
        while (/^\@\@ -(\d+)(?:,\d+)? \+(\d+)/)
        {
          ($lline, $rline) = ($1, $2);
          while (defined($_ = <$gd>) && /^([-+])/)
          {
            print join"\t", $file, "$lline:$rline:$1", substr $_, 1;
            $1 eq "-" ? ++$lline : ++$rline;
          }
        }
      }
    };
  };

# Tree/blob objects: behave just like directories/files normally
defresource 'gittree',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    my $enum_command = shell_quote
      git => "--git-dir=$path", 'ls-tree', $ref,
             defined $file ? ('--', $file) : ();
    soproc {
      for (`$enum_command`)
      {
        chomp(my ($mode, $type, $id, $name) = split /\h/, $_, 4);
        print $type ne 'commit'
          ? "git$type://$outpath:$id\t$mode\t$name\n"
          : "git$type://$outpath/$name:$id\t$mode\t$name\n";
      }
    };
  };

defresource 'gitblob',
  read => q{
    my (undef, $path, $ref, $file) = git_parse_pathref $_[1];
    soproc {
      defined $file
        ? sh shell_quote git => "--git-dir=$path", 'show', "$ref:$file"
        : sh shell_quote git => "--git-dir=$path", 'cat-file', 'blob', $ref};
  };

# gitsnap: a full listing of all gitblob:// entries that make up a commit at a
# given ref. This is a snapshot of the repository at some moment in time. You
# could then use W\< to read the repo contents.
#
# Blobs are specified using logical paths; so you'll see
# gitblob://path:ref::filename instead of gitblob://path:blobhash.

defresource 'gitsnap',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'ls-tree',
                                   '-r', '--name-only',
                                   $ref, defined $file ? ('--', $file) : ();
    soproc { print "gitblob://$outpath:$ref\::$_" for `$enum_command` };
  };

# gitdsnap: just like gitsnap, but returns direct object IDs instead of
# gitblob://repo:ref::path. Because object IDs track content, this gives you a
# simple way to know whether an object has been changed.
#
# gitdsnap:// has two columns of output: the object IDs and the logical
# filenames they map to.

defresource 'gitdsnap',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'ls-tree',
                                   '-r', $ref,
                                   defined $file ? ('--', $file) : ();
    soproc { /^\S+ \S+ ([0-9a-fA-F]+)\t(.*)/
             && print "gitblob://$outpath:$1\t$2\n" for `$enum_command` };
  };

# gitdelta: a listing of all files changed by a specific revision, listed as
# gitpdiff:// URIs with path components.

defresource 'gitdelta',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'diff-tree',
                                   '-r', '--no-commit-id', '--name-only',
                                   $ref, defined $file ? ('--', $file) : ();
    soproc { print "gitpdiff://$outpath:$ref\::$_" for `$enum_command` };
  };

# gitddelta: the gitdsnap:// version of gitdelta: object IDs with a path
# afterwards. These are gitpdiff:// URIs that include both object IDs; i.e.
# gitpdiff://<repo>:<objid1>..<objid2>.

defresource 'gitddelta',
  read => q{
    my ($outpath, $path, $ref, $file) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'diff-tree',
                                   '-r', '--no-commit-id',
                                   $ref, defined $file ? ('--', $file) : ();
    soproc { /^\S+\s\S+\s(\S+)\s(\S+)\s\S+\s(.*)/
             && print "gitpdiff://$outpath:$1..$2\t$3\n" for `$enum_command` };
  };
