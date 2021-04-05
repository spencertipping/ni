# Git interop
# Allows you to use git repositories as data sources for ni

use Cwd qw/abs_path/;

sub is_git_dir
{
  my $d = shift;
  return "$d/.git" if -d "$d/.git";
  return "$d/$1"   if -f "$d/.git" && rf("$d/.git") =~ /gitdir: (.*)/;
  return $d if -f "$d/HEAD" && -f "$d/config"
            && -d "$d/objects" && -d "$d/refs";
  undef;
}

sub git_dir_parents
{
  my ($sub, $abs) = @_;
  my $gitdir = is_git_dir $abs;
  return ($sub, $gitdir) if defined $gitdir;
  die "ni git_dir: no git directory in parent chain" if $abs =~ /^\/?$/;
  $abs =~ s/\/([^\/]+)$//;
  $sub = "$1/$sub";
  git_dir_parents($sub, $abs);
}

# Returns (subdir, gitdir), where subdir is undef if the gitdir directly
# contains .git. If we're operating from a relative-path subdir, then use that
# subdir as the implied path for git operations if one wasn't specified
# manually.
sub git_dir($)
{
  my $d = shift;
  my $gitdir = is_git_dir $d;
  defined $gitdir ? (undef, $gitdir) : git_dir_parents "", abs_path $d;
}

# Returns ($logical_root, $gitdir, $ref, $path//undef, $subdir//undef).
# $path is if you use :: to explicitly specify a sub-path.
# $subdir is similar, but it's inferred from the git:// URI base. It's mostly of
# use when you say something like git://.:master from a subdirectory of the repo
# root; your intent is probably to refer to that subdirectory (although not
# always; gitblob:// is an exception).
sub git_parse_pathref($)
{
  my ($path, $ref, $extra) = $_[0] =~ /^([^\t:]*)(?::([^\t:]+))?(?:::(.*)$)?/;
  my ($gitsub, $gitpath) = git_dir $path;
  $ref = git_infer_ref($gitpath) unless defined $ref;
  (my $outpath = $gitpath) =~ s/\/.git$//;
  ($outpath, $gitpath, $ref, $extra, $gitsub);
}

sub git_dashed_path($$)
{
  my ($file, $subdir) = @_;

  # Always interpret $file as being relative to $subdir, unless it begins with
  # "/".
  return ("--", $file) if $file =~ s/^\///;
  return ("--", "$subdir/$file") if defined $subdir and defined $file;
  return ("--", $file)   if defined $file;
  return ("--", $subdir) if defined $subdir;
  return ();
}

sub git_infer_ref($)
{
  my $git_dir = shift;
  die "ni git_infer_ref: no ref can be inferred from headless repo (or HEAD"
    . " is unreadable). You'll need to specify the ref manually, e.g."
    . " gittree://repopath:master instead of just gittree://repopath."
    unless -r "$git_dir/HEAD";
  my ($ref) = rfc("$git_dir/HEAD") =~ /(\S+)$/;
  $ref;
}


# Auxiliary operators:
# cat blob-ids | ni git\</path/to/repo == cat blob-ids | git cat-file --batch
# cat blob-ids | ni gitm\</path/to/repo == ... | git cat-file --batch-check

defoperator git_cat_objects =>
q{
  my $batch_option = shift;
  my (undef, $gitdir)  = git_dir dor shift, ".";
  my $gitproc = siproc {
    sh shell_quote git => "--git-dir=$gitdir", "cat-file", $batch_option};

  while (<STDIN>)
  {
    chomp;
    my ($id) = /^[^\t]*([0-9a-fA-F]{40})(?:\t|$)/
      or die "git<: line $_ does not contain a git object ID";
    print $gitproc "$id\n";
  }
};

defshort '/git<',  pmap q{git_cat_objects_op "--batch", $_}, popt pc filename;
defshort '/gitm<', pmap q{git_cat_objects_op "--batch-check", $_}, popt pc filename;


# Main entry point: repo -> branches/tags
# git:///path/to/repo
defresource 'git',
  read => q{
    my (undef, $path) = git_dir $_[1];
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
                     gitnmhistory gitsnap gitdelta gitclosure /), "\n";
    };
  };

defresource 'gitcommitmeta',
  read => q{
    my (undef, $path, $ref) = git_parse_pathref $_[1];
    soproc {sh shell_quote
      git => "--git-dir=$path", "cat-file", "commit", $ref};
  };

defresource 'githistory',
  read => q{
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    soproc {sh shell_quote
      git => "--git-dir=$path", "log",
             "--format=gitcommit://$outpath:%H\t%an:%ae\t%at\t%s", $ref,
             git_dashed_path $file, $subdir};
  };

defresource 'gitnmhistory',
  read => q{
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    soproc {sh shell_quote
      git => "--git-dir=$path", "log", "--no-merges",
             "--format=gitcommit://$outpath:%H\t%ae\t%at\t%s", $ref,
             git_dashed_path $file, $subdir};
  };

defresource 'gitdiff',
  read => q{
    my (undef, $path, $refs, $file, $subdir) = git_parse_pathref $_[1];
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
                                  git_dashed_path $file, $subdir};
  };

defresource 'gitpdiff',
  read => q{
    my (undef, $path, $refs, $file, $subdir) = git_parse_pathref $_[1];
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
                           git_dashed_path $file, $subdir};
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
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    my $enum_command = shell_quote
      git => "--git-dir=$path", 'ls-tree', $ref,
             git_dashed_path $file, $subdir;
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
    my (undef, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    $file = "$subdir/$file" if defined $subdir and defined $file;
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
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'ls-tree',
                                   '-r', $ref, git_dashed_path $file, $subdir;
    soproc { /^\d+ blob \S+\t(.*)/ and print "gitblob://$outpath:$ref\::$1\n"
             for `$enum_command` };
  };

# gitdsnap: just like gitsnap, but returns direct object IDs instead of
# gitblob://repo:ref::path. Because object IDs track content, this gives you a
# simple way to know whether an object has been changed.
#
# gitdsnap:// has two columns of output: the object IDs and the logical
# filenames they map to.

defresource 'gitdsnap',
  read => q{
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'ls-tree',
                                   '-r', $ref,
                                   git_dashed_path $file, $subdir;
    soproc { /^\S+ blob ([0-9a-fA-F]+)\t(.*)/
             && print "gitblob://$outpath:$1\t$2\n" for `$enum_command` };
  };

# gitdelta: a listing of all files changed by a specific revision, listed as
# gitpdiff:// URIs with path components.

defresource 'gitdelta',
  read => q{
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'diff-tree',
                                   '-r', '--no-commit-id', '--name-only',
                                   $ref, git_dashed_path $file, $subdir;
    soproc { print "gitpdiff://$outpath:$ref\::$_" for `$enum_command` };
  };

# gitddelta: the gitdsnap:// version of gitdelta: object IDs with a path
# afterwards. These are gitpdiff:// URIs that include both object IDs; i.e.
# gitpdiff://<repo>:<objid1>..<objid2>.

defresource 'gitddelta',
  read => q{
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    my $enum_command = shell_quote git => "--git-dir=$path", 'diff-tree',
                                   '-r', '--no-commit-id',
                                   $ref, git_dashed_path $file, $subdir;
    soproc { /^\S+\s\S+\s(\S+)\s(\S+)\s\S+\s(.*)/
             && print "gitpdiff://$outpath:$1..$2\t$3\n" for `$enum_command` };
  };

# gitclosure://<repo>:<ref>[::<path>]: returns the full set of direct-encoded
# gitcommit://, gittree://, and gitblob:// URLs that constitute the ref.

defresource 'gitclosure',
  read => q{
    my ($outpath, $path, $ref, $file, $subdir) = git_parse_pathref $_[1];
    my $revlist_cmd = shell_quote git => "--git-dir=$path", 'rev-list',
                                  '--objects', $ref,
                                  git_dashed_path $file, $subdir;
    my %object_paths;
    my @object_order;
    my $revlist_fh = soproc { sh $revlist_cmd };
    /^(\S+)(?:\s+(.*))?/ and push(@object_order, $1)
                         and $object_paths{$1} = dor $2, "" while <$revlist_fh>;
    close $revlist_fh;
    $revlist_fh->await;

    my $batchcheck = shell_quote git => "--git-dir=$path", 'cat-file',
                                 '--batch-check';
    soproc {
      my $fh = soproc { my $pipe = siproc { sh $batchcheck };
                        print $pipe "$_\n" for @object_order };
      /^(\S+)\s+(\w+)/ && print "git$2://$outpath:$1\t$object_paths{$1}\n"
        while <$fh> };
  };
