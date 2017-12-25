# Hadoop Map (and Reduce!) Side Joins
# This is a port of the old logic from nfu with
# some nice improvements. hdfsj takes a stream and a folder,
# and using the hadoop context clues, identifies the file
# in that folder that should match the stream.
# Joins can be done on both the map _and_ on the reduce side, and
# they're blazing fast; they should be used liberally.

sub hadoop_partfile_n { $_[0] =~ /[^0-9]([0-9]+)(?:\.[^\/]+)?$/ ? $1 : 0 }
sub hadoop_partsort {
  sort {hadoop_partfile_n($a) <=> hadoop_partfile_n($b)} @_
}
sub hadoop_ls {
  # Now get the output file listing. This is a huge mess because Hadoop is a
  # huge mess.
  local $SIG{CHLD} = "DEFAULT";
  my $ls_command = shell_quote conf 'hadoop/name', 'fs', '-ls', @_;
  grep /\/[^_][^\/]*$/, map +(split " ", $_, 8)[7],
                        grep !/^Found/,
                        split /\n/, ''.qx/$ls_command/;
}

sub stem_partfile_path($$) {
  my ($fn, $shift_amt) = @_;  
  my ($part, $idx) = ($fn =~ /^(part[^\d]+)(\d+)/);
  $part . substr($idx, $shift_amt) . "*";
}

defresource 'hdfsc',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    die "map side compact only" unless my $map_path = $ENV{mapreduce_map_input_file};
                    my @map_path_parts = split /\//, $map_path;
                    my $map_folder = join "/", @map_path_parts[0..$#map_path_parts-1];
                    my $map_fn  = (split /\//, $map_path)[-1];
                    my $n_files = $_[1];
                    my $shift_amt = $n_files == 10 ? 1 : $n_files == 100 ? 2 : die "only 10 or 100 files";
                    my $stem_path = stem_partfile_path $map_fn, $shift_amt; 
                    my $compact_path = join "/", $map_folder, $stem_path;
                    sh qq{$hadoop_name fs -text $compact_path 2>/dev/null }} @_};
 
defresource 'hdfscname',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    die "map side compact only" unless my $map_path = $ENV{mapreduce_map_input_file};
                    my @map_path_parts = split /\//, $map_path;
                    my $map_folder = join "/", @map_path_parts[0..$#map_path_parts-1];
                    print "$map_path\t$map_folder\n";
                    my $map_fn  = (split /\//, $map_path)[-1];
                    my $n_files = $_[1];
                    my $shift_amt = $n_files == 10 ? 1 : $n_files == 100 ? 2 : die "only 10 or 100 files";
                    print "$n_files\t$shift_amt\t$map_fn\n";
                    my $stem_path = stem_partfile_path $map_fn, $shift_amt; 
                    my $compact_path = join "/", $map_folder, $stem_path;
                    print "$map_path\t$stem_path\t$compact_path\n"; } @_};

defresource 'hdfsj',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    my $total_left_files;
                    my $left_file_number;
                    my $left_path;
                    if(exists $ENV{mapreduce_map_input_file}) {
                      $left_path = $ENV{mapreduce_map_input_file};
                      my $left_folder = join "/", (split /\//, $left_path)[0..-1];
                      my @left_folder_files = hadoop_partsort hadoop_ls $left_folder;
                      $left_file_number = hadoop_partfile_n $left_path; 
                      $total_left_files = @left_folder_files;
                    } else {
                      $left_path = $ENV{mapreduce_task_id};
                      my @left_task_id_parts = split /_/, $left_path;
                      die "not on the reduce side" unless $left_task_id_parts[-2] eq "r";
                      $left_file_number = $left_task_id_parts[-1];
                      $total_left_files = $ENV{mapreduce_job_reduces};
                    }
                    my $right_folder = $_[1];
                    my @right_folder_files = hadoop_partsort hadoop_ls $right_folder;
                    my $right_file_idx = $left_file_number % @right_folder_files; 
                    die "# of left files must be evenly divisible by # of right files" if $total_left_files % @right_folder_files;
                    my $right_file = shell_quote $right_folder_files[$right_file_idx];
                    sh qq{$hadoop_name fs -text $right_file 2>/dev/null }} @_};

defresource 'hdfsjname',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    my $total_left_files;
                    my $left_file_number;
                    my $left_path;
                    if(exists $ENV{mapreduce_map_input_file}) {
                      $left_path = $ENV{mapreduce_map_input_file};
                      my $left_folder = join "/", (split /\//, $left_path)[0..-1];
                      my @left_folder_files = hadoop_partsort hadoop_ls $left_folder;
                      $left_file_number = hadoop_partfile_n $left_path; 
                      $total_left_files = @left_folder_files;
                    } else {
                      $left_path = $ENV{mapreduce_task_id};
                      my @left_task_id_parts = split /_/, $left_path;
                      die "not on the reduce side" unless $left_task_id_parts[-2] eq "r";
                      $left_file_number = $left_task_id_parts[-1];
                      $total_left_files = $ENV{mapreduce_job_reduces};
                    }
                    my $right_folder = $_[1];
                    my @right_folder_files = hadoop_partsort hadoop_ls $right_folder;
                    my $right_file_idx = $left_file_number % @right_folder_files; 
                    die "# of left files must be evenly divisible by # of right files" if $total_left_files % @right_folder_files;
                    my $right_file = shell_quote $right_folder_files[$right_file_idx];
                    print "$left_path\t$right_file\n";} @_}; 
