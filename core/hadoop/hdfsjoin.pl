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


# Map-side partfile concatenation
# Hadoop 2.x has an upper limit of 100,000 partfiles per map/reduce,
# and sometimes (map-only) jobs will have a large number of files
# where each file only contains a small amount of data. Running
# a job on such a file suffers from high overhead for container
# startup/shutdown and instability from map to reduce because of the
# high number of networked connections required to transmit data
# from one side ot the other. To get around the first restriction
# and improve performance, hdfsc takes a maximum
# number of partfiles to process per mapper, and uses
# hadoop fs -text <path> to run on all of the partfiles that 
# have 
# Usage:
# ni ihdfst://path/.../part-00*.path \
#   HS[zn hdfsc://100 mapper] [combiner] [reducer]
# 
# Caveats:
# - zn is necessary to consume the input stream, which will be
#     re-created using hdfsc; this leads to a small amount of overhead.
# - the number of leading zeroes in the input path must match
#     the number of zeroes in the hdfsc://<number> path

sub generate_compact_filename($$) {
  my ($filename, $shift_amount) = @_;  
  my ($prefix, $partfile_index, $suffix) = ($filename =~ /^(part[^\d]+)(\d+)(.*)/);
  my $compact_partfile_index =  substr($partfile_index, $shift_amount); 
  return "$prefix*$compact_partfile_index$suffix";
}

our %FILES_PER_MAPPER_TO_SHIFT= (10 => 1, 100 => 2, 1_000 => 3, 10_000 => 4, 100_000 => 5);
sub generate_compact_tail($$) {
  my ($map_filename, $max_files_per_mapper) = @_;
  my $shift_amount = $FILES_PER_MAPPER_TO_SHIFT{$max_files_per_mapper};
  die "# of files must be a power of 10 between 10 and 10**5, inclusive" unless $shift_amount;
  my $compact_filename = generate_compact_filename $map_filename, $shift_amount;
  return $compact_filename;
}

sub generate_compact_path($$) {
  my ($map_path, $max_files_per_mapper) = @_;
  my @map_path_parts = split /\//, $map_path;
  my $map_folder = join "/", @map_path_parts[0..$#map_path_parts-1];
  my $map_filename  = (split /\//, $map_path)[-1];
  my $compact_tail = generate_compact_tail($map_filename, $max_files_per_mapper);
  my $compact_path = shell_quote join "/", $map_folder, $compact_tail;
  return $compact_path;
}


defresource 'hdfsc',
  read => q{soproc {my $compact_factor = $_[1];
                    die "map side compact only" unless my $map_path = $ENV{mapreduce_map_input_file};
                    my $compact_path = generate_compact_path($map_path, $compact_factor);
                    my $hadoop_name = conf 'hadoop/name';
                    sh qq{$hadoop_name fs -text $compact_path 2>/dev/null }} @_};
 
defresource 'hdfscname',
  read => q{soproc {my $compact_factor = $_[1];
                    print "$compact_factor\n";
                    die "map side compact only" unless my $map_path = $ENV{mapreduce_map_input_file};
                    my $compact_path = generate_compact_path($map_path, $compact_factor);
                    print "$compact_path\n";
                    my $hadoop_name = conf 'hadoop/name';
                    my $files_per_mapper = scalar hadoop_ls $compact_path;
                    print "$files_per_mapper\n";
                    print "$map_path\t$compact_path\t$files_per_mapper\t\n"; } @_};


