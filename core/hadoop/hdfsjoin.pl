
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
                    my $left_path =  $ENV{mapreduce_map_input_file};
                    my $left_file_number = hadoop_partfile_n $left_path; 
                    my $left_folder = join "/", (split /\//, $left_path)[0..-1];
                    my @left_folder_files = hadoop_partsort hadoop_ls $left_folder;
                    my $right_folder = $_[1];
                    my @right_folder_files = hadoop_partsort hadoop_ls $right_folder;
                    my $right_file_idx = $left_file_number % @right_folder_files; 
                    die "number of left files must be evenly divisible by number of right files" if @left_folder_files % @right_folder_files;
                    my $right_file = shell_quote "$right_folder/$right_folder_files[$right_file_idx]";
                    sh qq{$hadoop_name fs -text $right_file 2>/dev/null }} @_};

defresource 'hdfsjname',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    my $left_path =  $ENV{mapreduce_map_input_file};
                    my $left_file_number = hadoop_partfile_n $left_path; 
                    my $left_folder = join "/", (split /\//, $left_path)[0..-1];
                    my @left_folder_files = hadoop_partsort hadoop_ls $left_folder;
                    my $right_folder = $_[1];
                    my @right_folder_files = hadoop_partsort hadoop_ls $right_folder;
                    my $right_file_idx = $left_file_number % @right_folder_files; 
                    die "number of left files must be evenly divisible by number of right files" if @left_folder_files % @right_folder_files;
                    my $right_file = shell_quote "$right_folder/$right_folder_files[$right_file_idx]";
                    print "$left_path\t$right_file\n";} @_}; 

