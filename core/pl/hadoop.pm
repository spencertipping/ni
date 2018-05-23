# Helpers for interacting with Hadoop and YARN
sub extract_hdfs_path($) {
  my @input_path_parts = $_[0] =~ m([^/]+)mg;
  if($input_path_parts[0] =~ /^hdfs[^:]*:/) {
    shift @input_path_parts;
  };
  if ($input_path_parts[-1] =~ /^(\*|part-)/) {
    pop @input_path_parts;
  }
 "/" . join "/", @input_path_parts;
}

sub extract_parent_path($) {
  my @path_parts = $_[0] =~ m([^/]+)mg;
  pop @path_parts;
  return "/" . join "/", @path_parts;
}

sub hdfs_du_h ($) {
  my $hdfs_path = extract_hdfs_path $_[0];
  `hadoop fs -du -h $hdfs_path`
}

sub hdfs_mkdir_p($) {
  my $hdfs_path = extract_hdfs_path $_[0];
  `hadoop fs -mkdir -p $_[0]`  
}

sub hdfs_rm_r($) {
  my $hdfs_path = extract_hdfs_path $_[0];
  `hadoop fs -rm -r $_[0]`
}

sub hdfs_put($$) {
  `hadoop fs -put -f $_[0] $_[1]`
}

sub hdfs_get($$) {
  my $hdfs_path = extract_hdfs_path $_[1];
  `hadoop fs -get $_[0] $hdfs_path`
}

sub hdfs_ls($) {
  my $hdfs_path = extract_hdfs_path $_[0];
  `hadoop fs -ls -h $hdfs_path`
}

sub hdfs_mv {
  my $raw_output_hdfs_path = pop @_;
  my @input_hdfs_paths  = map {extract_hdfs_path $_} @_;
  my $output_hdfs_folder = extract_hdfs_path($raw_output_hdfs_path);
  my $output_hdfs_parent_folder = extract_parent_path $output_hdfs_folder;
  my @output_hdfs_paths = map {@input_hdfs_paths == 1 ? $output_hdfs_folder : $output_hdfs_folder . "/$_"} 0..$#input_hdfs_paths;
  if(@input_hdfs_paths == 1) {
    hdfs_mkdir_p $output_hdfs_parent_folder;
  } else {
    hdfs_mkdir_p $output_hdfs_folder;
  }
  `hadoop fs -mv $input_hdfs_paths[$_] $output_hdfs_paths[$_]` for 0..$#input_hdfs_paths;
  if(@input_hdfs_paths == 1) {
    return "hdfst://$output_hdfs_folder";
  } else {
    return "hdfst://$output_hdfs_folder/*/*";
  }
  
}

sub yarn_application_kill($) {
  `yarn application -kill $_[0]`
}

BEGIN {
  *hddu = \&hdfs_du_h;
  *hdmp = \&hdfs_mkdir_p;
  *hdrm = \&hdfs_rm_r;
  *hdpt = \&hdfs_put;
  *hdgt = \&hdfs_get;
  *hdls = \&hdfs_ls;
  *hdmv = \&hdfs_mv;
  *yak = \&yarn_application_kill;
}

