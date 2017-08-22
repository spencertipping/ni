# Hash utilities

# Key-By-Value ascending and descending
sub kbv_dsc { my %h = @_; sort { $h{$b} <=> $h{$a} } keys %h }
sub kbv_asc { my %h = @_; sort { $h{$a} <=> $h{$b} } keys %h }
sub dump_array {
  my $r = shift;
  my $indent = $_[0] ? $_[0] : 0;
  print "\t" x $indent, "[\n";
  for my $el (@$r) {
    if( ref $el eq "HASH" ) { 
        dump_hash( $el, $indent + 1 );
    } elsif( ref $el eq "ARRAY") {
        dump_array( $el, $indent + 1);
    } else { 
        print "\t" x ($indent + 1), "$el\n";
    } 
  } 
  print "\t" x $indent, "]\n";
}



sub dump_hash { 
  my $h = shift; 
  my $indent = $_[0] ? $_[0] : 0;
  foreach my $key (keys %$h) { 
    print "\t" x $indent, "$key\t=>";
    if( ref $h->{$key} eq "HASH" ) { 
      print "\n";
      dump_hash( $h->{$key}, $indent + 1);
    } elsif( ref $h->{$key} eq "ARRAY") {
      print "\n";
      dump_array( $h->{$key}, $indent + 1);
    } else { 
      print "\t", $h->{$key}, "\n";
    } 
  }   
}

sub dump_data {
  $dumpme = pop @_;
  print join "\t", @_, "\n";
  if(ref($dumpme) eq "HASH") {
    dump_hash($dumpme);
  } elsif(ref($dumpme) eq "ARRAY") {
    dump_array($dumpme);
  } else {
    print "$dumpme\n";
  }
}

sub merge_hash_values($$) {
  my ($val1, $val2) = @_;
  return $val1 unless defined $val2;
  return $val2 unless defined $val1;

  my $ref1 = ref($val1);
  my $ref2 = ref($val2);
  my $output;
  if ($ref1 eq "" and $ref2 eq "") {
    $output = $val1 || $val2;
  } elsif($ref1 eq "ARRAY" and $ref2 eq "ARRAY") {
    my @output = @$val1;
    push @output, @$val2;
    $output = \@output;
  } elsif($ref1 eq "HASH" and $ref2 eq "HASH") {
    $output = merge_hashes($val1, $val2);
  } else {
    die "cannot merge different types of values value 1: $val1, value 2: $val2\n";
  }
  $output
}

sub accumulate_two_hashes($$) {
  my ($href1, $href2) = @_;
  dump_data "href1: ", $href1;
  dump_data "href2: ", $href2;
  for my $key (keys %{$href2}) {
    $href1->{$key} = {} if not exists $href1->{$key};
    print "key: $key\n";
    my $val = $href2->{$key};
    print "val: $val\n";
    if(ref($val) eq "") {
      $href1->{$key}->{$val} += 1;
    } elsif(ref($val) eq "ARRAY") {
      for (@{$val}) {$href1->{$key}->{$_} += 1;}
    } elsif(ref($val) eq "HASH") {
      $href1 = accumulate_two_hashes($href1->{$key}, $href2->{$key});
    } else {
      die "accumulating went bad";
    }
  }
  $href1;
}

sub merge_two_hashes($$) {
  my ($href1, $href2) = @_;
  my %h1 = %$href1;
  my %h2 = %$href2;
  my @keys = uniq keys %h1, keys %h2; 
  my %h;
  for my $key(@keys) {
    my $val1 = $h1{$key};
    my $val2 = $h2{$key};
    $h{$key} = merge_hash_values($val1, $val2); 
  }
  \%h;
}

# "intelligently" merges values from many 
# multi-dimensional hashes. arrays are appended;
# hashes are recursively accumulated, key-by-key,
# and the first truthy scalar to occupy a particular 
# hash slot is kept. See chapter 6 of ni by example 
# for an example.
sub merge_hashes {
  my $href = shift;
  for(@_) {
    $href = merge_two_hashes($href, $_);
  }
  $href;
}

sub accumulate_hashes {
  my $href = {};
  for(@_) {
    $href = accumulate_two_hashes($href, $_);
  }
  $href;
}

sub freqify_path($$) {
  my $r_hash  = shift;
  my $r_keyArray  = shift;
  my(@keyArray) = @{$r_keyArray};
  my @keyArray = defined($keyArray[0]) ? @keyArray : keys %{$r_hash};
  my $lastKey = pop @keyArray;
  foreach my $key (@keyArray) {
    $r_hash = $r_hash->{$key};
  }
  $r_hash->{$lastKey} = freqs @{$r_hash->{$lastKey}};
}

# Use: freqify(\%h, \@paths) = \%h;
# Converts the arrays located in the multidimensional
# hash structure at that point to hashes of their frequencies
# my $h = {"foo" => {"bar" => [u,u,u,u,v,baz,baz], "qux" => [ay, ay, bee]}};
# my @keys = (foo, [bar, qux]); freqify $h, \@keys;
# $h => {"foo" => {"bar" => {"u" => 4, "v" => 1, "baz" => 2}, 
#                  "qux" => {"ay" => 2, "bee" => 1}}}
sub freqify($$) {
  my ($href, $raw_paths) = @_;
  my @clean_paths = cart map {ref($_) eq "ARRAY" ? $_ : [$_]} @$raw_paths;
  for my $path(@clean_paths) {
    freqify_path($href, $path);
  }
  $href;
}

