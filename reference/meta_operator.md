
# META OPERATOR cat

## IMPLEMENTATION
	
	  my ($args, $left, $right) = @_;
	  my ($f) = @$args;
	  my $i = -1;
	  ++$i while $i+1 < @$right && $$right[$i+1][0] eq 'cat';
	  ($left, [cat_multi_op($f, $i > -1 ? map $$_[1], @$right[0..$i] : ()),
	           @$right[$i+1..$#{$right}]]);

# META OPERATOR file_data_closure

## IMPLEMENTATION
	
	  my ($name, $f) = @{$_[0]};
	  my $c    = "file-closure://$name";
	  my $file = resource_write $c;
	  my $fh   = sni @$f;
	  sforward $fh, $file;
	  close $file;
	  close $fh;
	  $fh->await;
	  nuke_on_exit $c;
	  add_quoted_resource $c;
	  ();

# META OPERATOR inline_checkpoint

## IMPLEMENTATION
	
	  my ($args, $left, $right) = @_;
	  my ($file) = @$args;
	  ([], [checkpoint_op($file, $left), @$right]);

# META OPERATOR memory_data_closure

## IMPLEMENTATION
	
	  my ($name, $f) = @{$_[0]};
	  my $data;
	  my $fh = sni @$f;
	  1 while saferead $fh, $data, 8192, length $data;
	  close $fh;
	  $fh->await;
	  add_closure_key $name, $data;
	  ();

# META OPERATOR op_let

## IMPLEMENTATION
	
	  my ($args, $left, $right) = @_;
	  my ($bindings, $ops) = @$args;
	  my @keys = map $$_[0], @$bindings;
	  my %replacements = map @$_, @$bindings;
	  my $rewritten = rewrite_atoms_in $ops, sub {
	    my $a = shift;
	    $a =~ s/\Q$_\E/$replacements{$_}/g for @keys;
	    $a;
	  };
	  ($left, [@$rewritten, @$right]);

# META OPERATOR perl_require

## IMPLEMENTATION
	
	  my ($args, $left, $right) = @_;
	  my $code_fh = sni @$args;
	  my $code    = "BEGIN{\n#line 1 " . json_encode(json_encode($args)) . "\n"
	                                   . join('', <$code_fh>) . "\n}";
	  my $key     = "core/pl/require/" . gensym;
	  self_append_resource $key, $code;
	  push @ni::perl_prefix_keys, $key;
	  ($left, $right);

# META OPERATOR stderr_monitor_transform

## IMPLEMENTATION
	
	  my ($args, $left) = @_;
	  my ($interval) = @$args;
	  [map {;$$left[$_], stderr_monitor_op($_, json_encode $$left[$_], $interval)}
	        0..$#{$left}];
