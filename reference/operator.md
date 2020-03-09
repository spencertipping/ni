
# OPERATOR append
	Append another ni stream to this one

## IMPLEMENTATION
	my @xs = @_; sio; exec_ni @xs

# OPERATOR attenuate

## IMPLEMENTATION
	
	  my ($cs, $power) = @_;
	  my $rand_code = $power == int $power
	    ? join"*", ("rand()") x $power
	    : "rand() ** $power";
	  cell_eval {args => 'undef',
	             each => "\$xs[\$_] *= (1 - $rand_code)"}, $cs;

# OPERATOR binary_fixed

## IMPLEMENTATION
	
	  use bytes;
	  my ($pack_template) = @_;
	  my $length = length pack $pack_template, unpack $pack_template, "\0" x 65536;
	  die "ni: binary_fixed template consumes no data" unless $length;
	  my $bufsize = $length;
	  $bufsize <<= 1 until $bufsize >= 65536;
	  my $buf = '';
	  while (1)
	  {
	    read STDIN, $buf, $bufsize - length($buf), length($buf) or return
	      until length($buf) >= $length;
	    my $o = 0;
	    for (; $o + $length <= length($buf); $o += $length)
	    {
	      print join("\t", unpack $pack_template, substr $buf, $o, $length), "\n";
	    }
	    $buf = substr $buf, $o;
	  }

# OPERATOR binary_perl

## IMPLEMENTATION
	stdin_to_perl binary_perl_mapper $_[0]

# OPERATOR bloom_prehash

## IMPLEMENTATION
	
	  cell_eval {args  => '$m, $k',
	             begin => '($m, $k) = bloom_args $m, $k',
	             each  => '$xs[$_] = bloom_prehash $m, $k, $xs[$_]'}, @_;

# OPERATOR bloom_rows

## IMPLEMENTATION
	
	  use bytes;
	  my ($include_mode, $col, $bloom_lambda) = @_;
	  my $bloom;
	  my $r = sni @$bloom_lambda;
	  1 while read $r, $bloom, 65536, length $bloom;
	  $r->await;
	  while (<STDIN>) {
	    chomp(my @cols = split /\t/, $_, $col + 2);
	    print if !$include_mode == !bloom_contains $bloom, $cols[$col];
	  }

# OPERATOR bloomify

## IMPLEMENTATION
	
	  my ($n, $p) = @_;
	  my $f = bloom_new $n, $p;
	  chomp, bloom_add $f, $_ while <STDIN>;
	  print $f;

# OPERATOR bloomify_hex

## IMPLEMENTATION
	
	  my ($n, $p) = @_;
	  my $f = bloom_new $n, $p;
	  chomp, bloom_add $f, $_ while <STDIN>;
	  print unpack("H*", $f), "\n";

# OPERATOR bloomify_prehashed

## IMPLEMENTATION
	
	  my ($n, $p) = @_;
	  my $f = bloom_new $n, $p;
	  chomp, bloom_add_prehashed $f, $_ while <STDIN>;
	  print $f;

# OPERATOR buffer_null

## IMPLEMENTATION
	local $SIG{PIPE} = 'IGNORE'; sio

# OPERATOR cat_multi

## IMPLEMENTATION
	sio; scat $_ for @_

# OPERATOR cell_clean_regex

## IMPLEMENTATION
	
	  cell_eval {args  => '$qre',
	             begin => '',
	             each  => '$xs[$_] =~ s/$qre//g'}, @_;

# OPERATOR cell_exp

## IMPLEMENTATION
	
	  my ($cs, $base) = @_;
	  my $eb = log $base;
	  cell_eval {args => 'undef', each => "\$xs[\$_] = exp $eb * \$xs[\$_]"}, $cs;

# OPERATOR cell_log

## IMPLEMENTATION
	
	  my ($cs, $base) = @_;
	  my $lb = 1 / log $base;
	  cell_eval {args => 'undef', each => "\$xs[\$_] = log(max 1e-16, \$xs[\$_]) * $lb"}, $cs;

# OPERATOR cell_signed_log

## IMPLEMENTATION
	
	  my ($cs, $base) = @_;
	  my $lb = 1 / log $base;
	  cell_eval {
	    args => 'undef',
	    each => "\$xs[\$_] = (\$xs[\$_] > 0 ? $lb : -$lb) * log(1 + abs \$xs[\$_])"}, $cs;

# OPERATOR checkpoint

## IMPLEMENTATION
	
	  my ($file, $deps, $generator) = @_;
	  sio;
	  checkpoint_create $file, $generator if checkpoint_needs_regen $file, $deps;
	  scat $file;

# OPERATOR cleandos

## IMPLEMENTATION
	exec shell_quote 'perl', '-npe', 's/\r\n/\n/g'

# OPERATOR col_average

## IMPLEMENTATION
	
	  cell_eval {args  => 'undef',
	             begin => 'my @ns = map 0, @cols; $. = 0',
	             each  => '$xs[$_] = ($ns[$_] += $xs[$_]) / $.'}, @_;

# OPERATOR col_delta

## IMPLEMENTATION
	
	  cell_eval {args  => 'undef',
	             begin => 'my @ns = map 0, @cols',
	             each  => '$xs[$_] -= $ns[$_], $ns[$_] += $xs[$_]'}, @_;

# OPERATOR col_sum

## IMPLEMENTATION
	
	  cell_eval {args  => 'undef',
	             begin => 'my @ns = map 0, @cols',
	             each  => '$xs[$_] = $ns[$_] += $xs[$_]'}, @_;

# OPERATOR cols

## IMPLEMENTATION
	
	  my ($floor, @cs) = @_;
	  my $asc = join('', @cs) eq join('', sort {$a <=> $b} @cs);
	  my %dup; ++$dup{$_} for @cs;
	  return col_cut $floor + 1, scalar(grep $_ == -1, @cs), map $_ + 1, @cs
	    if !conf "col/disallow-cut" && $asc && !grep $_ > 1, values %dup;
	  exec 'perl', '-lne',
	       cols_gen->(limit => $floor + 1,
	                  is    => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cs);

# OPERATOR colswap

## IMPLEMENTATION
	
	  my ($floor, @cs) = @_;
	  my %cs; ++$cs{$_} for @cs;
	  die "ni colswap: . doesn't make sense"    if grep $_ == -1, @cs;
	  die "ni colswap: can't duplicate columns" if grep $_ > 1, values %cs;
	  my $n = 0;
	  my @cols = 0..$floor-1;
	  swap $cols[$n++], $cols[$_] for @cs;
	  exec 'perl', '-lne', cols_gen->(limit => $floor + 1,
	                                  is    => join ',', @cols, "$floor..\$#_");

# OPERATOR composite_images

## IMPLEMENTATION
	
	  my ($init, $reducer, $emitter) = @_;
	  my $ic = conf 'image_command';
	  my $reduced_image = substr(resource_tmp 'file://', 7) . '.png';
	  my $temp_image    = substr(resource_tmp 'file://', 7) . '.png';
	
	  my $reduced_q = shell_quote $reduced_image;
	  my $temp_q    = shell_quote $temp_image;
	
	  if (defined simage_into {sh "$ic - $init $reduced_q"}) {
	    image_sync_sh "$ic $reduced_q $emitter png:-";
	    image_sync_sh "$ic $reduced_q $emitter png:-"
	      while defined simage_into {sh "$ic $reduced_q $reducer $temp_q; mv $temp_q $reduced_q"};
	  }
	
	  unlink $reduced_image;

# OPERATOR conf_get

## IMPLEMENTATION
	
	  my ($name) = @_;
	  sio();
	  print conf $name, "\n";

# OPERATOR configure

## IMPLEMENTATION
	
	  my ($vars, $f) = @_;
	  conf_set $_, $$vars{$_} for keys %$vars;
	  conf_set monitor => 0 unless exists $$vars{monitor};
	  &$ni::main_operator(flatten_operators $f);

# OPERATOR count

## IMPLEMENTATION
	
	  my ($n, $last) = (0, undef);
	  while (<STDIN>) {
	    if (!defined $last or $_ ne $last) {
	      print "$n\t$last" if defined $last;
	      $n = 0;
	      $last = $_;
	    }
	    ++$n;
	  }
	  print "$n\t$last" if defined $last;

# OPERATOR decode

## IMPLEMENTATION
	sdecode

# OPERATOR dense_to_sparse

## IMPLEMENTATION
	
	  my ($col) = @_;
	  $col ||= 0;
	  my @q;
	  my $n = 0;
	  while (defined($_ = @q ? shift @q : <STDIN>)) {
	    chomp(my @fs = split /\t/);
	    if ($col) {
	      $n = 0;
	      my $k  = join "\t", @fs[0..$col-1];
	      my $kr = qr/\Q$k\E/;
	      print join("\t", $k, $n, $_ - $col, $fs[$_]), "\n" for $col..$#fs;
	      my $l;
	      while (defined($l = <STDIN>) && $l =~ /^$kr\t/) {
	        ++$n;
	        chomp(@fs = split /\t/, $l);
	        print join("\t", $k, $n, $_ - $col, $fs[$_]), "\n" for $col..$#fs;
	      }
	      push @q, $l if defined $l;
	    } else {
	      print join("\t", $n, $_, $fs[$_]), "\n" for 0..$#fs;
	      ++$n;
	    }
	  }

# OPERATOR destructure

## IMPLEMENTATION
	
	  ni::eval gen(q{
	    no warnings 'uninitialized';
	    eval {binmode STDOUT, ":encoding(utf-8)"};
	    print STDERR "ni: warning: your perl might not handle utf-8 correctly\n" if $@;
	    while (<STDIN>) {
	      %e;
	    }
	  })->(e => json_extractor $_[0]);

# OPERATOR dev_backdoor

## IMPLEMENTATION
	ni::eval $_[0]

# OPERATOR dev_local_operate

## IMPLEMENTATION
	
	  my ($lambda) = @_;
	  my $fh = siproc {exec ni_quoted_exec_args};
	  quote_ni_into $fh, @$lambda;

# OPERATOR divert
	Duplicate this stream into a ni pipeline, discarding that pipeline's output

## IMPLEMENTATION
	
	  my @xs = @_;
	  my $fh = siproc {close STDOUT; exec_ni @xs, sink_null_op};
	  stee \*STDIN, $fh, \*STDOUT;
	  close $fh;
	  $fh->await;

# OPERATOR docker_exec

## IMPLEMENTATION
	
	  my ($container, @f) = @_;
	  my $fh = siproc {exec qw|docker exec -i|, $container, ni_quoted_exec_args};
	  quote_ni_into $fh, @f;

# OPERATOR docker_run_dynamic

## IMPLEMENTATION
	
	  my ($dockerfile, @f) = @_;
	  my $fh = siproc {
	    my $quoted_dockerfile = shell_quote 'printf', '%s', $dockerfile;
	    my $quoted_args       = shell_quote ni_quoted_exec_args;
	    my $image_name        = "ni-tmp-" . lc noise_str 32;
	    sh qq{image_name=\`$quoted_dockerfile | docker build -q -\`
	          if [ \${#image_name} -gt 80 ]; then \
	            $quoted_dockerfile | docker build -q -t $image_name - >&2
	            image_name=$image_name
	            docker run --rm -i \$image_name $quoted_args
	            docker rmi --no-prune=true \$image_name
	          else
	            docker run --rm -i \$image_name $quoted_args
	          fi};
	  };
	  quote_ni_into $fh, @f;

# OPERATOR docker_run_image

## IMPLEMENTATION
	
	  my ($image, @f) = @_;
	  my $fh = siproc {exec qw|docker run --rm -i|, $image, ni_quoted_exec_args};
	  quote_ni_into $fh, @f;

# OPERATOR each_image

## IMPLEMENTATION
	
	  my ($lambda) = @_;
	  $ENV{KEY} = 0;
	  1 while ++$ENV{KEY} && defined simage_into {exec_ni @$lambda};

# OPERATOR echo
	Append text verbatim

## IMPLEMENTATION
	my ($x) = @_; sio; print "$x\n"

# OPERATOR encode_resource_stream

## IMPLEMENTATION
	
	  my @xs;
	  while (<STDIN>) {
	    chomp;
	    my $s = rfc $_;
	    my $line_count = @xs = split /\n/, "$s ";
	    print "$line_count $_\n", $s, "\n";
	  }

# OPERATOR epoch_to_formatted

## IMPLEMENTATION
	
	  cell_eval {args => 'undef',
	             each => q{$xs[$_] = sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ",
	                                         time_epoch_pieces $xs[$_]}}, @_;

# OPERATOR file_closure_append

## IMPLEMENTATION
	
	  sio;
	  sforward resource_read(closure_data $_[0]), \*STDOUT;

# OPERATOR file_prepend_name_number_read

## IMPLEMENTATION
	
	  my ($colspec, $transform) = @_;
	  $colspec   = [1, 0] unless defined $colspec;
	  $transform = defined $transform ? eval "sub {local \$_ = shift; $transform}"
	                                  : sub {shift};
	
	  my ($maxcol, $coli) = @$colspec;
	  my $file;
	  while (defined($file = <STDIN>))
	  {
	    chomp $file;
	    my $line  = 0;
	    my $fname = (split /\t/, $file, $maxcol)[$coli];
	    my $fh    = soproc {scat &$transform($fname)};
	    ++$line, chomp, print "$file\t$line\t$_\n" while <$fh>;
	    close $fh;
	    $fh->await;
	  }

# OPERATOR file_prepend_name_read

## IMPLEMENTATION
	
	  my ($colspec, $transform) = @_;
	  $colspec   = [1, 0] unless defined $colspec;
	  $transform = defined $transform ? eval "sub {local \$_ = shift; $transform}"
	                                  : sub {shift};
	
	  my ($maxcol, $coli) = @$colspec;
	  my $file;
	  while (defined($file = <STDIN>))
	  {
	    chomp $file;
	    my $fname = (split /\t/, $file, $maxcol)[$coli];
	    my $fh    = soproc {scat &$transform($fname)};
	    chomp, print "$file\t$_\n" while <$fh>;
	    close $fh;
	    $fh->await;
	  }

# OPERATOR file_prepend_name_write

## IMPLEMENTATION
	
	  my ($lambda) = @_;
	  my $file     = undef;
	  my $fh       = undef;
	
	  $lambda = undef if ref $lambda && !@$lambda;
	
	  while (<STDIN>)
	  {
	    my ($fname, $l) = /^([^\t\n]+)\t([\s\S]*)/;
	    ($fname, $l) = ($file, "\n") unless defined $fname;
	    if (!defined $file or $fname ne $file)
	    {
	      close $fh, $fh->can('await') && $fh->await if defined $fh;
	      print "$file\n" if defined $file;
	      $file = $fname;
	
	      # NB: swfile has much lower startup overhead than exec_ni(), so use that
	      # unless we have a lambda that requires slower operation.
	      $fh = defined $lambda
	        ? siproc {exec_ni(@$lambda, file_write_op($file), sink_null_op)}
	        : swfile $file;
	    }
	    print $fh $l;
	  }
	
	  close $fh, $fh->can('await') && $fh->await if defined $fh;
	  print "$file\n" if defined $file;

# OPERATOR file_read

## IMPLEMENTATION
	chomp, weval q{scat $_} while <STDIN>

# OPERATOR file_write

## IMPLEMENTATION
	
	  my ($file) = @_;
	  $file = resource_tmp('file://') unless defined $file;
	  my $fh = swfile $file;
	  sforward \*STDIN, $fh;
	  close $fh;
	  $fh->await if $fh->can('await');
	  print "$file\n";

# OPERATOR flatten_tabs

## IMPLEMENTATION
	
	  my ($gapsize) = @_;
	  my $eof = 0;
	  $gapsize //= 2;
	
	  until ($eof)
	  {
	    my @lines;
	    my @widths = ();
	    while (@lines < 1024 && !($eof ||= !defined($_ = <STDIN>)))
	    {
	      chomp;
	      my @fs = split /\t/;
	      push @lines, \@fs;
	      $widths[$_] = max $widths[$_] // 0, length $fs[$_] // 0 for 0..$#fs;
	    }
	
	    # The final width is never necessary because nothing after it needs to be
	    # aligned. If we tried to align it, we'd append a bunch of trailing spaces
	    # to each line.
	    pop @widths;
	
	    my $format = join("", map "%-" . ($_ + $gapsize) . "s", @widths) . "%s\n";
	    printf $format, @$_ for @lines;
	  }

# OPERATOR geohash_decode

## IMPLEMENTATION
	
	  cell_eval {args => '@precision',
	             each => q{$xs[$_] = join",", geohash_decode $xs[$_], @precision}}, @_;

# OPERATOR geohash_encode

## IMPLEMENTATION
	
	  cell_eval {args => '@precision',
	             each => q{$xs[$_] = geohash_encode split(/,/, $xs[$_]), @precision}}, @_;

# OPERATOR geojsonify

## IMPLEMENTATION
	
	  while (<STDIN>)
	  {
	    chomp;
	    my ($geom, $props) = geojson_parse(split /\t/);
	    print geojson_row_gen->(geom  => $geom,
	                            props => json_encode($props)), "\n";
	  }

# OPERATOR git_cat_objects

## IMPLEMENTATION
	
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

# OPERATOR gnuplot_all

## IMPLEMENTATION
	
	  my $code_prefix = shift || "set terminal wx persist";
	  my @col_vectors;
	  my @col_titles;
	
	  chomp(my $l = <STDIN>);
	  my @fs = split /\t/, $l;
	  if (grep !looks_like_number($_), @fs)
	  {
	    # First line is a header
	    @col_titles = @fs[1..$#fs];
	  }
	  else
	  {
	    # First line is data
	    @col_titles  = ("A".."Z")[1..$#fs];
	    @col_vectors = map [$_], @fs;
	  }
	
	  while (<STDIN>)
	  {
	    chomp;
	    my @fs = split /\t/;
	    push @{$col_vectors[$_] ||= []}, 0+$fs[$_] for 0..$#fs;
	  }
	
	  # NB: col A is implicitly the shared X coordinate
	  my $code = $code_prefix
	    . ";plot "
	    . join",", map "\"-\" with lines title \"$_\"", @col_titles;
	
	  my $xs = shift @col_vectors;
	  my $fh = siproc {exec 'gnuplot', '-e', $code};
	  for my $v (@col_vectors)
	  {
	    print $fh "$$xs[$_]\t$$v[$_]\n" for 0..$#$v;
	    print $fh "e\n";
	  }
	  close $fh;
	  $fh->await;

# OPERATOR hadoop_make_nukeable

## IMPLEMENTATION
	print sr $_, qr/^hdfst?/, 'hdfsrm' while <STDIN>

# OPERATOR hadoop_streaming

## IMPLEMENTATION
	
	  my ($mapper, $map_cmd_ref, 
	      $combiner, $combine_cmd_ref,
	      $reducer, $reduce_cmd_ref,
	      $nuke_inputs, $ipath_ref,
	      $streaming_jar) = hadoop_cmd_setup @_;
	
	  for my $ipaths (@$ipath_ref) {
	    my $opath = resource_tmp "hdfs://";
	    my $cmd = make_hadoop_cmd($mapper, $map_cmd_ref,
	                              $combiner, $combine_cmd_ref,
	                              $reducer, $reduce_cmd_ref, 
	                              $streaming_jar, $ipaths, $opath);
	    die "hadoop command text too long" if length $cmd > 125_000;
	    my $hadoop_fh = siproc {
	     sh "$cmd 1>&2";
	    };
	
	    close $hadoop_fh;
	    warn "ni: hadoop streaming failed" if $hadoop_fh->await;
	
	    /^hdfsrm:/ && resource_nuke($_) for @$ipaths;
	
	    (my $result_path = $opath) =~ s/^hdfs:/hdfst:/;
	    print "$result_path/part-*\n";
	  }
	
	  if ($nuke_inputs) {resource_nuke $_ for map @$_, @$ipath_ref}
	
	  resource_nuke $mapper;
	  resource_nuke $combiner if defined $combiner;
	  resource_nuke $reducer  if defined $reducer;

# OPERATOR hadoop_test

## IMPLEMENTATION
	
	  my ($mapper, $map_cmd_ref, 
	      $combiner, $combine_cmd_ref,
	      $reducer, $reduce_cmd_ref,
	      $nuke_inputs, $ipath_ref,
	      $streaming_jar) = hadoop_cmd_setup @_;
	
	  for my $ipaths (@$ipath_ref) {
	    my $opath = resource_tmp "hdfs://";
	    my $cmd = make_hadoop_cmd($mapper, $map_cmd_ref, 
	                              $combiner, $combine_cmd_ref,
	                              $reducer, $reduce_cmd_ref,
	                              $streaming_jar, $ipaths, $opath);
	    print "$cmd\n";
	  }

# OPERATOR head

## IMPLEMENTATION
	exec 'head', @_

# OPERATOR http_websocket_encode

## IMPLEMENTATION
	
	  load 'core/http/ws.pm';
	  safewrite \*STDOUT, ws_encode($_) while <STDIN>;

# OPERATOR http_websocket_encode_batch

## IMPLEMENTATION
	
	  load 'core/http/ws.pm';
	  safewrite \*STDOUT, ws_encode($_) while saferead \*STDIN, $_, $_[0] || 8192;

# OPERATOR identity

## IMPLEMENTATION
	sio

# OPERATOR interleave

## IMPLEMENTATION
	
	  my ($ratio, $lambda) = @_;
	  my $fh = soproc {close STDIN; exec_ni @$lambda};
	
	  if ($ratio) {
	    $ratio = 1/-$ratio if $ratio < 0;
	    my ($n1, $n2) = (0, 0);
	    while (1) {
	      ++$n1, defined($_ = <STDIN>) || goto done, print while $n1 <= $n2 * $ratio;
	      ++$n2, defined($_ = <$fh>)   || goto done, print while $n1 >= $n2 * $ratio;
	    }
	  } else {
	    my $rmask;
	    my ($stdin_ok,  $ni_ok) = (1, 1);
	    my ($stdin_buf, $ni_buf);
	    while ($stdin_ok || $ni_ok) {
	      vec($rmask, fileno STDIN, 1) = $stdin_ok;
	      vec($rmask, fileno $fh,   1) = $ni_ok;
	      my $n = select my $rout = $rmask, undef, undef, 0.01;
	      if (vec $rout, fileno STDIN, 1) {
	        $stdin_ok = !!saferead \*STDIN, $stdin_buf, 1048576, length $stdin_buf;
	        my $i = 1 + rindex $stdin_buf, "\n";
	        if ($i) {
	          safewrite \*STDOUT, substr $stdin_buf, 0, $i;
	          $stdin_buf = substr $stdin_buf, $i;
	        }
	      }
	      if (vec $rout, fileno $fh, 1) {
	        $ni_ok = !!saferead $fh, $ni_buf, 1048576, length $ni_buf;
	        my $i = 1 + rindex $ni_buf, "\n";
	        if ($i) {
	          safewrite \*STDOUT, substr $ni_buf, 0, $i;
	          $ni_buf = substr $ni_buf, $i;
	        }
	      }
	    }
	  }
	
	  done:
	  close $fh;
	  $fh->await;

# OPERATOR intify_compact

## IMPLEMENTATION
	
	  cell_eval {args  => 'undef',
	             begin => 'my %ids; my $n = 0',
	             each  => '$xs[$_] = ($ids{$xs[$_]} ||= ++$n) - 1'}, @_;

# OPERATOR intify_hash

## IMPLEMENTATION
	
	  cell_eval {args  => '$seed',
	             begin => '$seed ||= 0',
	             each  => '$xs[$_] = unpack "N", md5 $xs[$_] . $seed'}, @_;

# OPERATOR jitter_gaussian

## IMPLEMENTATION
	
	  my ($cs, $mag) = @_;
	  cell_eval {
	    args => 'undef',
	    each => "\$xs[\$_] += $mag * sqrt(-2 * log(max 1e-16, rand()))
	                                         * cos(6.28318530717959 * rand())"},
	    $cs;

# OPERATOR jitter_uniform

## IMPLEMENTATION
	
	  my ($cs, $mag, $bias) = @_;
	  my $adjust = $bias - $mag / 2;
	  cell_eval {args => 'undef', each => "\$xs[\$_] += rand() * $mag + $adjust"}, $cs;

# OPERATOR join

## IMPLEMENTATION
	
	  my ($left_cols, $right_cols, $f) = @_;
	  my $fh = sni @$f;
	  my ($leof, $reof) = (0, 0);
	  my ($llimit, @lcols) = @$left_cols;
	  my ($rlimit, @rcols) = @$right_cols;
	  my @rrows = ();
	  my @lrows = ();
	
	  chomp(my $lkey = join "\t", (split /\t/, my $lrow = <STDIN>, $llimit + 1)[@lcols]);
	  chomp(my $rkey = join "\t", (split /\t/, my $rrow = <$fh>,   $rlimit + 1)[@rcols]);
	
	  while (!$leof && !$reof) {
	    if ($lkey lt $rkey) {
	      chomp($lkey = join "\t", (split /\t/, $lrow = <STDIN>, $llimit + 1)[@lcols]);
	      $leof ||= !defined $lrow;
	    } elsif ($lkey gt $rkey) {
	      chomp($rkey = join "\t", (split /\t/, $rrow = <$fh>,   $rlimit + 1)[@rcols]);
	      $reof ||= !defined $rrow;
	    } else {
	      @rrows = $rrow;
	      while(!$reof) {
	        chomp(my $new_rkey = join "\t", (split /\t/, $rrow = <$fh>, $rlimit + 1)[@rcols]);
	        $reof ||= !defined $rrow;
	        if($new_rkey eq $rkey) {
	          push @rrows, $rrow;
	        } else {
	          $rkey = $new_rkey;
	          last;
	        }
	      }
	
	      my @clean_rrows = ();
	      my %delete_inds = map {$_ => 1} @rcols;
	      for my $rrow(@rrows) {
	        my @row_data = split /\t/, $rrow;
	        push @clean_rrows, join "\t", @row_data[grep {not $delete_inds{$_}} 0..$#row_data];
	      }
	      # If we join on all the columns on the right
	      # we'll need to append a newline;
	      @clean_rrows = map {substr($_, -1) ne "\n" ? "$_\n" : $_ } @clean_rrows;
	
	      while(!$leof) {
	        chomp $lrow;
	        print "$lrow\t$_" for @clean_rrows;
	        chomp(my $new_lkey = join "\t", (split /\t/, $lrow = <STDIN>, $llimit + 1)[@lcols]);
	        if ($new_lkey ne $lkey) { $lkey = $new_lkey; last;}
	      }
	    }
	  }
	  if (!$leof) {
	    # We need to stream the entire left side
	    # of the join to avoid breaking the pipe in
	    # a Hadoop streaming context.
	    1 while read STDIN, $_, 65536;
	  }

# OPERATOR lisp_code

## IMPLEMENTATION
	
	  my ($code) = @_;
	  cdup2 0, 3;
	  POSIX::close 0;
	  safewrite siproc {exec qw| sbcl --noinform --noprint --eval |,
	                         '(load *standard-input* :verbose nil :print nil)'},
	            $code;

# OPERATOR mapomatic

## IMPLEMENTATION
	
	  mapomatic_server 32768, geojson_container_gen->(features => join",", <STDIN>);

# OPERATOR md5

## IMPLEMENTATION
	
	  cell_eval {args  => 'undef',
	             begin => '',
	             each  => '$xs[$_] = md5_hex $xs[$_]'}, @_;

# OPERATOR mdtable

## IMPLEMENTATION
	
	  chomp(my $header = <STDIN>);
	  my $cols = $header =~ y/\t/|/;
	  print "|$header|\n";
	  print "|:----:|" . ":----:|" x $cols . "\n";
	  chomp, y/\t/|/, print "|$_|\n" while <STDIN>;

# OPERATOR memory_closure_append

## IMPLEMENTATION
	sio; print closure_data $_[0]

# OPERATOR memory_join

## IMPLEMENTATION
	
	  my ($col, $n, $f) = @_;
	  my $default = "\t" x $n;
	  my %lookup;
	  my $fh = sni @$f;
	  chomp, /^([^\t]+)\t(.*)/ and $lookup{$1} = $2 while <$fh>;
	  close $fh;
	  $fh->await;
	
	  while (<STDIN>)
	  {
	    chomp;
	    my $f = (split /\t/, $_, $col + 2)[$col];
	    print exists $lookup{$f}
	      ? "$_\t$lookup{$f}\n"
	      : "$_$default\n";
	  }

# OPERATOR meta_conf

## IMPLEMENTATION
	
	  sio;
	  print "$_\t" . conf($_) . "\t$ni::conf_variables{$_}\n" for sort keys %ni::conf_variables;

# OPERATOR meta_eval_number

## IMPLEMENTATION
	sio; print $ni::evals{$_[0] - 1}, "\n"

# OPERATOR meta_help

## IMPLEMENTATION
	
	  my ($topic) = @_;
	  $topic = 'tutorial' unless length $topic;
	  sio; print $ni::self{"doc/$topic.md"}, "\n";

# OPERATOR meta_image

## IMPLEMENTATION
	sio; print image, "\n"

# OPERATOR meta_key

## IMPLEMENTATION
	my @ks = @_; sio; print "$_\n" for @ni::self{@ks}

# OPERATOR meta_keys

## IMPLEMENTATION
	sio; print "$_\n" for sort keys %ni::self

# OPERATOR meta_op

## IMPLEMENTATION
	sio; print "sub {$ni::operators{$_[0]}}\n"

# OPERATOR meta_ops

## IMPLEMENTATION
	sio; print "$_\n" for sort keys %ni::operators

# OPERATOR meta_options

## IMPLEMENTATION
	
	  sio;
	  for my $c (sort keys %ni::contexts) {
	    printf "%s\tlong\t%s\t%s\n",  meta_context_name $c, $ni::long_names{$c}[$_], abbrev dev_inspect_nonl $ni::long_refs{$c}[$_],  40 for       0..$#{$ni::long_refs{$c}};
	    printf "%s\tshort\t%s\t%s\n", meta_context_name $c, $_,                      abbrev dev_inspect_nonl $ni::short_refs{$c}{$_}, 40 for sort keys %{$ni::short_refs{$c}};
	  }

# OPERATOR meta_parser

## IMPLEMENTATION
	sio; print json_encode(parser $_[0]), "\n"

# OPERATOR meta_parsers

## IMPLEMENTATION
	sio; print "$_\t" . json_encode(parser $_) . "\n" for sort keys %ni::parsers

# OPERATOR meta_short_availability

## IMPLEMENTATION
	
	  sio;
	  print "--------" . qwerty_prefixes . "\n";
	  for my $c (sort keys %ni::contexts) {
	    my $s = $ni::short_refs{$c};
	    my %multi;
	    ++$multi{substr $_, 0, 1} for grep 1 < length, keys %$s;
	
	    print substr(meta_context_name $c, 0, 7) . "\t"
	        . join('', map $multi{$_} ? '.' : $$s{$_} ? '|' : ' ',
	                       split //, qwerty_prefixes)
	        . "\n";
	  }

# OPERATOR n
	Append consecutive integers within a range

## IMPLEMENTATION
	
	  my ($l, $u) = @_;
	  sio; for (my $i = $l; $u < 0 || $i < $u; ++$i) {print "$i\n"};

# OPERATOR numpy_dense

## IMPLEMENTATION
	
	  my ($col, $f) = @_;
	  $col ||= 0;
	  my ($i, $o) = sioproc {
	    exec 'python', '-c', numpy_gen->(body => indent $f, 2)
	      or die "ni: failed to execute python: $!"};
	
	  my @q;
	  my ($rows, $cols);
	  my $zero = pack F => 0;
	  while (defined($_ = @q ? shift @q : <STDIN>)) {
	    chomp;
	    my @r = split /\t/;
	    my $k = $col ? join("\t", @r[0..$col-1]) : '';
	    $rows = 1;
	    my @m = pack "F*", @r[$col..$#r];
	    my $kr = qr/\Q$k\E/;
	    ++$rows, push @m, pack "F*", split /\t/, $col ? substr $_, length $1 : $_
	      while defined($_ = <STDIN>) and !$col || /^($kr\t)/;
	    push @q, $_ if defined;
	
	    $cols = max map length() / 8, @m;
	    safewrite_exactly $i, pack NN => $rows, $cols;
	    safewrite_exactly $i, length() < $cols * 8
	                            ? $_ . $zero x ($cols - length() / 8)
	                            : $_
	      for @m;
	
	    $_ = '';
	    saferead_exactly $o, $_, 8;
	    ($rows, $cols) = unpack "NN", $_;
	    for my $r (1..$rows)
	    {
	      $_ = '';
	      saferead_exactly $o, $_, $cols*8;
	      print join("\t", $col ? ($k) : (), unpack "F$cols", $_), "\n";
	    }
	  }
	
	  close $i;
	  close $o;
	  $o->await;

# OPERATOR op_fn

## IMPLEMENTATION
	
	  my ($bindings, $ops) = @_;
	  while (<STDIN>) {
	    chomp;
	    my @vars = split /\t/;
	    my %replacements;
	    @replacements{@$bindings} = @vars;
	    my $rewritten = rewrite_atoms_in $ops, sub {
	      my $a = shift;
	      $a =~ s/\Q$_\E/$replacements{$_}/g for @$bindings;
	      $a;
	    };
	    close(my $fh = siproc {exec_ni @$rewritten});
	    $fh->await;
	  }

# OPERATOR partial_sort

## IMPLEMENTATION
	
	  my $sort_size = shift;
	  my @buff = ();
	  while (<STDIN>) {
	    push @buff, $_;
	    if (@buff == $sort_size) {
	      print sort(@buff);
	      @buff = ();
	    }
	  }
	  print sort(@buff) if @buff;

# OPERATOR partial_transpose

## IMPLEMENTATION
	
	  my ($col) = @_;
	  while (<STDIN>)
	  {
	    chomp;
	    my @fs   = split /\t/;
	    my $base = join "", map "$_\t", @fs[0..$col-1];
	    print "$base$fs[$_]\n" for $col..$#fs;
	  }

# OPERATOR perl_assert

## IMPLEMENTATION
	stdin_to_perl perl_asserter $_[0]

# OPERATOR perl_cell_transformer

## IMPLEMENTATION
	
	  my ($colspec, $code) = @_;
	  my ($limit, @cols) = @$colspec;
	  my $gen = gen q{
	    for my $fi (%cols) {
	      $_ = $F[$fi];
	      $F[$fi] = row;
	    }
	    r @F;
	  };
	  stdin_to_perl perl_mapgen->(
	    prefix   => perl_prefix,
	    closures => perl_closures,
	    body     => perl_expand_begin $code,
	    each     => $gen->(cols => @cols ? join ',', @cols : '0..$#F'));

# OPERATOR perl_grepper

## IMPLEMENTATION
	stdin_to_perl perl_grepper  $_[0]

# OPERATOR perl_mapper

## IMPLEMENTATION
	stdin_to_perl perl_mapper   $_[0]

# OPERATOR perl_prefix

## IMPLEMENTATION
	 sio; print join"\n", @ni::perl_prefix_keys 

# OPERATOR pivot_table

## IMPLEMENTATION
	
	  my $row_id = 0;
	  my $col_id = 0;
	  my %row_ids;
	  my %col_ids;
	  my @cells;
	  my @row_labels;
	  my @col_labels;
	
	  while (<STDIN>)
	  {
	    chomp;
	    my ($row, $col, $v) = split /\t/;
	    my $x = ($col_ids{$col} ||= ++$col_id) - 1;
	    my $y = ($row_ids{$row} ||= ++$row_id) - 1;
	    $row_labels[$y] = $row;
	    $col_labels[$x] = $col;
	    ${$cells[$y] ||= []}[$x] += $v;
	  }
	
	  print join("\t", "", @col_labels), "\n";
	  for my $i (0..$#cells)
	  {
	    print join("\t", $row_labels[$i], @{$cells[$i] || []}), "\n";
	  }

# OPERATOR port_forward

## IMPLEMENTATION
	
	  my ($host, $port) = @_;
	  exec 'ssh', '-L', "$port:localhost:$port", '-N', @$host;

# OPERATOR prepend
	Prepend a ni stream to this one

## IMPLEMENTATION
	
	  my @xs = @_;
	  close(my $fh = siproc {exec_ni @xs});
	  $fh->await;
	  sio;

# OPERATOR pyspark_local_text

## IMPLEMENTATION
	
	  my ($fn) = @_;
	  my $inpath   = join ',', map sr("file://$_", qr/\n$/, ''), <STDIN>;
	  my $outpath  = "/tmp/ni-$$-out";
	  my $tempfile = "/tmp/ni-$$-temp.py";
	  safewrite swfile($tempfile),
	    pyspark_text_io_gen->(
	      master      => pyquote 'local[*]',
	      name        => pyquote "ni $inpath -> $outpath",
	      input_path  => pyquote $inpath,
	      output_path => pyquote "file://$outpath",
	      body        => $fn);
	  local $SIG{CHLD} = 'DEFAULT';
	  die "ni: pyspark failed with $_" if $_ = system 'spark-submit', $tempfile;
	  print "$outpath\n";

# OPERATOR pyspark_preview

## IMPLEMENTATION
	sio; print "$_[0]\n"

# OPERATOR quantize

## IMPLEMENTATION
	
	  my ($cs, $q) = @_;
	  my $iq = 1 / $q;
	  cell_eval {args => 'undef',
	             each => "\$xs[\$_] = $q * int(0.5 + $iq * \$xs[\$_])"}, $cs;

# OPERATOR real_hash

## IMPLEMENTATION
	
	  cell_eval {args  => '$seed',
	             begin => '$seed ||= 0',
	             each  => '$xs[$_] = unpack("N", md5 $xs[$_] . $seed) / (1<<32)'}, @_;

# OPERATOR resource_append

## IMPLEMENTATION
	
	  sio;
	  my $decoder = siproc {sdecode};
	  sforward resource_read $_[0], $decoder;
	  close $decoder;
	  $decoder->await;

# OPERATOR resource_quote

## IMPLEMENTATION
	sio; print "$_[0]\n"

# OPERATOR resource_quote_many

## IMPLEMENTATION
	sio; print "$_\n" for @_

# OPERATOR row_cols_defined

## IMPLEMENTATION
	
	  my ($floor, @cs) = @_;
	  my @pieces = ('[^\t\n]*') x $floor;
	  $pieces[$_] = '[^\t\n]+' for @cs;
	  my $r = join '\t', @pieces;
	  $r = qr/^$r/;
	  /$r/ and print while <STDIN>;

# OPERATOR row_every

## IMPLEMENTATION
	($. - 1) % $_[0] || print while <STDIN>

# OPERATOR row_fixed_scale

## IMPLEMENTATION
	
	  my $ibuf = conf 'scale/ibuf';
	  my $obuf = conf 'scale/obuf';
	
	  sub new_ref() {\(my $x = '')}
	
	  my ($n, $f) = @_;
	  conf_set monitor => 0;
	
	  my ($iqueue, $oqueue) = (64, 64);
	
	  my (@wi, @wo);
	  my ($wb, $rb, $w, $r);
	  my ($ib, $ob, $ibtmp, $obtmp);
	  for (1..$n) {
	    my ($i, $o) = sioproc {
	      setpriority 0, 0, $n >> 2;
	      &$ni::main_operator(flatten_operators $f);
	      exit;
	    };
	    push @wi, $i;
	    push @wo, $o;
	    vec($wb, fileno $i, 1) = 1;
	    vec($rb, fileno $o, 1) = 1;
	  }
	
	  vec($ib, fileno STDIN,  1) = 1;
	  vec($ob, fileno STDOUT, 1) = 1;
	
	  my $stdout_reader = siproc {
	    my @bufs;
	    my $buf_limit = $oqueue * $n;
	    my @stdout = map [], @wo;
	    my @outqueue;
	    my $b;
	    my $stdout = \*STDOUT;
	
	    close $_ for @wi;
	
	    while ($n) {
	      until (@outqueue < $oqueue * $n) {
	        safewrite $stdout, ${$b = shift @outqueue};
	        push @bufs, $b unless @bufs >= $buf_limit;
	      }
	
	      select $r = $rb, undef, undef, undef;
	      for my $i (0..$#wo) {
	        next unless defined $wo[$i];
	        next unless vec $r, fileno $wo[$i], 1;
	
	        while (@outqueue and select undef, $obtmp = $ob, undef, 0) {
	          safewrite $stdout, ${$b = shift @outqueue};
	          push @bufs, $b unless @bufs >= $buf_limit;
	        }
	        my $so = $stdout[$i];
	        if (saferead $wo[$i], ${$b = pop(@bufs) || new_ref}, $obuf) {
	          push @$so, $b;
	          my $np;
	          if (@$so >= $oqueue and 0 <= ($np = rindex $$b, "\n")) {
	            push @outqueue, @$so[0..$#{$so} - 1];
	            push @outqueue, \(my $x = substr $$b, 0, $np + 1);
	            $$b = substr $$b, $np + 1;
	            @$so = ($b);
	          }
	        } else {
	          --$n;
	          vec($rb, fileno $wo[$i], 1) = 0;
	          close $wo[$i];
	          push @outqueue, @$so;
	          $stdout[$i] = $wo[$i] = undef;
	        }
	      }
	    }
	
	    safewrite $stdout, $$_ for @outqueue;
	  };
	
	  close $stdout_reader;
	  close $_ for @wo;
	
	  {
	    my @bufs;
	    my $buf_limit = $iqueue * $n;
	    my @stdin = map [], @wi;
	    my @queue;
	    my $eof;
	    my $b;
	    my $stdin = \*STDIN;
	
	    until (!@queue && $eof) {
	      select undef, $w = $wb, undef, undef;
	      for my $i (0..$#wi) {
	        next unless vec $w, fileno $wi[$i], 1;
	
	        my $si = $stdin[$i];
	        if (@$si * 4 < $iqueue) {
	          # Commit to refilling this stdin queue, which means we need to write
	          # exclusively to this one until we find a line break.
	          push @$si, shift @queue while @$si < $iqueue and @queue;
	          while (@queue or not $eof) {
	            unless ($b = $queue[0]) {
	              last if $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, $ibuf;
	              push @queue, $b;
	            }
	
	            my $np;
	            if (0 <= ($np = rindex $$b, "\n")) {
	              push @$si, \(my $x = substr $$b, 0, $np + 1);
	              $$b = substr $$b, $np + 1;
	              last;
	            } else {
	              push @$si, shift @queue;
	            }
	          }
	        }
	
	        $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, $ibuf
	        or push @queue, $b
	          while @queue < $iqueue * $n and !$eof
	            and select $ibtmp = $ib, undef, undef, 0;
	
	        if (@$si) {
	          safewrite $wi[$i], ${$b = shift @$si};
	          push @bufs, $b unless @bufs >= $buf_limit;
	        }
	
	        $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, $ibuf
	        or push @queue, $b
	          while @queue < $iqueue * $n and !$eof
	            and select $ibtmp = $ib, undef, undef, 0;
	      }
	    }
	
	    # Run out the individual queues.
	    for my $i (0..$#wi) {
	      safewrite $wi[$i], $$_ for @{$stdin[$i]};
	      close $wi[$i];
	    }
	  }
	
	  $_->await for @wo;
	  $stdout_reader->await;

# OPERATOR row_grouped_sort

## IMPLEMENTATION
	
	  my ($key_col, $sort_cols) = @_;
	  my $key_expr = $key_col
	    ? qq{(split /\t/)[$key_col]}
	    : qq{/^([^\t\n]*)/};
	
	  my $sort_expr = join ' || ',
	    map {my $sort_op = $$_[1] =~ /[gn]/ ? '<=>' : 'cmp';
	         $$_[1] =~ /-/ ? qq{\$b[$$_[0]] $sort_op \$a[$$_[0]]}
	                       : qq{\$a[$$_[0]] $sort_op \$b[$$_[0]]}} @$sort_cols;
	
	  ni::eval gen(q{
	    my $k;
	    my @group;
	    push @group, $_ = <STDIN>;
	    ($k) = %key_expr;
	    while (<STDIN>) {
	      my ($rk) = %key_expr;
	      if ($rk ne $k) {
	        print sort {my @a = split /\t/, $a; my @b = split /\t/, $b; %sort_expr} @group;
	        @group = $_;
	        $k = $rk;
	      } else {
	        push @group, $_;
	      }
	    }
	    print sort {my @a = split /\t/, $a; my @b = split /\t/, $b; %sort_expr} @group;
	  })->(key_expr => $key_expr, sort_expr => $sort_expr);

# OPERATOR row_include_or_exclude_exact

## IMPLEMENTATION
	
	  my ($include_mode, $col, $lambda) = @_;
	  my %set;
	  my $fh = sni @$lambda;
	  chomp, ++$set{$_} while <$fh>;
	  close $fh;
	  $fh->await;
	  while (<STDIN>) {
	    chomp;
	    my @fs = split /\t/, $_, $col + 2;
	    print "$_\n" if !$include_mode == !$set{$fs[$col]};
	  }

# OPERATOR row_match

## IMPLEMENTATION
	$\ = "\n"; chomp, /$_[0]/o && print while <STDIN>

# OPERATOR row_repeat

## IMPLEMENTATION
	
	  my $col = shift;
	  while (defined (my $l = <STDIN>))
	  {
	    my $r = (split /\t/, $l, $col + 2)[$col];
	    print $l for 1..$r;
	  }

# OPERATOR row_sample

## IMPLEMENTATION
	
	  srand conf 'row/seed';
	  $. = 0;
	  while (<STDIN>) {
	    print, $. -= -log(1 - rand()) / $_[0] if $. >= 0;
	  }

# OPERATOR row_sort

## IMPLEMENTATION
	
	  exec 'sort', sort_extra_args(
	    length(conf 'row/sort-compress')
	      ? ('--compress-program=' . conf 'row/sort-compress') : (),
	    '--buffer-size=' . conf 'row/sort-buffer',
	    '--parallel='    . conf 'row/sort-parallel'), @_

# OPERATOR row_xargs_scale

## IMPLEMENTATION
	
	  my ($n, $inform, $outform, $lambda) = @_;
	  my $arg = conf 'xargs/arg';
	  my $tmp_ni = uri_path resource_tmp "file://";
	  wf $tmp_ni, image_with "quoted/$$-lambda" => json_encode $lambda;
	  chmod 0700, $tmp_ni;
	  my $cmd = shell_quote "xargs", "-P$n", "-I$arg", "sh", "-c",
	              "$tmp_ni $inform --internal/lambda$$-lambda $outform";
	  system $cmd and die "ni SX$n: $cmd failed; temporary ni in $tmp_ni";
	  unlink $tmp_ni;

# OPERATOR ruby_grepper

## IMPLEMENTATION
	stdin_to_ruby ruby_grepper $_[0]

# OPERATOR ruby_mapper

## IMPLEMENTATION
	stdin_to_ruby ruby_mapper  $_[0]

# OPERATOR safe_head

## IMPLEMENTATION
	$. <= $_[0] && print while <STDIN>

# OPERATOR scan_regex

## IMPLEMENTATION
	exec 'perl', '-lne',  'print join "\t", /' . "$_[0]/g"

# OPERATOR script

## IMPLEMENTATION
	
	  my ($lib, $cmd, @args) = @_;
	  $cmd = shell_quote $cmd, @args if @args;
	
	  my $tmpdir = export_lib_to_path $lib;
	  my $runner = siproc {
	    chdir $tmpdir;
	    sh $cmd;
	  };
	  sforward \*STDIN, $runner;
	  close $runner;
	  $runner->await;
	  rm_rf $tmpdir;

# OPERATOR sh
	Filter stream through a shell command

## IMPLEMENTATION
	my ($c) = @_; sh $c

# OPERATOR sharded_write

## IMPLEMENTATION
	
	  my ($lambda) = @_;
	  my %fhs;
	
	  while (<STDIN>)
	  {
	    my $i    = index $_, "\t";
	    next if $i == -1;
	    my $file = substr $_, 0, $i;
	    my $fh   = $fhs{$file} //= defined $lambda
	      ? siproc {exec_ni(@$lambda, file_write_op $file)}
	      : swfile $file;
	    print $fh substr $_, $i + 1;
	  }
	
	  close $_ for values %fhs;
	  $_->can('await') && $_->await for values %fhs;

# OPERATOR sink_null
	Consume stream and produce nothing

## IMPLEMENTATION
	1 while saferead \*STDIN, $_, 8192

# OPERATOR sparse_to_dense

## IMPLEMENTATION
	
	  my ($col) = @_;
	  $col ||= 0;
	  my $n = 0;
	  my @q;
	  my $row = -1;
	  while (defined($_ = @q ? shift @q : <STDIN>)) {
	    ++$row;
	    chomp;
	    my @r = split /\t/, $_, $col + 3;
	    my $k = join "\t", @r[0..$col];
	    my $kr = qr/\Q$k\E/;
	    my @fs = $col ? @r[0..$col-1] : ();
	    if ($col < @r) {
	      no warnings 'numeric';
	      ++$row, print "\n" until $row >= $r[$col];
	    }
	    matrix_cell_combine $fs[$col + $r[$col+1]], $r[$col+2];
	    matrix_cell_combine $fs[$col + $1], $2
	      while defined($_ = <STDIN>) && /^$kr\t([^\t]+)\t(.*)/;
	    push @q, $_ if defined;
	    print join("\t", map defined() ? $_ : '', @fs), "\n";
	  }

# OPERATOR split_chr

## IMPLEMENTATION
	exec 'perl', '-lnpe', $_[0] =~ /\// ? "y#$_[0]#\t#" : "y/$_[0]/\t/"

# OPERATOR split_proper_csv

## IMPLEMENTATION
	
	  while (<STDIN>)
	  {
	    $_ = ",$_";
	    $_ .= <STDIN> while 1 & (() = /"/g);
	    chomp;
	    print join("\t",
	      map { s/^"|"$//g; s/\t/        /g; y/\n/\r/; s/""/\n/g; s/"//g; y/\n/"/; $_ }
	          /\G,((?:"(?:[^"]+|"")*"|[^",]+)*)/g), "\n";
	  }

# OPERATOR split_regex

## IMPLEMENTATION
	
	  (my $quoted = shift) =~ s/([\$\@])/\\$1/g;
	  my $r = qr/$quoted/;
	  exec 'perl', '-lnpe', "s/$r/\$1\t/g";

# OPERATOR sql_preview

## IMPLEMENTATION
	sio; print "$_[0]\n"

# OPERATOR ssh

## IMPLEMENTATION
	
	  my ($host, $lambda) = @_;
	  my $ssh_pipe = siproc {exec 'ssh', @$host, shell_quote ni_quoted_exec_args};
	  quote_ni_into $ssh_pipe, @$lambda;

# OPERATOR stderr_monitor

## IMPLEMENTATION
	
	  BEGIN {eval {require Time::HiRes; Time::HiRes->import('time')}}
	  my ($monitor_id, $monitor_name, $update_rate) = (@_, 1);
	  my ($itime, $otime, $bytes) = (0, 0, 0);
	  my $last_update = 0;
	  my $start_time  = 0;
	  my $width       = $ENV{COLUMNS} || 80;
	  my $runtime     = 1;
	  my $preview     = "";
	  my $factor_log  = 0;
	  my ($stdin, $stdout) = (\*STDIN, \*STDOUT);
	
	  my $monitor_start = conf 'monitor/start';
	
	  while (1) {
	    my $t1 = time; $bytes += my $n = saferead $stdin, $_, 65536;
	                   last unless $n;
	    my $t2 = time; safewrite_exactly $stdout, $_;
	    my $t3 = time;
	
	    # Start the clocks only once some data starts moving; we ignore the initial
	    # read/write warmup
	    if ($start_time)
	    {
	      $itime += $t2 - $t1;
	      $otime += $t3 - $t2;
	    }
	    else
	    {
	      $start_time = $t2;
	    }
	
	    if ($t3 - $last_update > $update_rate && $t3 - $start_time > $monitor_start) {
	      $last_update = $t3;
	      $runtime = $t3 - $start_time || 1;
	      if ($t3 & 3 && /\n(.*)\n/) {
	        ($preview = substr $1, 0, $width - 20) =~ s/\t/  /g;
	        $preview =~ s/[[:cntrl:]]/./g;
	        $preview = substr $preview, 0, $width - 20;
	      } else {
	        $preview = substr $monitor_name, 0, $width - 20;
	      }
	
	      $factor_log = log(($otime || 1) / ($itime || 1)) / log 2;
	
	      safewrite \*STDERR,
	        sprintf "\033[s\033[%d;1H%d \r\033[K%5d%s %5d%s/s% 4d %s\n\033[u",
	          $monitor_id + 1,
	          int($t3),
	          unit_bytes $bytes,
	          unit_bytes $bytes / $runtime,
	          $factor_log * 10,
	          $preview;
	    }
	  }
	
	  # Indicate EOF by dropping = into whitespaces.
	  if (time() - $start_time > $monitor_start)
	  {
	    safewrite \*STDERR,
	      sprintf "\033[s\033[%d;1H%d=\r\033[K%5d%s=%5d%s/s% 4d=%s\n\033[u",
	        $monitor_id + 1,
	        int($last_update),
	        unit_bytes $bytes,
	        unit_bytes $bytes / $runtime,
	        $factor_log * 10,
	        substr $monitor_name, 0, $width - 20;
	  }

# OPERATOR stream_to_gnuplot

## IMPLEMENTATION
	
	  my ($col, $command) = @_;
	  exec 'gnuplot', '-e', $command unless defined $col;
	  my ($k, $fh) = (undef, undef);
	  while (<STDIN>) {
	    chomp;
	    my @fs = split /\t/, $_, $col + 2;
	    my $rk = join "\t", @fs[0..$col];
	    if (!defined $k or $k ne $rk) {
	      if (defined $fh) {
	        close $fh;
	        $fh->await;
	      }
	      $k  = $rk;
	      $fh = siproc {exec 'gnuplot', '-e', "KEY='$k';$command"};
	    }
	    print $fh join("\t", @fs[$col+1..$#fs]) . "\n";
	  }

# OPERATOR tail

## IMPLEMENTATION
	exec 'tail', $_[0], join "", @_[1..$#_]

# OPERATOR unflatten

## IMPLEMENTATION
	
	  my ($n_cols) = @_;
	  my @row = ();
	  while(<STDIN>) {
	    chomp;
	    push @row, split /\t/, $_;
	    while(@row >= $n_cols) {
	      my @emit_vals = splice(@row, 0, $n_cols);
	      print(join("\t", @emit_vals). "\n"); 
	      }
	    }
	  if (@row > 0) {
	    while(@row > 0) {
	      my @emit_vals = splice(@row, 0, $n_cols);
	      print(join("\t", @emit_vals). "\n");
	    }
	  }

# OPERATOR uniq

## IMPLEMENTATION
	exec 'uniq'

# OPERATOR unordered_count

## IMPLEMENTATION
	
	  my %h;
	  chomp, ++$h{$_} while <STDIN>;
	  print "$h{$_}\t$_\n" for sort keys %h;

# OPERATOR vertical_apply

## IMPLEMENTATION
	
	  my ($colspec, $lambda) = @_;
	  my ($limit, @cols) = @$colspec;
	  my ($i, $o) = sioproc {exec ni_quoted_exec_args};
	  safewrite $i, ni_quoted_image 0, @$lambda;
	
	  vec(my $rbits = '', fileno $o, 1) = 1;
	  vec(my $wbits = '', fileno $i, 1) = 1;
	  fh_nonblock $i;
	
	  my $read_buf = '';
	  my $write_buf = '';
	  my @queued;
	  my @awaiting_completion;
	  my $stdin_ok = my $proc_ok = 1;
	  while ($stdin_ok || $proc_ok) {
	    my $l = sum map length, @queued;
	    $_ = '';
	    chomp, push @queued, $_ while ($l += length) <= 1048576
	                              and $stdin_ok &&= defined($_ = <STDIN>);
	
	    while (@queued && sum(map length, @awaiting_completion) < 1048576
	                   && select undef, my $wout=$wbits, undef, 0) {
	      my $n = 0;
	      my @chopped;
	      push @chopped, join "\t", (split /\t/, $queued[$n++], $limit)[@cols]
	        while $n < @queued && 8192 > sum map 1 + length, @chopped;
	      ++$n unless $n;
	      push @awaiting_completion, @queued[0..$n-1];
	      @queued = @queued[$n..$#queued];
	      my $s  = $write_buf . join '', map "$_\n", @chopped;
	      my $sn = safewrite $i, $s;
	      $write_buf = substr $s, $sn;
	    }
	
	    close $i if !@queued && !$stdin_ok;
	
	    $proc_ok &&= saferead $o, $read_buf, 8192, length $read_buf
	      while $proc_ok && select my $rout=$rbits, undef, undef, 0;
	
	    my @lines = split /\n/, $read_buf . " ";
	    $proc_ok ? $read_buf = substr pop(@lines), 0, -1 : pop @lines;
	    for (@lines) {
	      die "ni: vertical apply's process emitted too many lines: $_"
	        unless @awaiting_completion;
	      my @fs = split /\t/, shift @awaiting_completion;
	      @fs[@cols] = my @cs = split /\t/;
	      print join("\t", @fs, @cs[@fs..$#cs]), "\n";
	    }
	  }
	
	  die "ni: vertical apply's process ultimately lost "
	    . scalar(@awaiting_completion) . " line(s)"
	  if @awaiting_completion;
	
	  close $o;
	  $o->await;

# OPERATOR wc_l

## IMPLEMENTATION
	sh 'wc -l'

# OPERATOR with_left

## IMPLEMENTATION
	
	  my $fh = sni @_;
	  my $l;
	  while (<STDIN>) {
	    return unless defined($l = <$fh>);
	    chomp $l;
	    print "$l\t$_";
	  }

# OPERATOR with_right

## IMPLEMENTATION
	
	  my $fh = sni @_;
	  my $l;
	  while (<STDIN>) {
	    chomp;
	    return unless defined($l = <$fh>);
	    print "$_\t$l";
	  }
