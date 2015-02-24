#!/usr/bin/env perl
eval($ni::selfcode = join '', <DATA>); die $@ if $@;
__DATA__
{
use v5.14;
no strict 'refs';
package ni;
sub ni;
sub ::ni;
sub self {
  join "\n", "#!/usr/bin/env perl",
             q{eval($ni::selfcode = join '', <DATA>); die $@ if $@;},
             "__DATA__",
             $ni::selfcode;
}
use POSIX qw/:sys_wait_h/;
$SIG{CHLD} = sub {
  local ($!, $?);
  waitpid -1, WNOHANG;
};
our %compiled_functions;
sub expand_function_shorthands {
  my ($code) = @_;
  $code =~ s/%(\d+)/\$_[$1]/g;
  1 while $code =~ s/([a-zA-Z0-9_\)\}\]\?\$])
                     \.
                     ([\$_a-zA-Z](?:-[0-9\w\?\$]|[0-9_\w?\$])*)
                    /$1\->{'$2'}/x;
  $code;
}
sub compile {
  return $_[0] if ref $_[0] eq 'CODE';
  return $compiled_functions{$_[0]}
     //= eval "package main; sub {\n" . expand_function_shorthands($_[0])
                                      . "\n}";
}
BEGIN {
sub ni::gen::new;
sub gen       { local $_; ni::gen->new(@_) }
sub gen_empty { gen('empty', {}, '') }
sub gen_seq {
  my ($name, @statements) = @_;
  my $code_template = join "\n", map "%\@x$_;", 0 .. $#statements;
  my %subst;
  $subst{"x$_"} = $statements[$_] for 0 .. $#statements;
  gen $name, {%subst}, $code_template;
}
{
package ni::gen;
use overload qw# %  subst  * map  @{} inserted_code_keys  "" compile
                 eq compile_equals #;
our $gensym_id  = 0;
our $randomness = join '', map sprintf("%04x", rand(65536)), 0..3;
sub gensym { '$' . ($_[0] // '') . "_${randomness}_" . $gensym_id++ }
sub parse_code;
sub new {
  my ($class, $sig, $refs, $code) = @_;
  my ($fragments, $gensyms, $gensym_indexes, $insertions) = parse_code $code;
  my %subst;
  for (keys %$refs) {
    if (exists $$insertions{$_}) {
      $subst{$_} = $$refs{$_};
      delete $$refs{$_};
    }
  }
  exists $$gensyms{$_} or die "undefined ref $_ in $code" for keys %$refs;
  exists $$refs{$_}    or die    "unused ref $_ in $code" for keys %$gensyms;
  bless({ sig               => $sig,
          fragments         => $fragments,
          gensym_names      => $gensyms,
          gensym_indexes    => $gensym_indexes,
          insertion_indexes => $insertions,
          inserted_code     => {},
          refs              => $refs // {} },
        $class) % {%subst};
}
sub genify {
  return $_[0] if ref $_[0] && $_[0]->isa('ni::gen');
  return ni::gen('genified', {}, $_[0]);
}
sub compile_equals {
  my ($self, $x) = @_;
  $x = $x->compile if ref $x;
  $self->compile eq $x;
}
sub share_gensyms_with {
  my ($self, $g) = @_;
  for (keys %{$$g{gensym_names}}) {
    if (exists $$self{gensym_names}{$_}) {
      $$g{gensym_names}{$_} = $$self{gensym_names}{$_};
      ${$$g{fragments}}[$$g{gensym_indexes}{$_}] =
        ${$$self{fragments}}[$$self{gensym_indexes}{$_}];
    }
  }
  $self;
}
sub inherit_gensyms_from {
  $_[1]->share_gensyms_with($_[0]);
  $_[0];
}
sub build_ref_hash {
  my ($self, $refs) = @_;
  $refs //= {};
  $$refs{$$self{gensym_names}{$_}} = $$self{refs}{$_} for keys %{$$self{refs}};
  $$self{inserted_code}{$_}->build_ref_hash($refs) for @$self;
  $refs;
}
sub inserted_code_keys {
  my ($self) = @_;
  [sort keys %{$$self{inserted_code}}];
}
sub subst {
  my ($self, $vars) = @_;
  for my $k (keys %$vars) {
    die "unknown subst var: $k (code is $self)"
      unless defined(my $i = $$self{insertion_indexes}{$k});
    $$self{inserted_code}{$k} = $$self{fragments}[$i] = genify $$vars{$k};
  }
  $self;
}
sub map {
  my ($self, $f) = @_;
  $f = ni::compile $f;
  my $y = &$f($self);
  return $y unless $y eq $self;
  my $new = bless {}, ref $self;
  $$new{$_} = $$self{$_} for keys %$self;
  $$new{fragments}     = [@{$$new{fragments}}];
  $$new{inserted_code} = {%{$$new{inserted_code}}};
  $new % {map {$_, $$new{inserted_code}{$_} * $f} @$new};
}
sub compile {
  my ($self) = @_;
  ref $_ eq 'ARRAY' && die "cannot compile underdetermined gen $self"
    for @{$$self{fragments}};
  join '', @{$$self{fragments}};
}
sub run {
  my ($self) = @_;
  my $code     = $self->compile;
  my $refs     = $self->build_ref_hash;
  my $bindings = join "\n", map sprintf("my %s = \$_[0]->{'%s'};", $_, $_),
                                keys %$refs;
  my $f        = eval($code = "package main; sub {\n$bindings\n$code\n}");
  die "$@ compiling\n$code" if $@;
  my @result = &$f($refs);
  delete $$refs{$_} for keys %$refs;    # we create circular refs sometimes
  @result;
}
our %parsed_code_cache;
sub parse_code {
  my ($code) = @_;
  my $cached;
  unless (defined($cached = $parsed_code_cache{$code})) {
    my @pieces = split /(\%:\w+|\%\@\w+)/, $code;
    my @fragments;
    my %gensym_indexes;
    my %insertion_points;
    for (0..$#pieces) {
      if ($pieces[$_] =~ /^\%:(\w+)$/) {
        $gensym_indexes{$1} = $_;
        push @fragments, undef;
      } elsif ($pieces[$_] =~ /^\%\@(\w+)$/) {
        $insertion_points{$1} = $_;
        push @fragments, [$1];
      } else {
        push @fragments, $pieces[$_];
      }
    }
    $cached = $parsed_code_cache{$code} = [[@fragments],
                                           {%gensym_indexes},
                                           {%insertion_points}];
  }
  my ($fragments, $gensym_indexes, $insertion_points) = @$cached;
  my $new_fragments = [@$fragments];
  my $gensym_names  = {};
  $$new_fragments[$$gensym_indexes{$_}] = $$gensym_names{$_} = gensym $_
    for keys %$gensym_indexes;
  ($new_fragments, $gensym_names, $gensym_indexes, $insertion_points);
}
}
}
our %io_constructors;
sub is_io { ref $_[0] && $_[0]->isa('ni::io') }
sub defio {
  my ($name, $constructor, $methods) = @_;
  *{"ni::io::${name}::new"} = $io_constructors{$name} = sub {
    my ($class, @args) = @_;
    bless $constructor->(@args), $class;
  };
  *{"::ni_$name"} = *{"ni::ni_$name"} =
    sub { ${"ni::io::${name}::"}{new}("ni::io::$name", @_) };
  *{"ni::io::$name::$_"} = $methods->{$_} for keys %$methods;
  push @{"ni::io::${name}::ISA"}, 'ni::io';
}
sub defioproxy {
  my ($name, $f) = @_;
  *{"::ni_$name"} = *{"ni::ni_$name"} = $f;
}
our %sig_conversions = (
  FL => q{ %@before chomp @_; $_ = join("\t", @_) . "\n"; %@after },
  FR => q{ %@before           $_ = join("\t", @_);        %@after },
  LF => q{ %@before chomp;    @_ = split /\t/;            %@after },
  LR => q{ %@before chomp;                                %@after },
  RF => q{ %@before           @_ = split /\t/;            %@after },
  RL => q{ %@before           $_ .= "\n" unless /\n$/;    %@after },
  FF => q{ %@before %@after },
  LL => q{ %@before %@after },
  RR => q{ %@before %@after },
);
$sig_conversions{$_} = gen("conv:$_", {}, $sig_conversions{$_})
  for keys %sig_conversions;
sub input_sig  { (${$_[0]}{sig} =~ /:(\w)\w$/)[0] }
sub output_sig { (${$_[0]}{sig} =~ /:\w(\w)$/)[0] }
sub with_input_type {
  my ($sig, $code) = @_;
  return $code unless $sig;
  die "unsigned code block: $code ($$code{sig})"
    unless my $codesig = input_sig $code;
  die "unknown code transform $sig$codesig for $sig > $$code{sig} ($codesig)"
    unless defined(my $transform = $sig_conversions{"$sig$codesig"});
  $transform % {before => gen_empty, after => $code};
}
sub with_output_type {
  my ($sig, $code) = @_;
  return $code unless $sig;
  die "unsigned code block: $code ($$code{sig})"
    unless my $codesig = output_sig $code;
  die "unknown code transform $sig$codesig for $$code{sig} ($codesig) > $sig"
    unless defined(my $transform = $sig_conversions{"$codesig$sig"});
  $transform % {before => $code, after => gen_empty};
}
sub mapone_binding;
sub flatmap_binding;
sub reduce_binding;
sub grep_binding;
sub pipe_binding;
{
package ni::io;
use overload qw# + plus_op  * mapone_op  / reduce_op  % grep_op  | pipe_op
                 >>= bind_op
                 > into  >= into_bg
                 < from  <= from_bg #;
BEGIN { *gen = \&ni::gen }
use POSIX qw/dup2/;
sub source_gen { ... }          # gen to source from this thing
sub sink_gen   { ... }          # gen to sink into this thing
sub transform {
  my ($self, $f) = @_;
  $f->($self);
}
sub reader_fh { (::ni_pipe() <= $_[0])->reader_fh }
sub writer_fh { (::ni_pipe() >= $_[0])->writer_fh }
sub has_reader_fh { 0 }
sub has_writer_fh { 0 }
sub supports_reads  { 1 }
sub supports_writes { 0 }
sub flatten { ($_[0]) }
sub close   { $_[0] }
sub plus_op   { $_[0]->plus($_[1]) }
sub bind_op   { $_[0]->bind($_[1]) }
sub mapone_op { $_[0]->mapone($_[1]) }
sub reduce_op { $_[0]->reduce($_[1], {}) }
sub grep_op   { $_[0]->grep($_[1]) }
sub pipe_op   { $_[0]->pipe($_[1]) }
sub plus    { ::ni_sum(@_) }
sub bind    { ::ni_bind(@_) }
sub mapone  { $_[0] >>= ni::mapone_binding  @_[1..$#_] }
sub flatmap { $_[0] >>= ni::flatmap_binding @_[1..$#_] }
sub reduce  { $_[0] >>= ni::reduce_binding  @_[1..$#_] }
sub grep    { $_[0] >>= ni::grep_binding    @_[1..$#_] }
sub pipe    { ::ni_process($_[1], $_[0], undef) }
sub from {
  my ($self, $source) = @_;
  ::ni($source)->source_gen($self)->run;
  $self;
}
sub from_bg {
  my ($self, $source) = @_;
  $self < $source, exit unless fork;
  $self;
}
sub into {
  my ($self, $dest) = @_;
  $self->source_gen(::ni $dest)->run;
  $self;
}
sub into_bg {
  my ($self, $dest) = @_;
  $self > $dest, exit unless fork;
  $self;
}
}
BEGIN {
  our @data_names;
  our %data_matchers;
  our %data_transformers;
  sub defdata {
    my ($name, $matcher, $transfomer) = @_;
    die "data type $name is already defined" if exists $data_matchers{$name};
    unshift @data_names, $name;
    $data_matchers{$name}     = $matcher;
    $data_transformers{$name} = $transfomer;
  }
  sub ni_io_for {
    my ($f, @args) = @_;
    for my $n (@data_names) {
      return $data_transformers{$n}->($f, @args)
        if $data_matchers{$n}->($f, @args);
    }
    die "$f does not match any known ni::io constructor";
  }
  sub ::ni {
    my ($f, @args) = @_;
    return undef unless defined $f;
    return $f if ref $f && $f->isa('ni::io');
    return ni_io_for($f, @args);
  }
  *{"ni::ni"} = *{"::ni"};
}
defdata 'file', sub { -e $_[0] || $_[0] =~ s/^file:// },
                sub { ni_file("< $_[0]", "> $_[0]") };
sub deffilter {
  my ($extension, $read, $write) = @_;
  $extension = qr/\.$extension$/;
  defdata $extension,
    sub { $_[0] =~ /$extension/ },
    sub { ni_filter(ni_file("< $_[0]", "> $_[0]"), $read, $write) };
}
deffilter 'gz',  'gzip -d',  'gzip';
deffilter 'lzo', 'lzop -d',  'lzop';
deffilter 'xz',  'xz -d',    'xz';
deffilter 'bz2', 'bzip2 -d', 'bzip2';
defdata 'ssh',
  sub { $_[0] =~ /^\w*@[^:\/]+:/ },
  sub { $_[0] =~ /^([^:@]+)@([^:]+):(.*)$/;
        my ($user, $host, $file) = ($1, $2, $3);
        };
defdata 'globfile', sub { ref $_[0] eq 'GLOB' },
                    sub { ni_file($_[0], $_[0]) };
BEGIN {
use POSIX qw/dup2/;
sub to_fh {
  return undef unless defined $_[0];
  return $_[0]->() if ref $_[0] eq 'CODE';
  return $_[0]     if ref $_[0] eq 'GLOB';
  open my $fh, $_[0] or die "failed to open $_[0]: $!";
  $fh;
}
defio 'sink_as',
sub { \$_[0] },
{
  supports_reads  => sub { 0 },
  supports_writes => sub { 1 },
  sink_gen        => sub { ${$_[0]}->(@_[1..$#_]) },
};
defio 'source_as',
sub { \$_[0] },
{
  source_gen => sub { ${$_[0]}->(@_[1..$#_]) },
};
sub sink_as(&)   { ni_sink_as(@_) }
sub source_as(&) { ni_source_as(@_) }
defio 'file',
sub { +{reader => $_[0], writer => $_[1]} },
{
  reader_fh => sub {
    my ($self) = @_;
    die "io not configured for reading" unless $self->supports_reads;
    to_fh $$self{reader};
  },
  writer_fh => sub {
    my ($self) = @_;
    die "io not configured for writing" unless $self->supports_writes;
    to_fh $$self{writer};
  },
  supports_reads  => sub { defined ${$_[0]}{reader} },
  supports_writes => sub { defined ${$_[0]}{writer} },
  has_reader_fh   => sub { ${$_[0]}->supports_reads },
  has_writer_fh   => sub { ${$_[0]}->supports_writes },
  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'file_source:VV', {fh   => $self->reader_fh,
                           body => $destination->sink_gen('L')},
      q{ while (<%:fh>) {
           %@body
         } };
  },
  sink_gen => sub {
    my ($self, $type) = @_;
    with_input_type $type,
      gen 'file_sink:LV', {fh => $self->writer_fh}, q{ print %:fh $_; };
  },
  close => sub { close $_[0]->writer_fh; $_[0] },
};
defio 'memory',
sub { [@_] },
{
  supports_writes => sub { 1 },
  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'memory_source:VV', {xs   => $self,
                             body => $destination->sink_gen('L')},
      q{ for (@{%:xs}) {
           %@body
         } };
  },
  sink_gen => sub {
    my ($self, $type) = @_;
    $type eq 'F' ? gen 'memory_sink:FV', {xs => $self},
                       q{ push @{%:xs}, [@_]; }
                 : gen "memory_sink:${type}V",
                       {xs => $self},
                       q{ push @{%:xs}, $_; };
  },
};
defio 'sum',
sub { [map $_->flatten, @_] },
{
  transform  => sub {
    my ($self, $f) = @_;
    my $x = $f->($self);
    $x eq $self ? ni_sum(map $_->transform($f), @$self)
                : $x;
  },
  flatten    => sub { @{$_[0]} },
  source_gen => sub {
    my ($self, $destination) = @_;
    return gen 'empty', {}, '' unless @$self;
    gen_seq 'sum_source:VV', map $_->source_gen($destination), @$self;
  },
};
defio 'cat',
sub { \$_[0] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self->source_gen(sink_as {
      gen 'cat_source:VV',
          {dest => $destination},
          q{ $_->source_gen(%:dest)->run; }});
  },
};
defio 'bind',
sub { +{ base => $_[0], code_transform => $_[1] } },
{
  supports_reads  => sub { ${$_[0]}{base}->supports_reads },
  supports_writes => sub { ${$_[0]}{base}->supports_writes },
  transform => sub {
    my ($self, $f) = @_;
    my $x = $f->($self);
    $x eq $self ? ni_bind($$self{base}->transform($f), $$self{code_transform})
                : $x;
  },
  sink_gen => sub {
    my ($self, $type) = @_;
    $$self{code_transform}->($$self{base}, $type);
  },
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self{base}->source_gen(sink_as {
      my ($type) = @_;
      $$self{code_transform}->($destination, $type);
    });
  },
  close => sub { ${$_[0]}{base}->close; $_[0] },
};
defioproxy 'pipe', sub {
  pipe my $out, my $in or die "pipe failed: $!";
  ni_file($out, $in);
};
defioproxy 'process', sub {
  my ($command, $stdin, $stdout) = @_;
  $stdin  = defined $stdin  ? defined $stdin->reader_fh ? $stdin
                                                        : ni_pipe() <= $stdin
                            : ni_pipe();
  $stdout = defined $stdout ? defined $stdout->reader_fh ? $stdout
                                                         : ni_pipe() >= $stdout
                            : ni_pipe();
  unless (fork) {
    close STDIN;
    close STDOUT;
    close $stdin->writer_fh;
    close $stdout->reader_fh;
    dup2 fileno $stdin->reader_fh,  0 or die "dup2 failed: $!";
    dup2 fileno $stdout->writer_fh, 1 or die "dup2 failed: $!";
    exec $command or exit;
  }
  ni_file($stdout->reader_fh, $stdin->writer_fh);
};
defioproxy 'filter', sub {
  my ($base, $read_filter, $write_filter) = @_;
  ni_file(
    $base->supports_reads && defined $read_filter
      ? sub {ni_process($read_filter, $base->reader_fh, undef)->reader_fh}
      : undef,
    $base->supports_writes && defined $write_filter
      ? sub {ni_process($write_filter, undef, $base->writer_fh)->writer_fh}
      : undef);
};
}
sub invocation {
  my ($f, @args) = @_;
  if (@args || ref $f eq 'CODE' || $f =~ s/^;//) {
    @args
      ? gen('fn:FF', {f => compile($f), args => [@args]},
            q{ %:f->(@_, @{%:args}) })
      : gen('fn:FF', {f => compile($f)}, q{ %:f->(@_) });
  } else {
    gen('fn:FF', {f => $f}, q{ (%@f) });
  }
}
sub flatmap_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen "flatmap:${is}V", {invocation => $i,
                             body       => $into->sink_gen('R')},
        q{ for (%@invocation) {
             %@body
           } };
  };
}
sub mapone_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen "mapone:${is}V", {invocation => $i,
                            body       => $into->sink_gen('F')},
        q{ if (@_ = %@invocation) {
             %@body
           } };
  };
}
sub grep_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen "grep:${is}V", {invocation => $i,
                          body       => $into->sink_gen($is)},
        q{ if (%@invocation) {
             %@body
           } };
  };
}
sub reduce_binding {
  my ($f, $init) = @_;
  $f = compile $f;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen 'reduce:FV', {f    => $f,
                        init => $init,
                        body => $into->sink_gen('R')},
        q{ (%:init, @_) = %:f->(%:init, @_);
           for (@_) {
             %@body
           } };
  };
}
sub tee_binding {
  my ($tee) = @_;
  sub {
    my ($into, $type) = @_;
    my $init    = gen 'tee_init', {x => []},
                      $type eq 'F' ? q{ @{%:x} = @_ } : q{ %:x = $_ };
    my $recover = gen('tee_recover', {x => []},
                      $type eq 'F' ? q{ @_ = @{%:x} } : q{ $_ = %:x })
                  ->inherit_gensyms_from($init);
    gen_seq "tee:${type}V", $init,    $tee->sink_gen($type),
                            $recover, $into->sink_gen($type);
  };
}
our %op_shorthand_lookups;      # keyed by short
our %op_shorthands;             # keyed by long
our %op_formats;                # ditto
our %op_usage;                  # ditto
our %op_fns;                    # ditto
sub defop {
  my ($long, $short, $format, $usage, $fn) = @_;
  if (defined $short) {
    $op_shorthands{$long}         = $short;
    $op_shorthand_lookups{$short} = "--$long";
  }
  $op_formats{$long} = $format;
  $op_usage{$long}   = $usage;
  $op_fns{$long}     = $fn;
  die "operator $long already exists (possibly as a method rather than an op)"
    if exists $ni::io::{$long};
  *{"ni::io::$long"} = $fn;     # programmatic access
}
our %format_matchers = (
  a => qr/^[a-zA-Z]+$/,
  d => qr/^[-+\.0-9]+$/,
  s => qr/^.*$/,
  v => qr/^[^-].*$/,
);
sub apply_format {
  my ($format, @args) = @_;
  my @format = split //, $format;
  my @parsed;
  for (@format) {
    die "too few arguments for $format" if !@args && !/[A-Z]/;
    my $a = shift @args;
    if ($a =~ /$format_matchers{lc $_}/) {
      push @parsed, $a;
    } else {
      die "failed to match format $format" unless /[A-Z]/;
      push @parsed, undef;
    }
  }
  [@parsed], @args;
}
sub file_opt { ['plus', ni $_[0]] }
sub parse_commands {
  my @parsed;
  for (my $o; defined($o = shift @_);) {
    return @parsed, map file_opt($_), @_ if $o eq '--';
    if ($o =~ /^--/) {
      my $c = $o =~ s/^--//r;
      die "unknown long command: $o" unless exists $op_fns{$c};
      my ($args, @rest) = apply_format $op_formats{$c}, @_;
      push @parsed, [$c, @$args];
      @_ = @rest;
    } elsif ($o =~ s/^-//) {
      unshift @_, map $op_shorthand_lookups{$_} // $_,
                      $o =~ /([:+^=%\/]?[a-zA-Z]|[-+\.0-9]+)/g;
    } else {
      push @parsed, file_opt $o;
    }
  }
  @parsed;
}
use File::Temp qw/tmpnam/;
defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni_memory(self) };
defop 'tee', undef, 's',
  'tees current output into the specified io',
  sub { $_[0] >>= tee_binding(ni $_[1]) };
defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * $_[1] };
defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % $_[1] };
defop 'deref', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni_cat($_[0] * \&ni) };
defop 'ref', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my $f = $_[1] // tmpnam;
        $_[0]->into(ni $_[1]);
        ni_memory($f) };
defop 'branch', 'b', 's',
  'splits input by its first field, forwarding to subprocesses',
  sub {
    my ($in, $subprocesses) = @_;
    my @subs = unpack_branch_map $subprocesses;
    my $fifo = ni::io::fifo->new->from(map ${$_}[1], @subs);
    unless (fork) {
      my $line;
      while (defined($line = <$in>)) {
        my ($k, $v) = split /\t/, $line, 2;
        for my $s (@subs) {
          if ($s->[0]->($k)) {
            $s->[1]->enqueue($line);
            last;
          }
        }
      }
      exit;
    }
    $fifo;
  };
sub sort_options {
  my @fieldspec = split //, $_[0] // '';
}
defop 'order', 'o', 'AD',
  'order {n|N|g|G|l|L|u|U|m} [fields]',
  sub {
    my ($in, $flags, $fields) = @_;
    $in | 'sort';
  };
use POSIX qw/dup2/;
sub preprocess_cli {
  my @preprocessed;
  for (my $o; defined($o = shift @_);) {
    if ($o =~ s/\[$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\]$/;
        $depth += /\[$/;
        push @xs, $_;
      }
      push @preprocessed, bless [@xs], $o;
    } elsif ($o =~ s/\{$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\}$/;
        $depth += /\{$/;
        push @xs, $_;
      }
      push @preprocessed, bless {@xs}, $o;
    } else {
      push @preprocessed, $o;
    }
  }
  @preprocessed;
}
sub stream_for {
  my ($stream, @options) = @_;
  $stream //= -t STDIN ? ni_sum() : ni_file(\*STDIN);
  for (parse_commands @options) {
    my ($command, @args) = @$_;
    $stream = $ni::io::{$command}($stream, @args);
  }
  $stream;
}
sub stream_to_process {
  my ($stream, @process_alternatives) = @_;
  close STDIN;
  dup2 0, fileno $stream->reader_fh or die "dup2 failed: $!";
  exec $_ for @process_alternatives;
}
sub main {
  $|++;
  my $data = stream_for undef, preprocess_cli @_;
  if (-t STDOUT && !exists $ENV{NI_NO_PAGER}) {
    stream_to_process $data, $ENV{NI_PAGER} // $ENV{PAGER} // 'less',
                             'more';
    print STDERR "ni: couldn't exec any pagers, writing to the terminal\n";
    print STDERR "ni: (sorry about this; if you set \$PAGER it should work)\n";
    print STDERR "\n";
    print while <>;
  } else {
    $data > \*STDOUT;
  }
}
END { main @ARGV }
}
