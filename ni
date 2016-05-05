#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.000_000;
$ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1
  while <DATA> =~ /^(\d+)\s+(.*)$/;
eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
close DATA;
__DATA__
8 ni
#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.000_000;
$ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1
  while <DATA> =~ /^(\d+)\s+(.*)$/;
eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
close DATA;
__DATA__
77 cli.pl

package ni;



use constant end_of_argv  => sub {@_           ? () : (0)};
use constant consumed_opt => sub {length $_[0] ? () : @_};
sub seqr(\@) {my ($ps) = @_;
         sub {my ($x, @xs, @ys, @ps);
              (($x, @_) = &$_(@_)) ? push @xs, $x : return () for @ps = @$ps;
              (\@xs, @_)}}
sub altr(\@) {my ($ps) = @_;
         sub {my @ps, @r; @r = &$_(@_) and return @r for @ps = @$ps; ()}}
sub seq {seqr @_}
sub alt {altr @_}
sub rep {my ($p, $n) = (@_, 0);
    sub {my (@c, @r);
         push @r, $_ while ($_, @_) = &$p(@c = @_);
         @r >= $n ? (\@r, @c) : ()}}
sub maybe($) {my ($p) = @_;
         sub {my @xs = &$p(@_); @xs ? @xs : (undef, @_)}};
sub pmap(&$) {my ($f, $p) = @_;
         sub {my @xs = &$p(@_); @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()}}
sub pif(&$) {my ($f, $p) = @_;
        sub {my @xs = &$p(@_); @xs && &$f($_ = $xs[0]) ? @xs : ()}}



sub mr($) {my $r = qr/$_[0]/;
      sub {my ($x, @xs) = @_; $x =~ s/($r)/$2/ ? ($1, $x, @xs) : ()}}
sub mrc($) {pmap {$$_[0]} seq mr $_[0], maybe consumed_opt}


sub chaltr(\%) {my ($ps) = @_;
           sub {my ($x, @xs, $c, @ys) = @_;
                return () unless $x =~ s/^(.)// && exists $$ps{$c = $1};
                (@ys = $$ps{$c}($x, @xs)) ? ([$c, $ys[0]], @ys[1..$#ys]) : ()}}
sub chalt {my %h = @_; chaltr %h}

use constant rbcode => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs, $x, $qcode) = @_;
  ($qcode = $code) =~ s/'/'\\''/g;
  $x .= ']' while $_ = system("ruby -ce '$qcode' >/dev/null 2>&1")
                  and ($qcode =~ s/\]$//, $code =~ s/\]$//);
  $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};

use constant plcode => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs, $x, $qcode) = @_;
  ($qcode = $code) =~ s/'/'\\''/g;
  my $begin_warning = $qcode =~ s/BEGIN/END/g;
  $x .= ']' while $_ = system("perl -ce '$qcode' >/dev/null 2>&1")
                  and ($qcode =~ s/\]$//, $code =~ s/\]$//);
  print STDERR <<EOF if $_ && $begin_warning;
ni: failed to get closing bracket count for perl code "$_[0]", possibly
    because BEGIN-block metaprogramming is disabled when ni tries to figure
    this out.
    https://github.com/spencertipping/ni/tree/master/design/cli-reader-problem.md
EOF
  $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};



our %operators;
our @quasifiles;

sub ops() {sub {ops()->(@_)}}


use constant short => chalt %operators;
use constant list  => pmap {$$_[1]} seq mrc '^\[', ops, mrc '^\]';
use constant op    => pmap {$$_[1]} seq maybe consumed_opt,
                                        alt(altr @quasifiles, list, short),
                                        maybe consumed_opt;
use constant ops   => rep op;
use constant cli   => pmap {$$_[0]} seq ops, end_of_argv;
47 sh.pl




package ni;



sub shell_quote;
sub shell_quote {
  my ($c) = @_;
  return join ' ', map shell_quote $_, @$c if ref $c;
  return $c                                if $c =~ /[^-A-Za-z_0-9\/]/;
  $c =~ s/'/'\\''/g;
  "'$c'";
}
sub heredoc_terminator {
  my $id = 0;
  ++$id while $_[1] =~ /^_$_[0]_$id$/m;
  "_$_[0]_$id";
}
sub compile_pipeline {
  my %file_inits;
  my @commands;
  my @heredocs;
  for my $i (0..$#_) {
    my $c = $_[$i];
    my $redirections;
    if (exists $c->{stdin}) {
      my $h = heredoc_terminator $i, $c->{stdin};
      push @heredocs, "$c->{stdin}\n$h";
      $redirections = "3<&0 <<\'$h\'";
    }
    $file_inits{$_} = $c->{files}{$_} for keys %{$c->{files}};
    my @env = map "$_=" . shell_quote($c->{env}{$_}), keys %{$c->{env}};
    push @commands, join " ", @env, shell_quote($c->{exec}), $redirections;
  }
  my $mkdirs = 'mkdir -p '
             . shell_quote [map /^(.*)\/[^\/]*/, keys %file_inits];
  my $cats   = join "\n", map {
                 my $h = heredoc_terminator '', $file_inits{$_};
                 "cat > " . shell_quote($_) . " <<'$h'\n$file_inits{$_}\n$h"
               } keys %file_inits;
  my $pipe   = join '|', @commands;
  my $docs   = join "\n", @heredocs;
  join "\n", $mkdirs, $cats, $pipe, $docs;
}
4 ops.pl

$operators{g} = pmap {"sort $_"} maybe mr '^\d+';
$operators{m} = pmap {"ruby $_"} rbcode;
$operators{p} = pmap {"perl $_"} plcode;
352 spreadsheet.rb
#!/usr/bin/env ruby





require 'set'
class Fixnum
  def to_column_index; self; end
end
class String
  def to_column_index; self.downcase.ord - ?a.ord; end
end
class NilClass
  def [] *args; nil; end
  def map &f;   [];  end
end
class Object
  def unchanged; self; end
end
def adjacent_condition &f
  Class.new do
    define_method :initialize do |*v_maybe|
      unless v_maybe.empty?
        @v     = v_maybe[0]
        @v_set = true
      end
    end
    define_method :take? do |x|
      if @v_set
        r = f.call(@v, x)
        @v = x
        r
      else
        @v     = x
        @v_set = true
      end
    end
  end
end
class TakeN
  def initialize n; @n = n.to_i;    end
  def take?      x; (@n -= 1) >= 0; end
end
class CondColumn
  def initialize c, cond; @c = c; @cond = cond; end
  def take? x;            @cond.take? x[@c];    end
end
CellSelectors = {
  :E  => adjacent_condition {|x, y| x == y},
  :G  => adjacent_condition {|x, y| x > y},
  :L  => adjacent_condition {|x, y| x < y},
  :GE => adjacent_condition {|x, y| x >= y},
  :LE => adjacent_condition {|x, y| x <= y},
  :S  => adjacent_condition {|x, y| x >= 0 == y >= 0},
  :Z  => adjacent_condition {|x, y| (x.to_f == 0) == (y.to_f == 0)},
  :N  => TakeN}
TypeCoercions = {"i" => "to_i", "d" => "to_f", "s" => "to_s", nil => "unchanged"}



class Reducer
  attr_reader :state
  def initialize
    @children = []
    @consumer = false
  end
  def reduced?
    @children.reject!(&:reduced?)
    !@consumer and @children.empty?
  end
  def end!;      @children.each(&:end!).clear;                    end
  def forward x; @children.each {|c| c << x}.reject!(&:reduced?); end
  def << x;      forward x;                                       end
  # Transforms
  def child! r;  @children << r; r; end
  def consumer!; @consumer = true;  end
  def map &f;          child! MapReducer.new(f);                        end
  def flatmap &f;      child! FlatmapReducer.new(f);                    end
  def take_while cond; child! TakeWhileReducer.new(cond);               end
  def select &f;       child! SelectReducer.new(f);                     end
  def reject &f;       child! SelectReducer.new(proc {|x| !f.call(x)}); end
  def reduce x, &f;    child! ReduceReducer.new(x, f);                  end
  def mean
    reduce([0, 0]) do |state, x|
      state[0] += x
      state[1] += 1
      state
    end.map {|state| state[0].to_f / state[1]}
  end
  def to_a; reduce([])  {|s, x| s << x}; end
  def sum;  reduce(0)   {|s, x| s + x}; end
  def max;  reduce(nil) {|s, x| s.nil? || x > s ? x : s}; end
  def min;  reduce(nil) {|s, x| s.nil? || x < s ? x : s}; end
  def frequencies; reduce(Hash.new 0) {|m, x| m[x] += 1; m}; end
  def uniq
    s = Set.new
    select {|x| s.add? x}
  end
  def method_missing name, *args
    map {|r| r.send name, *args}
  end
end
class MapReducer < Reducer
  def initialize f; super(); @f = f;              end
  def << x;         forward(@state = @f.call(x)); end
end
class FlatmapReducer < Reducer
  def initialize f; super(); @f = f; end
  def << x
    ys = @f.call(x)
    ys.each do |y|
      forward(@state = @f.call(y))
    end
  end
end
class SelectReducer < Reducer
  def initialize f; super(); @f = f;                   end
  def << x;         forward(@state = x) if @f.call(x); end
end
class TakeWhileReducer < Reducer
  def initialize cond; super(); @cond = cond; end
  def << x
    return if @cond.nil?
    if @cond.take? x
      forward(@state = x)
    else
      @cond = nil
      self.end!
    end
  end
end
class ReduceReducer < Reducer
  def initialize state, r; super(); @state = state; @r = r; @consumer = true; end
  def << x;                @state = @r.call(@state, x);                       end
  def end!
    @children.each {|c| c << @state; c.end!}
    @children.clear
    @consumer = false
  end
end

class Spreadsheet
  def initialize source_io
    @lookahead = []
    @io        = source_io
    @io_eof    = false
    @step      = 1
    @reducers  = []
    @callbacks = []
    @r_called  = false
  end
  def run! code
    f = compile(code)
    until eof?
      @r_called = false
      x = f.call
      unless @r_called
        if x.is_a? Array
          r *x
        elsif not x.nil?
          r x
        end
      end
      advance!
    end
  end
  def eof?
    @lookahead.empty? and @io_eof ||= @io.eof?
  end
  def context; binding; end
  def compile code
    eval "proc {#{code}\n}", context
  end
  # Output stuff
  def r *xs
    @r_called = true
    if xs.any? {|x| x.is_a? Reducer}
      xs.select {|x| x.is_a? Reducer}.each(&:consumer!)
      @callbacks << proc do
        s = xs.map {|x| x.is_a?(Reducer) ? x.state : x}.join("\t")
        puts s rescue exit
      end
      nil
    else
      puts xs.join("\t") rescue exit
    end
  end
  def child! r; @reducers << r; r; end
  def cell c, r
    lookahead_to r
    @lookahead[r][c.to_column_index]
  end
  # Buffered lookahead
  def next_row
    return nil if @io_eof ||= @io.eof?
    @io.gets.chomp!.split(/\t/)
  end
  def lookahead_to cond
    if cond.is_a? Number
      cond_n = cond
      cond   = proc { |l| l.size > cond_n }
    end
    until cond[@lookahead] or @io_eof
      r = next_row
      @lookahead << r unless r.nil?
    end
  end
  def conditional_lookahead row, col, cond
    cond = CellSelectors[cond].new(cell col, row)
    take = 1
    take += 1 while !@io_eof and cond.take? cell(col, row + take)
    take
  end
  # IO interop
  def seek! n
    @step = n if n > @step
  end
  def advance!
    until @reducers.empty? or @io_eof
      lookahead_to 0
      @reducers.each {|r| r << @lookahead.first}.reject!(&:reduced?)
      unless @reducers.empty?
        @step -= 1
        @lookahead.shift
      end
    end
    @reducers.each(&:end!).clear if @io_eof
    @callbacks.each(&:call).clear
    if @step > 0
      if @lookahead.size > @step
        @lookahead.shift @step
      else
        (@step -= @lookahead.size).times {next_row}
        @lookahead.clear
      end
    end
    @step = 1
  end
  # Code generators (used by method generators below)
  def accessor_0 c, r, t, force
    eval %Q{proc do |*args|
      #{force ? "seek! #{r}" : ""}
      lookahead_to #{r}
      @lookahead[#{r}][#{c}].#{TypeCoercions[t]}
    end}
  end
  def accessor_1 flip90, c, r1, r2, t, force
    eval %Q{proc do |*args|
      #{force ? "seek! #{flip90 ? c : r2}" : ""}
      lookahead_to #{flip90 ? c : r2}
      #{flip90 ? "@lookahead[#{c}][#{r1}..#{r2}].map(&:#{TypeCoercions[t]})"
               : "@lookahead[#{r1}..#{r2}].map {|x| x[#{c}].#{TypeCoercions[t]}}"}
    end}
  end
  def accessor_2 c1, c2, r1, r2, t, force
    eval %Q{proc do |*args|
      #{force ? "seek! #{r2}" : ""}
      lookahead_to #{r2}
      @lookahead[#{r1}..#{r2}].map {|r| r[#{c1}..#{c2}].
                               map(&:#{TypeCoercions[t]})}
    end}
  end
  # Method generators
  def genf name, f
    singleton_class.instance_eval do
      define_method name, f
    end
  end
  def gencell name, c, r, t, force
    genf name, accessor_0(c.to_column_index, r, t, force)
  end
  def genhrange name, c1, c2, r, t, force
    genf name, accessor_1(true, r, c1.to_column_index, c2.to_column_index, t, force)
  end
  def genvrange name, c, r1, r2, t, force
    genf name, accessor_1(false, c.to_column_index, r1, r2, t, force)
  end
  def genrange name, c1, c2, r1, r2, t, force
    genf name, accessor_2(c1.to_column_index, c2.to_column_index, r1, r2, t, force)
  end
  def genvcond name, c, r, cond, cond_col, t, force
    c        = c.to_column_index
    cond_col = cond_col.to_column_index
    genf name,
      eval("proc {n = conditional_lookahead(#{r}, #{cond_col}, :#{cond})
                  #{force ? "seek! #{r} + n" : ""}
                  lookahead_to #{r - 1} + n
                  @lookahead[#{r}..#{r - 1} + n].map! do |xs|
                    xs[#{c}].#{TypeCoercions[t]}
                  end}")
  end
  def gencond name, c1, c2, r, cond, cond_col, t, force
    c1       = c1.to_column_index
    c2       = c2.to_column_index
    cond_col = cond_col.to_column_index
    genf name,
      eval("proc {n = conditional_lookahead(#{r}, #{cond_col}, :#{cond})
                  #{force ? "seek! #{r} + n" : ""}
                  lookahead_to #{r - 1} + n
                  @lookahead[#{r}..#{r - 1} + n].map! do |xs|
                    xs.map!(&:#{TypeCoercions[t]})
                  end}")
  end
  def genvlazy name, c, t, transform
    genf name,
      eval("proc {r = child!(Reducer.new)#{transform}
                  r.map {|xs| xs[#{c}].#{TypeCoercions[t]}}}")
  end
  def genlazy name, c1, c2, t, transform
    genf name,
      eval("proc {r = child!(Reducer.new)#{transform}
                  r.map {|xs| xs[#{c1}..#{c2}].map!(&:#{TypeCoercions[t]})}}")
  end
  def method_missing name, *args
    case name.to_s
      # Eager cases
      when /^([a-z])(\d*)([dis])?(!)?$/
        gencell name, $1, $2.to_i, $3, !!$4
      when /^([a-z])(\d*)_([dis])?(!)?$/
        genhrange name, $1, -1, $2.to_i, $3, !!$4
      when /^([a-z])_?([a-z])(\d*)([dis])?(!)?$/
        genhrange name, $1, $2, $3.to_i, $4, !!$5
      when /^([a-z])(\d*)_(\d+)([dis])?(!)?$/
        genvrange name, $1, $2.to_i, $3.to_i, $4, !!$5
      when /^([a-z])(\d*)_?([a-z])(\d+)([dis])?(!)?$/
        genrange name, $1, $2.to_i, $3, $4.to_i, $5, !!$6
      when /^([a-z])(\d*)_?([A-Z]+)([a-z])([dis])?(!)?$/
        genvcond name, $1, $2.to_i, $3.to_sym, $4, $5, !!$6
      when /^([a-z])(\d*)_?([a-z])([A-Z]+)([a-z])([dis])?(!)?$/
        gencond name, $1, $3, $2.to_i, $4.to_sym, $5, $6, !!$7
      # Lazy cases
      when /^_([a-z])([dis])?$/
        genvlazy name, $1.to_column_index, $2, ""
      when /^_([a-z])_?(\d+)([dis])?$/
        genvlazy name, $1.to_column_index, $4,
                 ".take_while(TakeN.new(#{$3.to_i - $2.to_i}))"
      when /^_([a-z])_?([A-Z]+)([a-z])([dis])?$/
        genvlazy name, $1.to_column_index, $4,
                 ".take_while(CondColumn.new(#{$3.to_column_index},
                              CellSelectors[:#{$2}].new))"
      when /^_([a-z])_?([a-z]+)([A-Z]+)([a-z])([dis])?$/
        genlazy name, $1.to_column_index, $2.to_column_index, $5,
                ".take_while(CondColumn.new(#{$4.to_column_index},
                             CellSelectors[:#{$3}].new))"
      else
        raise "unknown cell or range specifier: #{name}"
    end
    send(name, *args)
  end
end
Spreadsheet.new(IO.for_fd 3).run! ARGV[0]
