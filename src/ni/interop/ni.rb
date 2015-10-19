#!/usr/bin/env ruby
# A standalone Ruby program that takes a spreadsheet function on the command
# line. See doc/spreadsheet.md for details about how this works.

class Fixnum
  def to_column_index; self; end
end

class String
  def to_column_index; self.downcase.ord - ?a.ord; end
end

class NilClass
  def [] *args; nil; end
end

class Object
  def unchanged; self; end
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
  :E  => Class.new {def initialize v; @v = v;  end
                    def take?      x; @v == x; end},

  :G  => Class.new {def initialize v; @v = v;                end
                    def take?      x; (x > @v).tap {@v = x}; end},
  :L  => Class.new {def initialize v; @v = v;                end
                    def take?      x; (x < @v).tap {@v = x}; end},
  :GE => Class.new {def initialize v; @v = v;                 end
                    def take?      x; (x >= @v).tap {@v = x}; end},
  :LE => Class.new {def initialize v; @v = v;                 end
                    def take?      x; (x <= @v).tap {@v = x}; end},

  :S  => Class.new {def initialize v; @v = v.to_f >= 0;  end
                    def take?      x; @v == x.to_f >= 0; end},
  :Z  => Class.new {def initialize v; @v = v.to_f == 0;    end
                    def take?      x; @v == (x.to_f == 0); end},
  :N  => TakeN}

TypeCoercions = {"i" => "to_i", "d" => "to_f", "s" => "to_s", nil => "unchanged"}

class Reducer
  attr_reader :state

  def initialize
    @children = []
    @eof      = false
  end

  def reduced?;  @children.empty? and !@eof;                      end
  def end!;      @children.each(&:end!).empty!; @eof = true;      end
  def forward x; @children.each {|c| c << x}.reject!(&:reduced?); end
  def << x;      forward x;                                       end

  # Transforms
  def child! x; @children << x; x; end

  def map &f;          child! MapReducer.new(f);          end
  def take_while cond; child! TakeWhileReducer.new(cond); end
  def select &f;       child! SelectReducer.new(f);       end
  def reduce x, &f;    child! ReduceReducer.new(x, f);    end

  def mean
    reduce([0, 0]) do |state, x|
      state[0] += x
      state[1] += 1
      state
    end.map {|state| state[0].to_f / state[1]}
  end

  def sum; reduce(0)   {|s, x| s + x}; end
  def max; reduce(nil) {|s, x| s.nil? || x > s ? x : s}; end
  def min; reduce(nil) {|s, x| s.nil? || x < s ? x : s}; end
end

class MapReducer < Reducer
  def initialize f; @f = f;                      end
  def << x;         forward @f.call(@state = x); end
end

class SelectReducer < Reducer
  def initialize f; @f = f;                            end
  def << x;         forward(@state = x) if @f.call(x); end
end

class TakeWhileReducer < Reducer
  def initialize cond; @cond = cond; end
  def << x
    @cond = nil unless @cond.take? x
    end! if @cond.nil?
  end
end

class ReduceReducer < Reducer
  def initialize state, r; @state = state; @r = r;      end
  def << x;                @state = @r.call(@state, x); end
end

class Spreadsheet
  @@instance = nil

  def initialize source_io
    @lookahead = []
    @io        = source_io
    @io_eof    = false
    @step      = 1
    @reducers  = []
  end

  def run! code
    f = compile(code)
    until eof?
      instance_eval &f
      advance!
    end
  end

  def eof?
    @lookahead.empty? and @io_eof ||= @io.eof?
  end

  def context; binding; end
  def compile code; @@instance = self; eval "proc {#{code}\n}", context; end

  # Output stuff
  def r *xs
    if xs.any? {|x| x.is_a? Reducer}
      @callbacks << proc do
        r xs.map {|x| x.is_a?(Reducer) ? x.state : x}
      end
    else
      puts xs.join("\t") rescue exit
    end
  end

  def child! r; @reducers << x; end

  # Input stuff
  def cell c, r
    lookahead_to r
    @lookahead[r][c.to_column_index]
  end

  def hrange c1, c2, r
    lookahead_to r
    @lookahead[r][c1.to_column_index .. c2.to_column_index]
  end

  def vrange c, r1, r2
    lookahead_to r2
    c = c.to_column_index
    @lookahead[r1 .. r2].map {|r| r[c]}
  end

  def range c1, c2, r1, r2
    lookahead_to r2
    c1 = c1.to_column_index
    c2 = c2.to_column_index
    @lookahead[r1 .. r2].map {|r| r[c1 .. c2]}
  end

  def cond_vrange c, r, col, cond
    vrange c, r, r + conditional_lookahead(r, col, cond)
  end

  def cond_range c1, c2, r, col, cond
    range c1, c2, r, r + conditional_lookahead(r, col, cond)
  end

  # Buffered lookahead
  def next_row
    xs = @io.gets.chomp!.split(/\t/)
    @io_eof ||= @io.eof?
    xs
  end

  def lookahead_to row
    until @lookahead.size > row or @io_eof
      xs = next_row
      @lookahead << xs
      @reducers.each {|x| x << xs}.reject!(&:reduced?)
    end
  end

  def conditional_lookahead row, col, cond
    cond = CellSelectors[cond].new(cell col, row)
    take = 1
    take += 1 while cond.take? cell(col, row + take)
    take
  end

  # IO interop
  def seek! n
    @step = n if n > @step
  end

  def advance!
    if @step > @lookahead.size
      (@step -= @lookahead.size).times {@io.gets unless @io_eof ||= @io.eof?}
      @lookahead = []
    else
      discarded = @lookahead.shift @step
    end
    @step = 1

    # Handle any outstanding lazy computations TODO
    until @reducers.empty? or @lookahead.empty? && @io_eof
      xs = next_row
      @reducers.each {|r| r << xs}.reject!(&:reduced?)
    end
    @reducers.each(&:end!)
    @callbacks.each(&:call)
  end

  # Code generators (used by method generators below)
  def accessor_0 c, r, t, force
    eval "proc {#{force ? "seek! #{r}" : ""}
                lookahead_to #{r}
                @lookahead[#{r}][#{c}].#{TypeCoercions[t]}}"
  end

  def accessor_1 flip90, c, r1, r2, t, force
    eval "proc {#{force ? "seek! #{flip90 ? c : r2}" : ""}
                lookahead_to #{flip90 ? c : r2}
                #{flip90 ? "@lookahead[#{c}][#{r1}..#{r2}].map(&:#{TypeCoercions[t]})"
                         : "@lookahead[#{r1}..#{r2}].map {|x| x[#{c}].#{TypeCoercions[t]}}"}}"
  end

  def accessor_2 c1, c2, r1, r2, t, force
    eval "proc {#{force ? "seek! #{r2}" : ""}
                lookahead_to #{r2}
                @lookahead[#{r1}..#{r2}].map {|r| r[#{c1}..#{c2}].map(&:#{TypeCoercions[t]})}}"
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
                  vrange(#{c}, #{r}, #{r - 1} + n).map!(&:#{TypeCoercions[t]})}")
  end

  def gencond name, c1, c2, r, cond, cond_col, t, force
    c1       = c1.to_column_index
    c2       = c2.to_column_index
    cond_col = cond_col.to_column_index
    genf name,
      eval("proc {n = conditional_lookahead(#{r}, #{cond_col}, :#{cond})
                  #{force ? "seek! #{r} + n" : ""}
                  range(#{c1}, #{c2}, #{r}, #{r - 1} + n).map! {|xs| xs.map!(&:#{TypeCoercions[t]})}}")
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
        genvlazy name, $1, $2, ".map {|r| r[#{$2}]}"
      when /^_([a-z])_?(\d+)([dis])?$/
        genvlazy name, $1, $4, ".take_while(TakeN.new(#{$3.to_i - $2.to_i}))"
      when /^_([a-z])_?([A-Z]+)([a-z])([dis])?$/
        genvlazy name, $1,
          ".take_while(CondColumn.new(#{$3.to_column_index},
                                      CellSelectors[:#{$2}].new))"
      when /^_([a-z])_?([a-z]+)([A-Z]+)([a-z])([dis])?$/
        genlazy name, $1, $2, $5,
          ".take_while(CondColumn.new(#{$4.to_column_index},
                                      CellSelectors[:#{$3}].new))"

      else
        raise "unknown cell or range specifier: #{name}"
    end

    send(name)
  end
end

Spreadsheet.new($stdin).run! ARGV[0]
