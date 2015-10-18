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
  def [] i; nil; end
end

class Object
  def unchanged; self; end
end

CellSelectors = {
  :E => Class.new {def initialize v; @v = v;  end
                   def take?      x; @v == x; end},
  :S => Class.new {def initialize v; @v = v >= 0;  end
                   def take?      x; @v == x >= 0; end},
  :Z => Class.new {def initialize v; @v = v.to_f == 0; end
                   def take?      x; @v == (x.to_f == 0); end},
  :N => Class.new {def initialize n; @n = n; end
                   def take?      x; (@n -= 1).nonzero?; end}}

TypeCoercions = {"i" => "to_i", "d" => "to_f", "s" => "to_s", nil => "unchanged"}

class Spreadsheet
  def initialize source_io
    @lookahead = []
    @io        = source_io
    @io_eof    = false
    @step      = 1
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
  def compile code; eval "proc {#{code}\n}", context; end

  # Output stuff
  def r *xs
    puts xs.join("\t")
  end

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
  def lookahead_to row
    @lookahead << @io.gets.chomp!.split(/\t/) until
      @lookahead.size > row or @io_eof ||= @io.eof?
  end

  def conditional_lookahead row, col, cond
    cond = cond.new(cell row, col)
    take = 0
    take += 1 while cond.take?(cell row + take, col)
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
      @lookahead.shift @step
    end
    @step = 1
  end

  # Method generators
  def gencell name, c, r, t, force
    c = c.to_column_index
    singleton_class.instance_eval do
      define_method name,
        eval(force ? "proc {seek! #{r + 1}; cell(#{c}, #{r}).#{TypeCoercions[t]}}"
                   : "proc {                cell(#{c}, #{r}).#{TypeCoercions[t]}}")
    end
  end

  def genhrange name, c1, c2, r, t, force
    c1 = c1.to_column_index
    c2 = c2.to_column_index
    singleton_class.instance_eval do
      define_method name,
        eval(force ? "proc {seek! #{r + 1}; hrange(#{c1}, #{c2}, #{r}).map!(&:#{TypeCoercions[t]})}"
                   : "proc {                hrange(#{c1}, #{c2}, #{r}).map!(&:#{TypeCoercions[t]})}")
    end
  end

  def genvrange name, c, r1, r2, t, force
    c = c.to_column_index
    singleton_class.instance_eval do
      define_method name,
        eval(force ? "proc {seek! #{r2 + 1}; vrange(#{c}, #{r1}, #{r2}).map!(&:#{TypeCoercions[t]})}"
                   : "proc {                 vrange(#{c}, #{r1}, #{r2}).map!(&:#{TypeCoercions[t]})}")
    end
  end

  def genrange name, c1, c2, r1, r2, t, force
    c1 = c1.to_column_index
    c2 = c2.to_column_index
    singleton_class.instance_eval do
      define_method name,
        eval(force ? "proc {seek! #{r2 + 1}; range(#{c1}, #{c2}, #{r1}, #{r2}).map! {|xs| xs.map!(&:#{TypeCoercions[t]})}}"
                   : "proc {                 range(#{c1}, #{c2}, #{r1}, #{r2}).map! {|xs| xs.map!(&:#{TypeCoercions[t]})}}")
    end
  end

  def method_missing name, *args
    ns = name.to_s
    case ns
    when /^([a-z])(\d*)([dis])?(!)?$/
      gencell name, $1, $2.to_i, $3, !!$4
    when /^([a-z])_?([a-z])(\d*)([dis])?(!)?$/
      genhrange name, $1, $2, $3.to_i, $4, !!$5
    when /^([a-z])(\d*)_(\d+)([dis])?(!)?$/
      genvrange name, $1, $2.to_i, $3.to_i, $4, !!$5
    when /^([a-z])(\d*)_?([a-z])(\d+)([dis])?(!)?$/
      genrange name, $1, $2.to_i, $3, $4.to_i, $5, !!$6
    when /^([a-z])(\d*)_?([A-Z])([a-z])([dis])?(!)?$/
      genvcond name, $1, $2.to_i, $3.to_sym, $4, $5, !!$6
    when /^([a-z])(\d*)_?([a-z])([A-Z])([a-z])([dis])?(!)?$/
      gencond name, $1, $3, $2.to_i, $4.to_sym, $5, $6, !!$7
    end
    send(name)
  end
end

Spreadsheet.new($stdin).run!(ARGV[0])
