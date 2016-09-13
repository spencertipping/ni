# ni ruby driver prefix
# This is loaded prior to the short codegen segment in rb.pl.sdoc.

$have_json = true
require 'json' rescue $have_json = false

class Line
  attr_reader :str

  def initialize s
    @str    = s
    @fields = nil
  end

  def [] *x
    fields[*x]
  end

  def fields
    @fields ||= @str.split /\t/
  end

  def to_s
    @str
  end
end

# Some metaprogramming to get letter accessors
Line.class_eval do
  ('a'..'q').each do |l|
    index = l[0].ord - 97
    define_method    l   .to_sym, proc {fields[index]}
    define_method "#{l}s".to_sym, proc {fields[index].to_s}
    define_method "#{l}i".to_sym, proc {fields[index].to_i}
    define_method "#{l}f".to_sym, proc {fields[index].to_f}
    define_method "#{l}d".to_sym, proc {fields[index].to_f}
  end
end

def r *xs
  xs.join "\t"
end

# Readahead support
$q = []
$l = nil

def next_line
  return $q.shift unless $q.empty?
  Line.new STDIN.readline.chomp! rescue nil
end

def rw
  r = [$l]
  l = nil
  r << l while l = next_line and yield l
  $q << l if l
  r
end

def ru
  r = [$l]
  l = nil
  r << l until !(l = next_line) or yield l
  $q << l if l
  r
end

def re &f
  v = f.call $l
  rw {|l| f.call(l) == v}
end
