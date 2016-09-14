# ni ruby driver prefix
# This is loaded prior to the short codegen segment in rb.pl.sdoc.

$have_json = true
begin
  require 'json'
rescue ScriptError
  $have_json = false
end

# Portability stuff.
# Old versions of Ruby have "a"[0] == 97, new versions have "a"[0] == "a". This
# makes it always safe to say "a"[0].ord.
class Numeric
  def ord; self; end
end

# Add to_proc conversion to symbols, which makes it possible to write
# map(&:foo).
class Symbol
  def to_proc
    x = self
    proc {|v| v.send(x)}
  end
end

class Line
  attr_reader :fields

  def initialize s
    @fields = s.split /\t/
  end

  def [] *x
    fields[*x]
  end

  def to_s
    fields.join "\t"
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
  end
end

Enumerable.class_eval do
  ('a'..'q').each do |l|
    index = l[0].ord - 97
    define_method    l   .to_sym, proc {map {|x| x.fields[index]}}
    define_method "#{l}s".to_sym, proc {map {|x| fields[index].to_s}}
    define_method "#{l}i".to_sym, proc {map {|x| fields[index].to_i}}
    define_method "#{l}f".to_sym, proc {map {|x| fields[index].to_f}}
  end
end

def r *xs
  xs.join("\t")
end

# Readahead support
$q = []
$l = nil

def next_line
  return $q.shift unless $q.empty?
  Line.new($in.readline.chomp!) rescue nil
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
