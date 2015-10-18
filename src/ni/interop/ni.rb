#!/usr/bin/env ruby
# A standalone Ruby program that takes a row-mapper on the command line.

#REQUIRES
#LIBRARIES

module Ni
  @@context = binding

  def r *xs; xs.join "\t"; end

  class TSVContext
    attr_reader :context
    attr        :fields

    def init
      @context = binding
    end

    def << fields
      @fields = fields
    end

    def method_missing name, *args
      f = case name.to_s
          when /[sf](\d+)/
            eval "proc {@fields[#{$1}]}"
          when /d(\d+)/
            eval "proc {@fields[#{$1}].to_f}"
          when /i(\d+)/
            eval "proc {@fields[#{$1}].to_i}"
          else
            raise "unknown field spec: #{name.to_s}"
          end

      singleton_class.instance_eval do
        define_method name, f
      end

      send name, *args
    end
  end

  def self.run code
    c = TSVContext.new
    f = eval "proc {#{code}\n}", c.context

    STDIN.each do |line|
      c << line.chomp!.split(/\t/)
      rs = c.instance_eval &f
      if rs.is_a? Array
        rs.each {|r| puts r}
      else
        puts rs.to_s
      end
    end
  end
end

Ni.run ARGV[0]
