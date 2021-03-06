# Ruby code element.
# This works just like the Perl code parser but is slightly less involved because
# there's no `BEGIN/END` substitution. We also don't need to take a code
# transform because no amount of wrapping will change whether an expression can
# be parsed.

use POSIX ();

BEGIN {
defparser 'rbcode', '', q{
  return $_[1], '', @_[2..$#_] unless $_[1] =~ /\]$/;
  my ($self, $code, @xs) = @_;
  my ($x, $status) = ('', 0);
  $x .= ']' while $status = syntax_check 'ruby -c -', $code and $code =~ s/\]$//;
  die <<EOF if $status;
ni: failed to get closing bracket count for ruby code "$code$x"; this means
    your code has a syntax error.
EOF
  ($code, $x, @xs);
};
}

# Ruby wrapper.

use constant ruby_mapgen => gen q{
  %prefix
  STDIN.close
  $in = IO.new(3)
  class Line
    def row
      %body
    end
  end

  def map_mode! x
    if x.is_a? Enumerable
      x.each do |v|
        v = r *v if v.is_a? Enumerable
        puts v
      end
    elsif !x.nil?
      puts x
    end
  end

  while $l = next_line
    x = $l.row
    %each
  end
  exit 0
};

use constant ruby_prefix => join "\n", @ni::self{qw| core/rb/prefix.rb |};

sub stdin_to_ruby($) {
  cdup2 0, 3;
  POSIX::close 0;
  safewrite siproc {exec 'ruby', '-'}, $_[0];
}

sub ruby_code($$) {ruby_mapgen->(prefix => ruby_prefix,
                                 body   => $_[0],
                                 each   => $_[1])}

sub ruby_mapper($)  {ruby_code $_[0], 'map_mode! x'}
sub ruby_grepper($) {ruby_code $_[0], 'puts $l if x'}

defoperator ruby_mapper  => q{stdin_to_ruby ruby_mapper  $_[0]};
defoperator ruby_grepper => q{stdin_to_ruby ruby_grepper $_[0]};

defshort '/m',
  defalt 'rubyalt', 'alternatives for the /m ruby operator',
    pmap q{ruby_mapper_op $_}, rbcode;

defrowalt pmap q{ruby_grepper_op $_}, pn 1, pstr 'm', rbcode;
