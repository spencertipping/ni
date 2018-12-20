=head1 Priority queue
A hash-based object that supports efficient query and removal of the minimum
element. You can use arbitrary values and a custom comparator if you want to.
=cut

# Interfacing: address the priority queue as a mutable map. STORE operations
# update elements' priorities, which is a delete/reinsert.
package pqueue::hashtie;
sub TIEHASH
{
  my ($class, $pqueue) = @_;
  bless \$pqueue, $class;
}

sub FETCH { my ($pqueue_ref, $k) = @_; $$$$pqueue_ref{vals}{$k} }
sub STORE
{
  my ($pqueue_ref, $k, $v) = @_;
  $$pqueue_ref->insert($k, $v);
}

sub DELETE { my ($pqueue_ref, $k) = @_; $$pqueue_ref->delete($k) }
sub CLEAR  { my ($pqueue_ref)     = @_; $$pqueue_ref->clear }
sub EXISTS { my ($pqueue_ref, $k) = @_; exists $$$$pqueue_ref{index}{$k} }

sub FIRSTKEY { my ($pqueue_ref) = @_; $$$$pqueue_ref{heap}[1] }
sub NEXTKEY
{
  my ($pqueue_ref, $k) = @_;
  $$$$pqueue_ref{heap}[$$$$pqueue_ref{index}{$k} + 1];
}

sub SCALAR { my ($pqueue_ref) = @_; $$$$pqueue_ref{heap}[1] }


# The queue itself, which delegates hash functionality to the above class
package pqueue;

use overload '%{}' => \&to_h;

sub new
{
  my ($class, $compfn) = @_;
  my $self = bless \{ index  => {},       # key -> heap position
                      vals   => {},       # key -> value
                      heap   => [undef],  # heap of keys
                      strict => 0,
                      comp   => defined $compfn ? $compfn
                                                : sub { $_[0] < $_[1] }},
                   $class;

  tie %{$$$self{magic_hash} = {}}, 'pqueue::hashtie', $self;
  $self;
}

sub strict { my $self = shift; $$$self{strict} = 1; $self }

sub to_h { ${+shift}->{magic_hash} }

sub size { $#{${+shift}->{heap}} }
sub top  { ${+shift}->{heap}->[1] }
sub pull
{
  my $self = shift;
  my $top  = $self->top;
  $self->delete($top);
  $top;
}

sub clear
{
  my $self = shift;
  @{$$$self{heap}}  = (undef);
  %{$$$self{index}} = ();
  %{$$$self{vals}}  = ();
  $self;
}

sub insert
{
  my $self = shift;
  my $h  = $$$self{heap};
  my $ki = $$$self{index};
  my $kv = $$$self{vals};
  my $fn = $$$self{comp};

  # TODO: optimize bulk insert on an empty queue
  while (@_)
  {
    my $k = shift;
    my $v = shift;

    $self->delete($k) if exists $$ki{$k};

    $$kv{$k} = $v;
    push @$h, $k;
    my $i = $$ki{$k} = $#$h;

    while ($i > 1 && &$fn($v, $$kv{$$h[$i >> 1]}))
    {
      @$ki{$k, $$h[$i >> 1]} = ($i >> 1, $i);
      @$h[$i, $i >> 1] = @$h[$i >> 1, $i];
      $i >>= 1;
    }
  }

  $self;
}

sub delete
{
  my ($self, $k) = @_;
  my $h  = $$$self{heap};
  my $ki = $$$self{index};
  my $kv = $$$self{vals};
  my $fn = $$$self{comp};

  my $v = delete $$kv{$k};
  my $i = delete $$ki{$k};

  if (!defined $i)
  {
    die "ni pqueue: attempting to remove nonexistent element $k"
      if $$self{strict};
    return $v;
  }

  $k = pop @$h;

  # First order of business: the rest of the heap is fine if we've just
  # legitimately removed the last element.
  if ($i < @$h)
  {
    # OK, we need to update an element somewhere in the heap -- which means
    # first heapifying up, then down. Heapify up is required because the last
    # element and the one we're replacing may come from different subtrees, so
    # we don't know their relative ordering.
    $$ki{$$h[$i] = $k} = $i;
    $v = $$kv{$k};

    while ($i > 1 && &$fn($v, $$kv{$$h[$i >> 1]}))
    {
      @$ki{$k, $$h[$i >> 1]} = ($i >> 1, $i);
      @$h[$i, $i >> 1] = @$h[$i >> 1, $i];
      $i >>= 1;
    }

    while (1)
    {
      # From the set of (i, leftchild, rightchild), select the topmost element
      # and swap i with it (or, if it's i, then we're done).
      my $top = $i;

      $top = $i << 1
        if $i << 1 < @$h
        && &$fn($$kv{$$h[$i << 1]}, $$kv{$$h[$top]});

      $top = $i << 1 | 1
        if ($i << 1 | 1) < @$h
        && &$fn($$kv{$$h[$i << 1 | 1]}, $$kv{$$h[$top]});

      last if $top == $i;

      # Swap element positions within @$h, and update %$ki to reflect the new
      # positions. $k can remain the same because $i becomes $top.
      @$ki{$k, $$h[$top]} = ($top, $i);
      @$h[$i, $top] = @$h[$top, $i];
      $i = $top;
    }
  }

  $v;
}
