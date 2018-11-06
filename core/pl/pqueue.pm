=head1 Priority queue
A hash-based object that supports efficient query and removal of the minimum
element. You can use arbitrary values and a custom comparator if you want to.
=cut

package pqueue
{
  # Interfacing: address the priority queue as a mutable map. STORE operations
  # update elements' priorities, which is a delete/reinsert.
  package pqueue::hashtie
  {
    sub TIEHASH
    {
      my ($class, $pqueue) = @_;
      bless \$pqueue, $class;
    }

    sub FETCH { my ($pqueue_ref, $k) = @_; $$$$pqueue_ref{vals}{$k} }
    sub STORE
    {
      my ($pqueue_ref, $k, $v) = @_;
      $$pqueue_ref->delete($k) if exists $$$$pqueue_ref{index}{$k};
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
  }

  use overload '%{}' => \&to_h;

  sub new
  {
    my ($class, $compfn) = @_;
    my $self = bless \{ index => {},      # key -> heap position
                        vals  => {},      # key -> value
                        heap  => [undef], # heap of keys
                        comp  => defined $compfn ? $compfn
                                                 : sub { $_[0] < $_[1] }},
                     $class;

    tie %{$$$self{magic_hash} = {}}, 'pqueue::hashtie', $self;
    $self;
  }

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

    # TODO: optimize bulk insert
    while (@_)
    {
      my $k = shift;
      my $v = shift;

      # Append, then heapify up
      $$kv{$k} = $v;
      my $i = $$ki{$k} = push(@$h, $k) - 1;
      while ($i > 1 && &$fn($v, $$kv{$$h[$i >> 1]}))
      {
        @$ki{$k, $$h[$i >> 1]} = @$ki{$$h[$i >> 1], $k};
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

    if ($i < $#$h)
    {
      $$ki{$$h[$i] = pop @$h} = $i;

      while (1)
      {
        my $top = $i;
        $top = $i << 1     if $i << 1 < @$h
                           && &$fn($$kv{$$h[$i << 1]},     $$kv{$$h[$top]});
        $top = $i << 1 | 1 if ($i << 1 | 1) < @$h
                           && &$fn($$kv{$$h[$i << 1 | 1]}, $$kv{$$h[$top]});

        last if $top == $i;

        # Swap the two elements
        my $topk = $$h[$top];
        @$h[$i, $top] = @$h[$top, $i];
        @$ki{$k, $topk} = ($top, $i);
        $i = $top;
      }
    }
    else
    {
      pop @$h;
    }

    $v;
  }
}
