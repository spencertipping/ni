# Buffering operators.
# Buffering a stream causes it to be forced in its entirety. Buffering does not
# imply, however, that the stream will be consumed at any particular rate; some
# buffers may be size-limited, at which point writes will block until there's
# space available.

# Null buffer.
# Forwards data 1:1, but ignores pipe signals on its output.

defoperator buffer_null => q{local $SIG{PIPE} = 'IGNORE'; sio};

defshort '/B',
  defdsp 'bufferalt', 'dispatch table for /B buffer operator',
    n => pmap q{buffer_null_op}, pnone;
