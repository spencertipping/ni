# JSPlot interop.
# JSPlot is served over HTTP as a portable web interface. It requests data via
# AJAX, and may request the same data multiple times to save browser memory. The
# JSPlot driver buffers the data to disk to make it repeatable.

use constant jsplot_gen => gen $ni::self{'core/jsplot/html'};

use constant jsplot_html =>
  jsplot_gen->(css => $ni::self{'core/jsplot/css'},
               js  => join '', @ni::self{qw| core/jsplot/jquery.min.js
                                             core/jsplot/jquery.mousewheel.min.js
                                             core/caterwaul/caterwaul.min.js
                                             core/caterwaul/caterwaul.std.min.js
                                             core/caterwaul/caterwaul.ui.min.js
                                             core/jsplot/murmurhash3.js
                                             core/jsplot/modus.js
                                             core/jsplot/vector.js
                                             core/jsplot/axis.waul
                                             core/jsplot/label.waul
                                             core/jsplot/dataframe.waul
                                             core/jsplot/matrix.waul
                                             core/jsplot/socket.waul
                                             core/jsplot/render.waul
                                             core/jsplot/camera.waul
                                             core/jsplot/interface.waul |});

# Parsing.
# This entry point provides a realtime CLI parse for the UI.

sub jsplot_parse($$@) {
  my ($reply, $req, @ni_args) = @_;
  my ($ops, @rest) = cli @ni_args;
  http_reply $reply, 200, json_encode {ops => $ops, unparsed => [@rest]};
}

# JSPlot data streaming.
# This is the websocket connection that ni uses to stream data to the client. Any
# data we receive from the client indicates that the client is canceling the
# websocket request, so we need to break the pipe and kill off subprocesses.

sub jsplot_log($@) {printf STDERR "ni js[$$]: $_[0]", @_[1..$#_]}

sub jsplot_stream($$@) {
  local $_;
  my ($reply, $req, @ni_args) = @_;
  my ($ops, @rest) = cli @ni_args;
  die "ni: jsplot failed to parse starting at @rest" unless defined $ops;

  jsplot_log "running %s\n", json_encode $ops;

  safewrite $reply, ws_header($req);
  my $ni_pipe = sni @$ops, http_websocket_encode_batch_op 65536;

  my $incoming;
  my $rmask = '';
  vec($rmask, fileno $reply, 1) = 1;

  while (saferead $ni_pipe, $_, 65536) {
    safewrite $reply, $_;
    if (select(my $rout = $rmask, undef, undef, 0.004))
    {
      saferead $reply, $incoming, 8192;
      if ($incoming =~ /^\x88/) {
        jsplot_log "SIGTERM to worker\n";
        $ni_pipe->kill('TERM');
        jsplot_log "awaiting worker exit\n";
        jsplot_log "worker exited with %d\n", $ni_pipe->await;
        return;
      }
    }
  }
  jsplot_log "done transferring data\n";
  $ni_pipe->await;
  jsplot_log "worker exited with %d\n";
}

sub jsplot_server {
  my ($port) = @_;
  load 'core/http/ws.pm';
  http $port, sub {
    my ($url, $req, $reply) = @_;
    return print "http://localhost:$port/\n"             unless defined $reply;
    return http_reply $reply, 200, jsplot_html           if $url eq '/';
    return jsplot_parse($reply, $req, shell_unquote $1)  if $url =~ /^\/parse\/([\s\S]*)/;
    return jsplot_stream($reply, $req, shell_unquote $1) if $url =~ /^\/ni\/([\s\S]*)/;
    return http_reply $reply, 404, $url;
  };
}

defclispecial '--js', q{jsplot_server $_[0] || 8090}, <<'_';
Usage: ni --js [port=8090]
Runs a web interface that allows you to visualize data streams. See ni
//help/visual for details.
_
