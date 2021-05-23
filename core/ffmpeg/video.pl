# Video access and transcoding

# youtube-dl: use youtube video data as URL streams, e.g. yt://dQw4w9WgXcQ
defconfenv 'ytdl', YOUTUBE_DL => 'youtube-dl';
defresource 'yt', read => q{sh conf('ytdl') . " " . shell_quote $_[1], "-o", "-"};

# v4l2 source: use local cameras as URL streams, e.g. v4l2:///dev/video0
defresource 'v4l2', read => q{
  sh conf('ffmpeg') . " -f v4l2 -i " . shell_quote($_[1]) . " -c:v copy -f avi -"};

# x11grab source: screencast to lossless video, e.g. x11grab://:0.0@3840x2160
defresource 'x11grab', read => q{
  my ($display, $w, $h) = $_[1] =~ /(:\d+(?:\.\d+)?)(?:@(\d+)x(\d+))?/;
  my $size = defined $w ? "-s ${w}x${h}" : "";
  sh conf('ffmpeg') . " -f x11grab $size -i " . shell_quote($display) . " -f avi -c:v huffyuv -"};

# ffplay alias for brevity
defoperator video_play => q{sh conf('ffplay') . " -"};
defshort '/VP', pmap q{video_play_op}, pnone;

# Video<->image conversion
defoperator video_to_imagepipe => q{
  my ($codec, $scale) = @_;
  $codec = 'png' unless defined $codec;
  $scale = defined($scale) ? "-s $scale" : '';
  sh conf('ffmpeg') . " -i - -f image2pipe $scale -c:v $codec -"};

defoperator imagepipe_to_video => q{
  my ($format, $codec, $bitrate) = @_;
  sh conf('ffmpeg') . " -f image2pipe -i - "
     . shell_quote '-f', $format,
                   defined($codec) ? ('-c:v', $codec) : (),
                   defined($bitrate) ? ('-b:v', $bitrate) : (),
                   '-'};

defshort '/VI', pmap q{video_to_imagepipe_op @$_}, pseq popt(prx '\w+'),
                                                        popt(prx '@(\d+x\d+)');
defshort '/IV', pmap q{imagepipe_to_video_op @$_}, media_format_spec;
