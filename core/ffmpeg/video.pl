# Video access and transcoding

# youtube-dl: use youtube video data as URL streams, e.g. yt://dQw4w9WgXcQ
defconfenv 'ytdl', YOUTUBE_DL => 'youtube-dl';
defresource 'yt', read => q{sh conf('ytdl') . " " . shell_quote $_[1], "-o", "-"};

# Video<->image conversion
defoperator video_to_imagepipe => q{
  my ($codec) = @_;
  $codec = 'png' unless defined $codec;
  sh conf('ffmpeg') . " -i - -f image2pipe -c:v $codec -"};

defoperator imagepipe_to_video => q{
  my ($format, $codec, $bitrate) = @_;
  sh conf('ffmpeg') . " -f image2pipe -i - "
     . shell_quote '-f', $format,
                   defined($codec) ? ('-c:v', $codec) : (),
                   defined($bitrate) ? ('-b:v', $bitrate) : (),
                   '-'};

defshort '/VI', pmap q{video_to_imagepipe_op $_}, popt prx '\w+';
defshort '/IV', pmap q{imagepipe_to_video_op @$_}, media_format_spec;
