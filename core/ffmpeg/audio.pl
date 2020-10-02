# Audio processing

defoperator audio_extract => q{
  my ($format, $codec, $bitrate) = @_;
  exec conf('ffmpeg'), '-i', '-', '-vn',
       '-f', $format,
       defined($codec) ? ('-c:a', $codec) : (),
       defined($bitrate) ? ('-b:a', $bitrate) : (), '-'};

defshort '/AE', pmap q{audio_extract_op @$_}, media_format_spec;
