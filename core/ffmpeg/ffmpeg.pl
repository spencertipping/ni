# NOTE: on some systems you may want to use 'avconv' instead
defconfenv 'ffmpeg', FFMPEG => 'ffmpeg';
defconfenv 'ffplay', FFPLAY => 'ffplay';

BEGIN {
  defparseralias media_format_spec =>
    pseq prx '\w+',
         popt pn(1, prx"/", prx '\w+'),
         popt pn(1, prx"/", prx '\w+');
}

# Read m3us directly using ffmpeg. We can't stream them because they rely on URL
# locality. The muxer format defaults to FLV, which is a flexible format that
# doesn't require seeking to write.
defresource 'm3u',
  read => q{sh conf('ffmpeg') . ' ' .
    shell_quote '-i', $_[1], '-c', 'copy', '-f', 'flv', '-'};

# Not all container formats support all codecs, so we produce a separate URL
# scheme for each possible one.
#
# ni e[ffmpeg -muxers] rp'/^\s*E/' FSfCr/\\w/FCpF_ p'join" ", rw{1}'
for my $demuxer (qw/
  3g2 3gp a64 ac3 adts adx aiff alaw alsa amr apng aptx aptx_hd asf asf_stream
  ass ast au avi avm2 avs2 bit caca caf cavsvideo chromaprint codec2 codec2raw
  crc dash data daud dirac dnxhd dts dv dvd eac3 f32be f32le f4v f64be f64le
  fbdev ffmetadata fifo fifo_test film_cpk filmstrip fits flac flv framecrc
  framehash framemd5 g722 g723_1 g726 g726le gif gsm gxf h261 h263 h264 hash hds
  hevc hls ico ilbc image2 image2pipe ipod ircam ismv ivf jacosub latm lrc m4v
  matroska md5 microdvd mjpeg mkvtimestamp_v2 mlp mmf mov mp2 mp3 mp4 mpeg
  mpeg1video mpeg2video mpegts mpjpeg mulaw mxf mxf_d10 mxf_opatom null nut oga
  ogg ogv oma opengl opus oss psp pulse rawvideo rm roq rso rtp rtp_mpegts rtsp
  s16be s16le s24be s24le s32be s32le s8 sap sbc scc sdl sdl2 segment singlejpeg
  smjpeg smoothstreaming sndio sox spdif spx srt stream_segment ssegment sup
  svcd swf tee truehd tta u16be u16le u24be u24le u32be u32le u8 uncodedframecrc
  vc1 vc1test vcd vidc video4linux2 v4l2 vob voc w64 wav webm webm_chunk
  webm_dash_manifest webp webvtt wtv wv xv yuv4mpegpipe /)
{
  defresource 'm3u+' . $demuxer,
    read => q{sh conf('ffmpeg') . ' ' .
      shell_quote '-i', $_[1], '-c', 'copy', '-f', '} . $demuxer . q{', '-'};
}
