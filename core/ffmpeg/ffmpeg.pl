# NOTE: on some systems you may want to use 'avconv' instead
defconfenv 'ffmpeg', FFMPEG => 'ffmpeg';
defconfenv 'ffplay', FFPLAY => 'ffplay';

BEGIN {
  defparseralias media_format_spec =>
    pseq prx '\w+',
         popt pn(1, prx"/", prx '\w+'),
         popt pn(1, prx"/", prx '\w+');
}