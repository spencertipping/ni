defconfenv 'ni/dangermode', NI_DANGER_MODE => 0;
defconfenv 'ni/yolomode',   NI_YOLO_MODE   => 0;


sub requires_dangermode($)
{
  my ($operator) = @_;
  unless (conf('ni/dangermode') or conf('ni/yolomode'))
  {
    die "$operator has the potential to destroy your data or cost money "
      . "if misused. To enable it, export NI_DANGER_MODE=1 or use "
      . "^{ni/dangermode=1}.";
  }

  print "ni: using $operator in YOLO MODE!!!!\n" if conf('ni/yolomode');
}
