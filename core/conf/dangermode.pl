defconfenv 'ni/dangermode', NI_DANGER_MODE => 0;

sub requires_dangermode($)
{
  unless (conf('ni/dangermode'))
  {
    my ($operator) = @_;
    die "$operator has the potential to destroy your data if misused. "
      . "To enable it, export NI_DANGER_MODE=1 or use ^{ni/dangermode=1}.";
  }
}
