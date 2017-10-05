# Image functions.
# ni needs to be able to reconstruct itself from a map. These functions implement
# the map commands required to do this.

our %self;

our $ni_map = 'core/boot/ni.map';

sub self_append_resource($$) {
  my ($k, $v) = @_;
  $self{$ni_map} .= "\nresource $k";
  $self{$k} = $v;
}

sub lib_entries($$) {
  local $_;
  my ($name, $text) = @_;
  map "$name/$_", grep {s/#.*//; length} split /\n/, $text;
}

sub quote_resource {my @xs; map sprintf("%d %s\n%s", scalar(@xs = split /\n/, "$self{$_} "), $_, $self{$_}), @_}
sub quote_library  {map quote_resource("$_/lib", lib_entries $_, $self{"$_/lib"}), @_}

sub read_map {join '', map "$_\n",
                       (map {my ($c, @a) = split /\s+/;
                               $c eq 'bootcode'    ? ni::boot_header
                             : $c eq 'resource'    ? quote_resource @a
                             : $c =~ /^lib$|^ext$/ ? quote_library @a
                             : die "ni: unknown map command+args: $c @a"}
                        grep {s/#.*//g; length}
                        map split(/\n/), @self{@_}), "__END__"}

sub intern_lib($) {
  my ($l) = @_;
  if (-d $l) {
    set $_, rfc $_ for lib_entries $l, ($self{"$l/lib"} = rfc "$l/lib");
  } else {
    my $k = "lib/$l";
    self_append_resource $k, rfc $l;
    set $k, $ni::self{$k};
  }
}

sub modify_self() {
  die "ni: not a modifiable instance: $0" unless -w $0;
  open my $fh, "> $0" or die "ni: failed to open self: $!";
  print $fh read_map $ni_map;
  close $fh;
}

sub extend_self($$) {
  my ($type, $lib) = @_;
  intern_lib $lib;
  set $ni_map, "$self{$ni_map}\n$type $lib"
    unless grep /^(lib|ext)\s+$lib$/, split /\n/, $self{$ni_map};
}

sub image {read_map $ni_map}
sub image_with(%) {
  my %old_self = %self;
  my %h        = @_;
  $self{$ni_map} .= join '', map "\nresource $_", keys %h;
  @self{keys %h} = values %h;
  my $i = image;
  %self = %old_self;
  $i;
}
