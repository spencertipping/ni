@xs = 0..2000;
@ys = 0..2000;

my $x = 0;
my $y;
xloop:
$y = 0;
yloop:
print "$ys[$y]\n";
goto yloop if ++$y < @ys;
goto xloop if ++$x < @xs;
