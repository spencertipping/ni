# Image processing
**TODO:** Convert these examples to unit tests (this will require adding tools
to the test containers)

ni includes operators that interface with Gnuplot, ImageMagick, and ffmpeg to
produce either single-frame or animated graphs. For example:

```sh
$ ni nE3 p'r a, $_, sin($_ / 1000 * a) for 0..1000' \
     GAP640x480%l GF[-y -qscale 5 freqs.avi]
```

`G<colspec-one>` is an operator that uses gnuplot to render streaming groups of
data, where `colspec-one` specifies which column should be fixed. You can also
use `G:` to consume the whole stream in one frame. `P640x480` is expanded into
`set terminal png size 640,480;` and `%l` expands to `plot "-" with lines`; see
[core/gnuplot/gnuplot.pl](../core/gnuplot/gnuplot.pl) for a full list of these
shorthands.

`GF[...]` is an operator that runs ffmpeg in a way that consumes PNG or JPG
images on stdin; it's equivalent to `e[ ffmpeg -f image2pipe -i - ... ]`.

## Image iteration
If you've got a series of PNG images like those emitted by `G`, you can do a
couple of things with that. One option is to pipe each one through a ni lambda
using the `I` operator:

```sh
# show the size of each PNG
$ ni nE3 p'r a, $_, sin($_ / 1000 * a) for 0..1000' \
     GAP640x480%l Ie'wc -c'
```

More interesting is to composite images using ImageMagick.

## Compositing
The `IC` operator takes three command-line fragments that will be used as
arguments to `convert`, where `:` means "do nothing":

```sh
$ ni nE3 p'r a, $_, sin($_ / 1000 * a) for 0..1000' \
     GAP640x480%l IC: [-compose blend -define compose:args=50,50 -composite] : \
     GF[-y -qscale 5 composited.avi]
```

The three commands, `init`, `reduce`, and `emit`, work like this:

```
for the first image:
  pipe it into "convert - $init $tempfile"
  run "convert $tempfile $emit"
for each subsequent image:
  pipe it into "convert $tempfile - $reduce $tempfile"  # note below
  run "convert $tempfile $emit"
```

Technically you can't run `convert` with the output image set to any of the
inputs, so ni allocates a second temp image and uses `mv` to shuffle the files
around accordingly.

Intuitively what's going on here is that we have an image file that we update
each frame, then we emit some part of it. This might seem overly complicated,
but it lets you do some cool stuff; for example, you could use `+append` and
cropping to store a window of frames and have an emitter that collapsed them in
arbitrary ways.
