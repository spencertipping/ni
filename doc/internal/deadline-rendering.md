# Deadline rendering
`ni --js` is generally pretty great, but it hasn't historically been as accurate
as it could be at rendering your dataset while you're manipulating it. For
example, here's the old `ni --js` interacting with the [SRTM1
dataset](https://github.com/spencertipping/www/blob/master/srtm.md):

![image](https://github.com/spencertipping/www/raw/master/srtm-images/srtm-scale.gif)

Here's what it looks like with deadline rendering:

![image](https://github.com/spencertipping/www/raw/master/srtm-images/srtm-scale-deadline.gif)

## What's going on?
ni is trying to conserve brightness while rendering to a 30ms deadline. This
isn't quite a trivial problem because ni is also trying to maintain overall
screen luminosity. [Here's the commit for
deadlines.](https://github.com/spencertipping/ni/commit/70a3533ae026b926ac86269a81d91dc9614b7964#diff-4cff1f10c589e60ac4ff80e341dcb557)

Originally, `ni --js` rendered a fixed slice of the data in stripes, and would
cancel the preview render if it received another mouse event while in progress.
This maintained responsiveness, but looked terrible: you'd get variable
luminosity depending on how long each render step was allowed to run.

Deadline rendering fixes this by measuring the light output per slice, the time
per slice, and estimating the remaining time; it then dynamically adjusts the
shading rate to try to hit the same luminosity target as the final render. As a
result, there isn't a huge difference between the preview and final
brightnesses, and there isn't much flicker. There can be some if the browser
can't get enough CPU time and ni gets only one or two slices, but that's not a
common case.
