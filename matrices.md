# Client-side matrix refactor
The client-side view is constructed using a view matrix multiplied with an
object matrix. This works well for trivial rotations/etc, but it's terrible for
cases that require a lot of zooming or spatial searching.

Here's what I'm thinking:

- Any object auto-matrix like normalization needs to be forkable so you can
  stably search even though more data might come in later. (i.e. stream
  dependencies should support snapshots.)
- View matrices != object matrices from a manipulation standpoint: view
  matrices rotate/pan/scale along view axes, whereas object matrices are
  axis-locked.
- Object matrix transforms are done by inverting the view matrix and choosing
  "most likely" axes? (Sure: trace the mouse line through to each axis plane,
  take closest point to resolve ambiguity.)

I also need to think about how this will work with point labeling.

While I'm changing the matrix stuff, I should probably also add some things
like Z-plane volume slicing. Inspecting dense volumes isn't easy right now.
