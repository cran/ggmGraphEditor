# ggmGraphEditor 0.2.0

* `graphLayout()` gains a `coords` argument to set the initial node positions,
  either as an n x 2 matrix, as a layout function such as
  `igraph::layout_nicely()`, or as one of `"TB"`, `"BT"`, `"LR"`, `"RL"`
  for a layered layout of the directed edges (like `rankdir` in GraphViz).
* The editor interface is more compact (toolbar instead of sidebar, canvas
  filling the window), has a "Fit" button and a slower zoom.

# ggmGraphEditor 0.1.0

* Initial CRAN release.
