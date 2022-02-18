WhatIsThisBranch.md
===
This is branch **origin-in-corner**.

Up until 2/16/2022, I always put the origin (0,0) in the center of the
environment.

I was having trouble implementing toroidal paths for Vega-Lite.  The origin in
the center seemed to complicate that, and I noted that MASON's Continuous2D
handles toroidal wrapping under the assumption that the origin is in the
corner.

I decided to try putting the origin in the corner (lower left).  
(I can still start paths in the center at (half-width, half-height).)

This branch explores this option.  As of the creation of this branch, main
still uses origin in the center.
