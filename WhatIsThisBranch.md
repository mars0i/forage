WhatIsThisBranch.md
===

(ABANDONED BRANCH. I incorporated some ideas from this branch
piecemeal into main branch.  Some assumptions in this branch were
mistaken, though.)

This branch forked from main o0n 3/25/2022 to explore the idea
of flipping x and y more readily in walks.clj.  i.e. currently I flip
them (temporarily) when the slope is vertical or nearly vertical.
Otherwise the math doesn't work in practice, and we end up in an
infinite or near-infinite loop.  

Currently I test for a vertical-ish slope by checking for equality or
near-equality of the x coordinates that are the endpoints of a line
segment.

In this branch I want to explore swapping x and y whenever the absolute
slope is is greater than 45 degrees.  (I also ought to test for slope in
path-with-food rather than find-in-seg, since the former is called fewer
times, and it has the endpoints of a full line segment.)
