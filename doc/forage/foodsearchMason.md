foodsearchMason.md
===

I want to use MASON's `Continuous2D` (or possibly the grid version of
it) to place and look up food.

Afaik, I don't need to set up a whole MASON Sim and GUI class
superstructure to use this.  

See notes in the v20 manual and the `Continuous2D` javadoc on
bucket/discretizatio sizes.  It might be set as a function of
perceptual radius, but not equal to that quantity, I think.

Note that in the manual in the section on neighborhood lookup in a
continuous field, Luke says it's slow.  So (a) I might be able to do
things to speed it up (not searching when the bucket is empty?); (b)
it's probably still pretty fast relative to what I need; (c) I could
go back to working on the algorithms I was developing for a pure
Clojure food finding method.

(But if it's fast enough, it's better than specialized, possibly more
efficient grid-oriented algorithms, because using Continuous2D
generalizes trivially to random foodspots.)

---

With Continuous2D, I suppose that what I will do is run through the
points on a line, checking for nearby foodspots.  This is like a walk in
time, but without the constraint to timestep it.  (This is what I called
a "sausage algorithm" in some earlier notes.)

Note that when you find a foodspot, and then step, and find another
one, you have to check its identity to make sure you're not just
counting the same one twice.

	For rectangular grid located foodspots, this can be sped up
	like this:

		let msep be the separation between foodspots scaled by
		the slope m of the line.

		When food is found, skip to curr point + msep (or a
		little less), and then start searching again.  Because 
		there will be no new foodspots in between.

But then it's not really continuous: I'd have to step through points on
the line in increments.  That would be mostly OK, though, I think.  The
danger is just that where the line is exactly tangent to the perceptual
circle, you might miss it if it's in between check points.  This could
be addressed by:

1. Checking a slightly expanded radius (and then checking to see if the
precise radius is met in addition)

2. Saying well, you know, animal perception is kinda sloppy.  The
animal might have been looking away at that moment, or the wind was
blowing the wrong way.

Or I could use one of Mason's grid classes--maybe the sparse2D class,
which is also hashtable-based.  If you made the grid units very small,
maybe that would be effectively the same.

(And maybe if there was a need to run the simulation in time--e.g. to
put multiple animals interacting--it might be easy to simply introduce
timesteps by doing the checks one at a time in each time step.)

Note also that Mason's `Continous2D` allows storing "non-point"
objects, for which you can check for overlap.  From the javadoc:

	But if the object are non-point location objects (that is, they
	have dimensions of width, height, etc.), and you care about this
	overlap when you do distance lookups, then you have a minimum
	bound on your discretization. In this case, you want to make
	certain that your discretization is at LEAST larger than the
	LARGEST dimension of any object you plan on putting in the
	Continuous2D. The idea here is that if an any part of an object
	fell within the bounding box for your distance lookup task (see
	getNeighborsWithinDistance(...)), you're guaranteed that the
	stored location of the object must be within a bounding box 1
	discretization larger in each direction.

(Note that if you use a toroidal, i.e. wrapping space, Mason can do
nearnest lookups that wrap around boundaries.  Very convenient.)
