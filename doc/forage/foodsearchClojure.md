foodsearchClojure.md
===
Notes on pure Clojure foodspot identification algorithms

[background: The Bresenham algorithm, even with modifications, turns
out not to be good for identifying squares in which there might be a
foodspot: the algorithm routinely sends the original line
outside of the boundaries of the generated squares.  There is no
*simple* way to prevent this.]


**For foodspots on a rectangular grid:**

### algorithm 0:

(For foodspots in odd places within squares, additional squares would
have to be added, or larger overlapping squares would be needed.)

Specify the grid in advance.  Squares/blocks have side length s.

First point of line is either on a border or within a square.

If it's within a border, add that square to the list.  
If it's on a border, add all of the adjacent squares (four if it's on a
corner, two if it's on a line between corners).


loop
Find the first point that is on one of the (other) sides (or through a 
corner).  In effect, what you want to do is floor or ceil wrt the
grid width s.  More specifically:

	Use the slope/direction and initial point to calculate the next
	point that crosses a side.  You can get the x coord or y coord
	to check using floor and ceil on x and y.

		Note that this procedure is the same whether you are
		within a square or on its edge.  If the latter, you're
		looking for one of the other sides of the square in
		the direction the vector is pointed.

	So for y=mx+b, where m is positive or negative per the vector
	direction, and I'm at (x0,y0), use (mod x0 s) (or rem) if the
	direction is negative for x, or ((mod x0 s) + s) if the
	direction is positive for x.  mutatis mutandis for y.

	If the resulting new x or y coordinate is beyond a square
	border, then don't use that one--use the other one.  This
	would mean that the line didn't exist the square first in that
	direction. 

If you get to the end of the line segment (vector) before reaching a
border--i.e. the new point is not on the segment, just stop and return
your list so far.

Add the square you are heading into to the list.
recur


### algorithm A:

Instead of thinking of a grid of boxes each centered on a foodspot,
think of the grid lines with the foodspots at intersections.

Then whenever a search path crosses a grid line, there are two nearest
foodspots in either direction: up and down, or right and left.

	when (mod x-pos grid-sep) = 0

		if (mod y-pos grid-sep) < perceptual radius r

			This means that the distance between y-pos and
			the foodspot below is less than perceptual
			radius, so we can see the foodspot below--it's
			been found.

		elseif (grid-sep - (mod y-pos grid-sep)) < perceptual radius r

			This means the distance up to the higher
			foodspot is less than perceptual radious, so
			again we can see the foodspot.

		else do the same test with x and y swapped.
	
	(The only case where both tests will succeed is when the
	search path goes through a foodspot, in which case the first
	test will succeed.)


What's wrong with this test:

A path moving diagonally past a foodspot can be nearer than perceptual 
radius but not within r at the grid lines.


### algorithm B:

A possible solution is to expand the range at which a path is flagged as
possibly close enough, and then back up and figure it out if was or will
be.  However, this expansion depends on the slope of the line.  With a
very extreme slope and a very sparse grid, it might be all of the way
to the next foodspot.

Or consider a path that is parallel to the gridlines in one direction,
passing within r of every foodspot it passes.
No, that case is OK because it will be flagged when it crosses the
perpendicular grid lines.

Um, by the same token, maybe the only case to worry about are
45-degree angles.  Because all of the other ones will be even closer
on the gridline on one side or the other.  i.e. if it's too far along
the x gridline, it will be closer on the y gridline.

So the expansion just has to use the 45-degree case.

The expansion:

	The length along the grid line is the hypothenuse of a right
	triangle with the longer right-angle-adjacent side pointing
	into the foodspot, with length = perceptual radius.

	So the length to check along the grid line is the length of
	the hypotenuse on a 30/60/90 triangle with side length = r

	Specifically, since the closest approach of the diagonal path is
	midway between the two grid lines, the acute-corner angles are
	both 45 degrees, and the distance from the closest point to
	food, along the path to the next gridline, is the same as the
	distance to the foodspot from there.  i.e. it's r

	So the length that flags possible entrance into perceptual
	range is:

		sqrt (2r^2) = r sqrt 2

		Call this value r+


So the new algorithm is:

	when (mod x-pos grid-sep) = 0

		if (mod y-pos grid-sep) < r+ OR? AND? (mod y-pos grid-sep) < r

			(and there's a foodspot at the corner)
		  	we found the lower foodspot along the y gridline

		elseif (grid-sep - (mod y-pos grid-sep)) < r+
		AND, OR, WHAT?
		(grid-sep - (mod y-pos grid-sep)) < r

			(and there's a foodspot at the corner)
		  	we found the higher foodspot along the y gridline

		else do the same test with x and y swapped.

Wait--does this always work?

Here's an argument that it does:

	A 45-deg line that hits a vertical or horizontal at r(sqrt 2) is
	tangent to the circle right in the middle between the two lines
	that intersect its center.

	Other line tangent to the circle in that quadrant of it will
	touch it closer to the vertical or to the horizontal.

	In order to do that, the point on the line hitting the
	vertical/horizontal nearest to the tangent point must be
	closer to the center of the circle (because of the shape of
	circles).  The point hitting the other horiz or vert line will
	be farther, but that's OK, because the test for nearer will
	catch the tangent line.

	So every line that intersects the circle has to hit either the
	horiz or the vert line through center of circle, no farther
	than r(sqrt 2).

	(This is true for horiz or vertical paths as well.) 

Now, that it hits within r(sqrt 2) doesn't guarantee that it hits the
circle.  The angle might send it away from the circle in one
direction, and not close enough to it in the other.  See
linespasttarget2.gcx.


### foodspot tests for algorithm B:

So once you find a line that hits a vertical or horizontal within r(sqrt
2) of a circle's center, you have to test it.  There are a few ways to
do this, afaics:

	(1) Take the derivative of the line's distance from the circle
	center, and find the minimum, i.e. the closest point to the center.  
	Then the line intersects the circle iff the minimum is <= r.

	(2) Draw a line through center perpendicular to the line.
	The angle along this is easily derived from the line's angle.
	Calculate the distance along this line from the center until
	the line being examined.

	(3) Specify some function on angles and points at which lines
	hit the vertical/horizontal, that tells whether it hits the
	circle.


### foodspot test strategy (1): derivative and maxima

Where line is y = mx + b, distance from circle center c,d is
sqrt[(c - mx - b)^2 + (d - y)^2]  (right?):

$$((c - mx - b)^2 + (d - y)^2)^\frac{1}{2}$$

Note

$$(c - mx - b)^2 =  (mx + (c-b))^2 = m^2x^2 - 2(c-b)mx + (c-b)^2$$

with derivative $2m^2x - 2m(c-b)$, and note

$$(d-y)^2 = y^2 -2dy + d^2$$

with derivative $2y-2d$.

The derivative of the first expression is therefore:

$$\frac{1}{2} ((c - mx - b)^2 + (d - y)^2)^{-\frac{1}{2}} (2m^2x -
2m(c-b) +  2y-2d)$$

Um, probably strategy (2) with the perpendicular line is better.
Or maybe it will be equivalent.


### foodspot test strategy (2): intersection with perpendicular

If the slope of a line is $m$, a line is perpendicular to it iff it has
slope $-\frac{1}{m}$.

To find the formula for a line with formula $y=mx+b$, fixing the slope
$m$ and setting $x=x_1$ and $y=y_1$, $b = y_1 - mx_1$.


### algorithm C:

(Based on ideas above.)

bg: Why I am I trying to figure out what grid square the path is
crossing into or leaving?  For foodspots on a rectangular grid, just
check every near foodspot for a line segment specified by its initial
and final coordinates.


	Initial point: $(x1,y1)$
	final point: $(x2,y2)$
	slope $m = \frac{x2-x1}{y2-y1}$.
	sep = distance between foodspots

[warning: beware of double-counting these points when foodpots
are near them, each are shared by two paths.]

We need to specify analogues of `floor` and `ceil` relative to the
slope of the line.  Maybe these should be renamed to something like
"round back" or "round forward".  Also, these are really supposed to
be mod operators, not integer-rounding operators.  Need to fix that.
*In particular*, they should round to the x and y coordinates on which
foodspots might lie.

	let x-ceil = if x1 <= x2, ceil else floor
	let x-floor = if x1 <= x2, floor else ceil
	let y-ceil = if y1 <= y2, ceil else floor
	let y-floor = if y1 <= y2, floor else ceil
	let f(x) = mx+b
	let q = direction perpendicular to m

	for x = (ceil x1); x = x + sep; (floor x2) [inclusive]:
		for y = (mx+b - sep) ; y + sep; (y = upper x, + sep)
			draw line through foodspot x,y with slope q
			solve for intersection with the line segment
			compute distance between that point and foodspot
			test whether it's less than perceptual radius




