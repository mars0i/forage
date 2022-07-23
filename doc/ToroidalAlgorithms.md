Algorithms used in forage/toroidal.clj
==

The "standard region" or "region" is the area that counts--it's what
would be displayed, or it's the environment in which agents move.  The
result we want is that we can at least display lines as "wrapping" back
to the other side of the region if they exist out at one side.  However,
this might involve creating new segments with parts outside of the
region, with the assumption that those parts will get truncated later,
perhaps by external display routines.

In this version of this code, the standard region must be a square that
lies between bound-min and bound-max in each of the two dimensions.


---
### `choose-shifts` algorithm

It may be helpful to look at doc/exceedingboundaries1.pdf

Each segment treated by this function is assumed to have its
first point within bound-min and bound-max in both the x and y
dimensions.  Then the segment either:
  1. Does not exceed the boundaries at bound-min and bound-max
     in any direction.  Then [x-dir,y-dir] in the code = [0,0]
     In this case the shift directions to be returned are [0,0].
  2. Exceeds one but not the other; i.e. the segment crosses one
     boundary. [x-dir,y-dir] = [d,0] or [0,e], where d, e = -1 or 1.
     Shift direction to be returned is [-d,0] or [0,-e], respectively.
  3. Exceeds both boundaries: [x-dir,y-dir] = [d,e].
     Then either:
     a. The segment crosses through one boundary, and exceeds the other
        simply because after crossing the boundary, it goes so far
        in a diagonal direction that its endpoint is past bound-min
        or bound-max in that second dimension.  Then we want to shift
        in the first dimensionm but not the second.
        SEE doc/exceedingboundaries1.pdf FOR A GRAPHICAL ILLUSTRATION.
        To determine whether to return [-d,0] or [0,-e], we generate
        the formula for a line running through the segment, and see
        whether the value of the line function at bound-min, or bound-max
        (depending on which is relevant) in one dimension is between
        bound-min and bound-max in the other dimension.  If so, then
        the segment crosses through the first border, and should be
        shifted back so that the next variant of the segment is less
        likely (so to speak) to cross the border.  This test may have
        to be done in both dimensions.
     b. The segment crosses through both boundaries where they meet,
        i.e. at a corner.  Then we shift in both dimensions, returning
        [-d,-e].

--- ## `wrap-segs` algorithm Note: This algorithm and the one in
`seg-dirs` are  derived from generateme's `correct-path` function
at:<br/>
https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104<br/>
or:<br/>
https://github.com/generateme/cljplot/blob/f272932c0228273f293a834e6c19c50d0374d3da/sketches/examples.clj#L572


`wrap-segs` walks along a sequence of segments, i.e. pairs of coordinate pairs where
the second point of each segment is the first point of the next.

The first point in a segment is its start point; the second is its
ending point or "forward point".

We assume that the start point of the first segment is inside the
standard region.

As we go along, some segments will have their position linearly shifted
(by `shift-segs`).  Whenever there's a shift, it will persist and be
applied to all subsequent segments (unless a later shift undoes previous
shifts).

If the end point of a segment is within the region



---
## `segs-to-points` algorithm

Always take only the second point of each segment (which is normally the
first point of the next segment), except:

At the beginning, we need to add on the first point in the first segment,

and

After a nil, which delimits duplicated-but-shifted points
we also need to add on the first point in the segment after the nil.
