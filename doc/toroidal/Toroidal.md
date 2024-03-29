Usage notes and code notes for forage/toroidal.clj
==
Marshall Abrams

**These are algorithms that help to implement a toroidal environment,
i.e.  one with periodic boundary conditions, for paths composed of line
segments.**  This code is primarily oriented toward *display* of paths,
but could be used in modeling, i.e. prior to display, with some
additional steps.


Not every function is discussed here--only the ones whose operation
seems less obvious to me.

---

### Simple usage examples:

```clojure
(toroidal-to-cljplot (wrap-path -100 100 path))
```
```clojure
(-> (wrap-path bound-min bound-max path)
    (toroidal-to-vega-lite "subpath"))
```
*  `path`: A sequence of 2D points, i.e. a sequence of pairs of numbers.
*  `-100`: Value of lower/left boundary of the region to wrap to, in both
        dimensions.
*  `100`:  Value of upper/right value of the region to wrap to, in both
        dimensions.
*  `"subpath"`: initial string for values of `"label"` key, which are used
             to distinguish different sub-paths.
The result will be a sequence that can be used in a library like cljplot
or Vega-Lite, respectively in a plot of a random walk (or something
similar), where the line segments representing the walk may go outside
the official boundaries.  Segments that cross a boundary will be
duplicated so that they come back in at the opposite side, and the walk
plot continues from there.

---

### General notes, vocabulary and notation

- The "standard region" or "region" is the area that counts--it's what 
  would be displayed, or it's the environment in which agents move.  
- "seg" means line segment, i.e. a pair of coordinate pairs.
- "bound-" means "boundary-", or as in "upper bound". 
  (It has nothing to do with binding.)
- "sh" means "shifted".
- "dir" means "direction".
- sequences of points (coordinate pairs) are called either "points" or "pts".
- "wrap" as in "wrap around": cause a line that leaves the standard region 
  on one side to come back in on the other side.

The result we want is that we can at least display lines as "wrapping"
back to the other side of the region if they exist out at one side.
However, this might involve creating new segments with parts outside of
the region, with the assumption that those parts will get truncated
later, perhaps by external display routines.

In this version of this code, the standard region must be a square that
lies between bound-min and bound-max in each of the two dimensions.

Many of the functions operate on segments, i.e. pairs of pairs of
coordinates, which have to be repeatedly decomposed and reconstructed.
Although this is slightly inefficient, it made the code easier to
understand, I felt.  If the inefficiency really matters (seems
unlikely), the code could be rewrite in terms of points or raw
coordinates.

**I recommend that one begin reading the code at either `wrap-segs` or
`wrap-paths`. The second is a wrapper (in a different sense) for the
first.**


--- 

## `wrap-segs` algorithm

Note: This algorithm and the one in `seg-dirs` implement an enhanced
version of generateme's `correct-path` function
at:<br/>
https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104<br/>
or:<br/>
https://github.com/generateme/cljplot/blob/f272932c0228273f293a834e6c19c50d0374d3da/sketches/examples.clj#L572


#### General information:

The input to `wrap-segs` is a sequence of segments, i.e. pairs of
coordinate pairs where the second point of each segment is the first
point of the next.

The first point in a segment is its start point; the second is its
ending point or "forward point".

As we walk through the segments, some segments will have their position
linearly shifted.  This occurs in `wrap-segs` if the first point in the
path is outside the standard region.  After that it, it might occur in
`shift-segs`, which is called by `wrap-segs`.  Whenever there's a shift,
this shift persists and will be applied to all subsequent segments (unless a
later shift undoes previous shifts).

#### Initial shifting:

If the first point of the first segment is outside the standard region,
it will be shifted into the region by an amount equal to the
width/height of the region.  That is, it's "wrapped" before anything
starts.  This also means that everything else in the sequence after that
should be shifted by the same amounts, initially.  And then after that,
other shifts may occur.


#### Logic of rest of function:

##### If after applying the current shift, the end point of a segment *is* within the region:

We recurse with shifted segment (`new-seg`), which is `conj`ed onto the
end of output vector `new-segs`, the current shifts are passed as is (in
the variables `new-sh-x` and `new-sh-y`), and we drop the first segment
in the input sequence so that we can process the rest of it.

Note that it is the call to the `choose-shifts` that determines that
the current shifts will be passed on as is.  When the end point of the
(shifted) segment is within the boundaries, it returns `[0, 0]`, so
that both `x-sh-dir` and `y-sh-dir` will be equal to 0.  This causes
the new shift values, `new-sh-x` and `new-sh-y`, to be equal to the
old values passed in `sh-x` and `sh-y`.


##### If after applying the current shift, the end point is *not* within the region, then:

* We still add the shifted segment `new-seg` onto the end of the
  output sequence `new-segs`, but
* we also `conj` on a delimiter (`nil`), and
* as before, we pass on `new-sh-x` and `new-sh-y`, but
* we *do not remove the current segment from the input sequence*
  to be processed next.

Because the end point of the current segment (after applying the current
shifts) is not within the region, we are going to "duplicate" the
segment, but with additional shifts.  The goal is to shift it the width
of the region in one direction or both, so that the new,
additionally-shifted segment will emerge on the opposite side from where
the current segment exited the region.

As above, the new shift values in `new-sh-x` and `new-sh-y` are a
function of the return value of `choose-shifts`.  In this case, at
least one of `x-sh-dir` and `y-sh-dir` will have the value of either
-1 or 1.  This will be multiplied by the `width` of the region and
added to the previous shift value.  Then either `new-sh-x` or `new-sh-y`
will be different fromn `sh-x` or `sh-y`.  Thus one or more new shift
values will be passed on, and the second `recur` call will be run
instead of the first one.

We may have to apply this procedure multiple times, until the last
added, "duplicate" segment has its end point within the region.  Later,
the segments may be cut at the boundaries of the region (perhaps only
by a display routine), and this will create the effect of lines
"wrapping" around toroidally, i.e. with periodic boundary conditions.

---

### `choose-shifts` algorithm

It may be helpful to look at doc/exceedingboundaries1.pdf, which
illustrates the third case below, and might also help one to think
through the other two cases.

Each segment treated by this function is assumed to have its
first point within `bound-min` and `bound-max` in both the x and y
dimensions.  Then the segment either:
  1. Does not exceed the boundaries at `bound-min` and `bound-max`
     in any direction.  Then `[x-dir,y-dir]` in the code = `[0,0]`
     In this case the shift directions to be returned are `[0,0]`.
  2. Exceeds one but not the other; i.e. the segment crosses one
     boundary. Then `[x-dir,y-dir]` will be = `[d,0]` or `[0,e]`, where
     `d`, `e` are -1 or 1.  The shift direction to be returned will be
     `[-d,0]` or `[0,-e]`, respectively.
  3. Exceeds both boundaries: `[x-dir,y-dir]` = `[d,e]`.
     Then either:

     a. The segment crosses through one boundary, and exceeds the other
        simply because after crossing the boundary, it goes so far
        in a diagonal direction that its endpoint is past `bound-min`
        or `bound-max` in that second dimension.  Then we want to shift
        in the first dimensionm but not the second.
        *(See doc/exceedingboundaries1.pdf for a graphical illustration.)*
        To determine whether to return `[-d,0]` or `[0,-e]`, we generate
        the formula for a line running through the segment, and see
        whether the value of the line function at `bound-min`, or `bound-max`
        (depending on which is relevant) in one dimension is between
        `bound-min` and `bound-max` in the other dimension.  If so, then
        the segment crosses through the first border, and should be
        shifted back so that the next variant of the segment is less
        likely (so to speak) to cross the border.  This test may have
        to be done in both dimensions.
	Or:

     b. The segment crosses through both boundaries where they meet,
        i.e. at a corner.  Then we shift in both dimensions, returning
        `[-d,-e]`.

---

## `segs-to-points` algorithm

Input is a sequence of segments, perhaps delimited by `nil`s, which
divide distinct subsequences that needn't connect at their endpoints.

Output is a sequence of points (pairs), from those segments, delimited
by `nil`s at the "same" locations.  Essentially, all we're doing is
removing points from the segments where one segment's end point and the
next segment's start point are the same.  This has to be done with a little
bit of care because the `nil`'s indicate places where this procedure
should not be followed.

The general rule is:

* Always take only the second point of each segment (which is normally the
first point of the next segment),

*except*

* At the beginning, we need to add on the first point in the first segment,

and

* After a nil, which delimits duplicated-but-shifted points
we also need to add on the first point in the segment after the nil.
