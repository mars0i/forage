`xy-shifts`:

Let $\epsilon$ be the amount to shift along a path to 
check whether there is food visible from the next spot.  This has to
be translated into $x$ and $y$ shifts:

We can derive the amount to shift along the x and y axes from these:

$\epsilon^2 = x^2 + y^2$
&nbsp;&nbsp; and &nbsp;&nbsp; 
$y = mx + b$ .

Therefore

$\epsilon^2 \;\;=\;\;  x^2 + (mx + b)^2 \;\;=\;\; x^2 + m^2x^2 +2mbx + b^2$,
&nbsp; so:

$0 = (1+m^2)x^2 + 2mbx + (b^2 - \epsilon^2)$ .

Then by the quadratic equation,

$x = \frac{-2mb \pm \sqrt{4m^2b^2 - 4(b^2-\epsilon^2)}}{2(1-m^2)}$ 
$= \frac{-mb \pm \sqrt{m^2b^2 - b^2 + \epsilon^2}}{1-m^2}$ .

In the function definition below, $\epsilon$ is called "shift", and 
$m$ and $b$ are called "slope" and "intercept", respectively.

FIXME:
```
(defn xy-shifts
  "Given an incremental shift (vector) in the direction of a line specified 
  by its slope and intercept, return a pair [x-shift y-shift] that give
  the shifts in the x and y directions that would produce the desired shift
  (i.e. the vectors along x and y that would sum to the desired shift)."
  [shift slope intercept]
  (let [-mb (- (* slope intercept))
        -b2 (- (* intercept intercept))
        eps2 (* shift shift)
        part2 (nt/sqrt (+ (* -mb -mb) -b2 eps2))
        x-shift (+ -mb part2) ; TODO Why plus and not minus? FIXME
        y-shift (+ (* slope x-shift) intercept)]
    [x-shift y-shift]))
```
