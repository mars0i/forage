xy-shifts.md
===

Notes on the logic, history, etc. of `forage.walks/xy-shifts`
and its interaction with `forage.walks/find-in-seg`.

----------------

`find-in-seg` is given a line segment--part of an animal's search
path--and it moves along it in small distances, $\epsilon$, and every
$\epsilon$ it stops and calls `look-fn` to see whether there is a
foodspot that's visible.  If so, the search is over along that segment
is over (although it could be restarted by a calling function).

**Note:** `xy-shifts` performs only a *length* decomposition, not a
vector decomposition.  The Pythagoren theorem and the quadratic formula
don't know anything about direction of travel.  I enforce direction below
by taking the absolute value of the decomposed values `x-eps` and `y-eps`
in `xy-shifts`, and then the calling function `find-in-seg` has to transform
these into negative values if necessary to preserve direction of shifts.

----------------

Let $\epsilon$ be the amount to shift along a path to check whether 
there is food visible from the next spot.  This has to be translated 
into shifts along the x and y axes.

We can derive the amount to shift along the x and y axes from these:

$\epsilon^2 = x^2 + y^2$
&nbsp;&nbsp; and &nbsp;&nbsp; 
$y = sx + i$ ,

where $\epsilon$ (`eps` in the code) is the shift amount along the line
segment, $s$ is `slope`, and $i$ is the y `intercept` in the function below.

Therefore

$\epsilon^2 \;\;=\;\;  x^2 + (sx + i)^2 \;\;=\;\; x^2 + s^2x^2 +2six + i^2$,
&nbsp; so:

$0 = (1+s^2)x^2 + 2six + (i^2 - \epsilon^2)$ .

Then we can use the quadratic formula:

$x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a},\;$ with
$a=1+s^2,\;\;\; b=2si,\;\;\; c=i^2-\epsilon^2$ .

This $x$ is the length of the shift along needed the $x$ axis needed
as a component of the original length $\epsilon$.  We can then calculate
the corresponding shift along the $y$ axis using $\;y=sx+i\;$.
(Note using either $+$ or $-$ from $\pm$ will work.  Both
results will be such that $\epsilon^2 = x^2 + y^2\,$.)

It will be informative to see the resulting full calculation:

$x = \frac{-2si \pm \sqrt{4s^2i^2 - 4(1+s^2)(i^2-\epsilon^2)}}{2(1+s^2)}$ 
$= \frac{-si \pm \sqrt{s^2i^2 - (1+s^2)(i^2-\epsilon^2)}}{1+s^2}$

$= \frac{-si \pm \sqrt{s^2i^2 - (i^2 + s^2i^2 - \epsilon^2 - s^2\epsilon^2)}}{1+s^2}$

$= \frac{-si \pm \sqrt{- i^2 +\epsilon^2 + s^2\epsilon^2)}}{1+s^2}$
$= \frac{-si \pm \sqrt{(1+s^2)\epsilon^2 - i^2}}{1+s^2}$

(The `xy-shifts` code calls out to a quadratic formula function rather
than performing the entire calculatin in `xy-shifts`.  That makes the
code clearer.  The trivial efficiency gained by avoiding 
redundant calculations such as $s\times i$ is surely insignificant.)

**Actually, there's no reason to use $i$ in call to the quadratic formula,
since the slope decomposition is the same whatever the y intercept is.
So I'm now setting `intercept` to 0 in `find-in-seg`.**  (I was getting `NaN`'s
before doing that. You can see the reason why in the last formula in the
full calculation above: The intercept $i$ can be arbitrarily large and
often has absolute value greater than 1, in which case $-i^2$ easily makes
the argument of the square root negative--especially since $\epsilon^2$
will always be small.  Taking the square root of a negative number is
one of the cases in which Java generates a `NaN`.)


**If this is correct, then I can simplify both functions.**
Then we'd have

$a=1+s^2,\;\;\; b=0,\;\;\; c=-\epsilon^2$

and the quadratic formula would reduce to

$x \;=\; \frac{\pm\sqrt{-ac}}{a} \;=\; \pm\sqrt{-ca/a^2} \;=\;$
$\pm\sqrt{-c/a} \;=\; \pm\sqrt{\epsilon^2/(1+s^2)}$
$\;=\; \pm\frac{\epsilon}{\sqrt{1+s^2}}$ .

Checking by doing it a different way from the above full calculation:

$x = \frac{0\pm \sqrt{(1+s^2)\epsilon^2 - 0^2}}{1+s^2}$
$= \frac{\pm \sqrt{(1+s^2)}\sqrt{\epsilon^2}}{\sqrt{1+s^2}\sqrt{1+s^2}}$
$= \frac{\pm\,\epsilon}{\sqrt{1+s^2}}$ .

So I'll do that in `xy-shifts`, and not call `quadratic-formula`.
I'll change find-in-seg, too, since it needn't pass the intercept.

Note that in that case there's no need to call absolute value on $x$,
since $\epsilon$, i.e.  `eps` should always be positive, $1+s^2$, i.e.
`(+ 1 (* slope slope))` must be positive, and I'll can choose the postive
square root.
