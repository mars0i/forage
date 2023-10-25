Distance from a point to a line segment
===

To find the distance from a point to a line segment:

1. Find a point on the line that minimizes the distance.
2. Check whether the minimum falls within the endpoints of the segment.
3. If so, use that distance; otherwise use the endpoint that is closest to the original point.

(Is this algorithm correct?)

Q: Will this work if the line runs through the point?

A: I think so.  It just means that the distance is zero.  (Is that
right?)

---

##### 1. Find point on the line that minimizes the distance:

Since the line is

$$y=mx+b$$

the minimum to $(p,q)$ is the minimum of

$$(x-p)^2 + (y-q)^2$$

since the square root is monotonic.

$$= (x-p)^2 + ((mx+b)-q)^2 =$$

$$(x^2 -2p x + p^2) + ([mx+b]^2 -2q[mx+b] + q^2) =$$

$$(x^2 -2p x + p^2) + (m^2x^2 + 2mbx + b^2 - 2mqx - 2qb + q^2) =$$

$$(m^2+1)x^2 - 2(mb-mq-p)x + (p^2+b^2-2qb+q^2)$$

A minimum or maximum occurs when the first derivative is $0$:

$$2(m^2+1)x - 2(mb-mq-p) = 0$$

i.e. when 

$$x = \frac{mb-mq-p}{m^2+1}$$

This is p minumum since the second derivative is positive when

$$2(m^2+1) > 0$$

i.e. when

$$m^2 > -1 \;,$$

and since $m$ is real, $m^2 \ge 0$.

So the minum point occurs when

$$x = \frac{m(b-q)-p}{m^2+1}$$

and 

$$y \;=\; mx+q \;=\; \frac{m^2(b-q) - mp}{m^2+1} + q$$

i.e. the minimum point on the line is

$$\left(\frac{m(b-q)-p}{m^2+1} \;,\; \frac{m[m(b-q) - p]}{m^2+1} + q\right)$$

where $m$ is the slope, $c$ is the $y$ intercept, and $(p,q)$ is
the point to which we wanted the minimum distance.

---

##### 2. Check whether the minimum falls within the endpoints of the segment:

i.e. determine whether the $x$ above is between the $x$ coordinates of
the endpoints, and whether the $y$ falls between the $y$ coordinates
of the endpoints:

$$x_0 \;\le\; \frac{m(b-q)-p}{m^2+1} \;\le\; x_1$$

and

$$y_0 \;\le\; \frac{m^2(b-q) - mp}{m^2+1} + q \;\le\; y_1$$

---

##### 3. If not, use the endpoint that is closest to the original point:

The latter is the minimum of $d((x_0,y_0),(p,q))$ and 
$d((x_1,y_1),(p,q))$.
