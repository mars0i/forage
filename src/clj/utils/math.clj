(ns utils.math
    (:require [clojure.math.numeric-tower :as nt]))

;; Make my code a little prettier, and allow passing as functions:
(def pi Math/PI)
(defn cos [theta] (Math/cos theta))
(defn sin [theta] (Math/sin theta))
(defn tan [theta] (Math/tan theta))

(defn sign
  [x]
  (cond (pos? x) 1
        (neg? x) -1
        :else 0))

(defn rotate
  "Given an angle theta and a pair of coordinates [x y], returns a
  new pair of coordinates that is the rotation of [x y] by theta."
  [theta [x y]]
  [(- (* x (cos theta)) (* y (sin theta)))
   (+ (* y (cos theta)) (* x (sin theta)))])

(defn distance2D
  "Computes distance between two-dimensional points [x0 y0] and [x1 y1]
  using the Pythagorean theorem."
  [[x0 y0] [x1 y1]]
  (let [xdiff (- x0 x1)
        ydiff (- y0 y1)]
  (nt/sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))

;; Implements $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}\;$  given $\;ax^2 + bx + c = 0$.
;; (If both results are routinely needed inside a tight loop, consider making 
;; a version of this function that returns both of them.)
(defn quadratic-formula
  "Returns the result of the quadratic formula applied to the coefficients in
  ax^2 + bx + c = 0.  plus-or-minus should be one of the two functions: + - ."
  [plus-or-minus a b c]
  (let [root-part (nt/sqrt (- (* b b) (* 4 a c)))
        negb (- b)
        a2 (* 2 a)]
    (/ (plus-or-minus negb root-part) a2)))


(defn mean
  "Returns the mean value of all numbers in collection xs, or the
  first n values if n is provided.  If n greater than the length of xs,
  takes the mean of xs."
  ([xs]
   (let [n (count xs)]
     (/ (reduce + xs) n)))
  ([n xs] (mean (take n xs)))) ; don't divide by n explicitly: xs may be short


(comment
  ;; USE APACHE COMMONS PARETO DISTRIBUTION INSTEAD:

;; Pareto PDF: $\mathsf{P}(x) = \frac{\alpha x_m^{\alpha}}{x^{\alpha + 1}}$, again for $x \leq x_m$.
;; (Note that memoizing this makes it slower.  Rearranging to use expt only
;; once also makes it slower.)
  (defn pareto
    "Given a scale parameter x_m (min value, should be positive) and a shape parameter
    alpha (positive), returns the value of the Pareto density function at x
    (from https://en.wikipedia.org/wiki/Pareto_distribution).  Returns 0
    if x < minumum"
    [xm alpha x]
    (if (< x xm)
      0
      (/ (* alpha (nt/expt xm alpha))
         (nt/expt x (inc alpha)))))

; Assuming that $\mu > 1$, 
; $\int_r^{\infty} x^{-\mu} \; dl = \frac{r^{1-\mu}}{\mu-1} \,$.
; &nbsp; So to distribute step lengths $x$ as $x^{-\mu}$ with $r$ as 
; the minimum length,
; $\mathsf{P}(x) = x^{-\mu}\frac{\mu-1}{r^{1-\mu}} = x^{-\mu}r^{\mu-1}(\mu-1)$.
;; &nbsp; See steplengths.md for further details.  &nbsp; cf. Viswanathan et al., *Nature* 1999.
;; This can be viewed as a Pareto distribution, but parameterized differently.
(defn powerlaw
  "Returns probability of x with normalized density x^mu, where r is
  x's minimum value.  Returns 0 if x < minumum."
  [r mu x]
  (if (< x r)
    0
    (let [mu- (dec mu)]
      (* (nt/expt x (- mu)) (nt/expt r mu-) mu-))))
)
