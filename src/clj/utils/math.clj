(ns utils.math
    (:require [clojure.math.numeric-tower :as nt]))

(declare neg)

;; Pareto PDF: $\mathsf{P}(x) = \frac{\alpha x_m^{\alpha}}{x^{\alpha + 1}}$, again for $x \leq x_m$.
;; (Note that memoizing this makes it slower.  Rearranging to use expt only
;; once also makes it slower.)
(comment
  ;; USE APACHE COMMONS PARETO DISTRIBUTION INSTEAD:
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
)

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
      (* (nt/expt x (neg mu)) (nt/expt r mu-) mu-))))


(defn neg
  "Returns -1 times x."
  [x]
  (* -1 x))

(def pi Math/PI)

(defn cos [theta] (Math/cos theta))
(defn sin [theta] (Math/sin theta))
(defn tan [theta] (Math/tan theta))

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
