;; Miscellaneous utility math functions.
;; See also utils/fractal.clj, utils/spiral.clj.
(ns utils.math
    (:require ;[clojure.math.numeric-tower :as nt] ; see https://clojureverse.org/t/article-blog-post-etc-about-clojure-math-vs-numeric-tower/9805/6?u=mars0i
              [clojure.math :as math :refer [cos sin tan atan2 sqrt round]]
              [fastmath.stats :as fstats]
              [fastmath.core :as fm]
              [clojure.core :as cc] ; to replace fastmath macros in reduce, map, etc.
              [clojure.string :as st]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(fm/use-primitive-operators)

(defn remove-decimal-pt
  "Given a number, returns a (base-10) string representation of the
  number, but with any decimal point removed.  Also works on existing
  string representations of numbers."
  [x]
  (apply str
         (st/split (str x) #"\.")))

(def pi math/PI) ; I just like it lowercase
(def pi2 (* 2 math/PI)) ; 360
;(defn cos [theta] (Math/cos theta)) ; now using clojure.math wrappers
;(defn sin [theta] (Math/sin theta))
;(defn tan [theta] (Math/tan theta))

(defn cartesian-to-polar
  "Convert Cartesian coordinates from x, y to [radius, angle]."
  [[^double x ^double y]]
  [(sqrt (+ (* x x) (* y y))), (atan2 y x)]) ; note args to atan must be backwards

(defn polar-to-cartesian
  "Convert polar coordinates from radius r and angle theta to a
  pair of points [x y]."
  [[^double r ^double theta]]
  [(* r (cos theta)) (* r (sin theta))])

(comment
  ;; How to convert from and back to polar coordinates:
  (let [original-r 5
        original-theta 0.5
        [x y] (polar-to-cartesian original-r original-theta)
        [new-r new-theta] (cartesian-to-polar x y)]
    [x y original-r new-r original-theta new-theta])
)

(def ln math/log) ; alias so I don't have to remember whether log is ln or is arbitrary base
(defn log-to-base [base x] (/ (math/log x) (math/log base))) ; don't use ln--confuses compiler optimization


(defn rotate
  "Given an angle theta in radians and a pair of coordinates [x y], returns
  a new pair of coordinates that is the rotation of [x y] by theta."
  [^double theta [^double x ^double y]]
  [(- (* x (cos theta))
      (* y (sin theta))) ,
   (+ (* y (cos theta))
      (* x (sin theta)))])


(defn distance-2D
  "Computes distance between two-dimensional points [x0 y0] and [x1 y1]
  using the Pythagorean theorem."
  [[^double x0 ^double y0] [^double x1 ^double y1]]
  (let [xdiff (- x0 x1)
        ydiff (- y0 y1)]
  (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))


;; Implements $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}\;$  given $\;ax^2 + bx + c = 0$.
;; (If both results are routinely needed inside a tight loop, consider making
;; a version of this function that returns both of them.)
(defn quadratic-formula
  "Returns the result of the quadratic formula applied to the coefficients in
  ax^2 + bx + c = 0.  plus-or-minus should be one of the two functions: + - ."
  [plus-or-minus ^double a ^double b ^double c]
  (let [root-part (sqrt (- (* b b) (* 4 a c)))
        negb (- b)
        a2 (* 2 a)]
    (/ (plus-or-minus negb root-part) a2)))


(defn bool-to-bin
  "Returns 1 if x is truthy, 0 if it's falsey."
  [x]
  (if x 1 0))

(defn sign
  [x]
  (cond (pos? x) 1
        (neg? x) -1
        :else 0))

;; Note that Java's Double/isInfinite and Float/isInfinite don't distinguish
;; between ##Inf and ##-Inf.
(defn pos-inf?
  "Returns true if and only if x is ##Inf."
  [x]
  (= x ##Inf))

;; Added to Clojure in 1.11
;; Just a wrapper for Double/isNaN
;(defn NaN?
;  "Returns true if and only if x is ##NaN."
;  [x]
;  (Double/isNaN x))

(defn slope-from-coords
  "Given a pair of points on a line, return its slope.  This is also the
  vector direction from the first point to the second.  If the line is
  vertical, returns ##Inf (infinity) to indicate that."
  (^double [[^double x1 ^double y1] [^double x2 ^double y2]]
   (if (== x1 x2)
     ##Inf ; infinity is what division below would give for the vertical slope
     (/ (- y2 y1) (- x2 x1))))
  (^double [^double x1 ^double y1 ^double x2 ^double y2]
   (if (== x1 x2)
     ##Inf ; infinity is what division below would give for the vertical slope
     (/ (- y2 y1) (- x2 x1)))))

;; y = mx + b  so  b = y - mx
(defn intercept-from-slope
  "Given a slope and a point on a line, return the line's y intercept."
  [^double slope [^double x ^double y]]
  (- y (* slope x)))

;; CONSIDER REPLACING WITH SIMILAR FUNCTIONS IN fastmath
(defn equalish?
  "True if numbers x and y are == or are within (* n-ulps ulp) of
  each other, where ulp is the minimum of (Math/ulp x) and (Math/ulp y).
  A ulp is \"units in the last place\", i.e. the minimum possible difference
  between two floating point numbers, but the numeric value of a ulp differs
  depending on the number, even within the same numeric class such as double.
  We use the minimum since that's the least difference between one of the
  numbers and the next one up or down from  it.  (It seem as if multiplying a
  number that's one ulp off produces a number that is some power of 2 ulp's
  away from the correct value.) See java.lang.Math for more."
  [^double n-ulps ^double x ^double y]
  (or (== x y)
      (let [xd (double x) ; Math/ulp doesn't work on integers
            yd (double y)
            ulp (min (Math/ulp xd) (Math/ulp yd))]
        (<= (abs (- xd yd))
            (* n-ulps ulp)))))

;(defn old-mean
;  "Returns the mean value of all numbers in collection xs, or the
;  first n values if n is provided.  If n greater than the length of xs,
;  takes the mean of xs."
;  ([xs]
;   (let [n (count xs)]
;     (/ (reduce + xs) n)))
;  ([n xs] (old-mean (take n xs)))) ; don't divide by n explicitly: xs may be short

(defn mean
  "Returns the mean value of all numbers in collection xs, or the
  expectation using corresponding probabilities in weights.  Return
  value is always a double."
  ([xs] (fstats/mean xs))
  ([xs weights] (fstats/wmean xs weights)))

(comment
  (mean [1 2 3 4])
  (mean [1 2 3 4] [0.5 0.25 0.25 0.0])
)

;; If I were to use fastmath's names, I'd probably forget that fastmath's
;; variance is the sample-variance, and that what I normally call variance
;; is fastmath's population-variance.  (fastmath follows Apache's naming convention.)
(def variance
  "[xs] [xs mean]
  Calculate the (population, normal, simple) variance  of the numbers in
  xs, treating each as having probability 1/N: The internal sum is divided
  by N, the size of xs.  If mean is provided, uses that rather than
  calculating the mean value of xs."
  fstats/population-variance)

(def sample-variance
  "[xs] [xs mean]
  Calculate the sample variance  of the numbers in xs. The internal sum is
  divided by N-1, where N is the size of xs.  If mean is provided, uses
  that rather than calculating the mean value of xs."
  fstats/variance)

(comment
  (variance [1 1 2 2])
  (let [xs [1 1 2 2]
        m (mean xs)
        squares (map #(* % %) xs)]
    (- (mean squares) (* m m)))

  (sample-variance [1 1 2 2])
  (let [xs [1 1 2 2]
        m (mean xs)]
    (/ (reduce + (map (fn [x] (let [y (- x m)] (* y y))) xs))
       (dec (count xs))))
)

(defn count-decimal-digits
  "Given a number, returns the number of digits in the decimal
  representation of its integer part."
  [n]
  (count (str (round n))))

(defn strictly-increasing?
  "Returns true iff the numbers in xs are strictly increasing."
  [xs]
  (every? identity (map cc/< xs (rest xs))))

(defn strictly-decreasing?
  [xs]
  "Returns true iff the numbers in xs are strictly descreasing."
  (every? identity (map cc/> xs (rest xs))))

(defn monotonically-increasing?
  [xs]
  "Returns true iff the numbers in xs are monotonically increasing, i.e. if
  every value is greater than or equal to the one before it."
  (every? identity (map cc/<= xs (rest xs))))

(defn monotonically-decreasing?
  [xs]
  "Returns true iff the numbers in xs are monotonically decreasing, i.e. if
  every value is less than or equal to the one before it."
  (every? identity (map cc/>= xs (rest xs))))

(comment
  (monotonically-decreasing? [2 2 2 2 3 3 4 5])
  (monotonically-increasing? [2 2 2 2 3 3 4 5])
  (monotonically-increasing? [2 3 4 5])
  (strictly-decreasing? [2 3 4 5])
  (strictly-increasing? [2 3 4 5])
  (strictly-increasing? [2 2 2 2 3 3 4 5])

  (monotonically-decreasing? [5 4 3 2])
  (strictly-decreasing? [5 4 3 2])
  (monotonically-increasing? [5 4 3 2])
  (strictly-increasing? [5 4 3 2])

  (monotonically-decreasing? [2 1 4 5])
  (strictly-decreasing? [2 1 4 5])
  (monotonically-increasing? [2 1 4 5])
  (strictly-increasing? [2 1 4 5])

  (monotonically-decreasing? [5 3 3 2 1 1])
  (strictly-decreasing? [5 3 3 2 1 1])
  (monotonically-increasing? [5 3 3 2 1 1])
  (strictly-increasing? [5 3 3 2 1 1])
)

;;;;;;;;;;;;;;;;;;;;;;;

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
      (/ (* alpha (math/pow xm alpha))
         (math/pow x (inc alpha)))))

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
        (* (math/pow x (- mu)) (math/pow r mu-) mu-))))
)


(fm/unuse-primitive-operators)
