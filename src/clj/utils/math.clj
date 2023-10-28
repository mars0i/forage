;; Miscellaneous utility math functions.
;; See also utils/fractal.clj, utils/spiral.clj.
(ns utils.math
    (:require ;[clojure.math.numeric-tower :as nt] ; see https://clojureverse.org/t/article-blog-post-etc-about-clojure-math-vs-numeric-tower/9805/6?u=mars0i
              [clojure.math :as math :refer [cos sin tan atan2 sqrt round]]
              [fastmath.stats :as fstats]
              [fastmath.core :as fm]
              [ham-fisted.api :as hamf]
              [clojure.core :as cc] ; to replace fastmath macros in reduce, map, etc.
              [clojure.string :as st]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(fm/use-primitive-operators)

(def ^:const +steep-slope-inf+ 
  "If a slope is greater than this value, the x and y coordinates will
  be swapped temporarily and then unswapped later.  This is a way to
  deal with both truly vertical slopes (slope = ##Inf) and slopes that are
  so close to vertical that moving through a line segment with this slope
  will be problematic.  It also sidesteps the problem of identifying slopes
  that are actually vertical, but don't appear so because of float slop."
  1.0)


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
    (/ (double (plus-or-minus negb root-part)) a2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC LINEAR GEOMETRY FUNCTIONS

(defn distance-2D
  "Computes distance between two-dimensional points [x0 y0] and [x1 y1]
  using the Pythagorean theorem."
  ^double [[^double x0 ^double y0] [^double x1 ^double y1]]
  (let [xdiff (- x0 x1)
        ydiff (- y0 y1)]
  (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))

(defn distance-2D*
  "Computes distance between two-dimensional points (x0, y0) and (x1, y1)
  using the Pythagorean theorem."
  ^double [^double x0 ^double y0 ^double x1 ^double y1]
  (let [xdiff (- x0 x1)
        ydiff (- y0 y1)]
  (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))


(defn slope-from-coords
  "Given a pair of points on a line, return its slope.  This is also the
  vector direction from the first point to the second.  If the line is
  vertical, returns ##Inf (infinity) to indicate that."
  [[^double x0 ^double y0] [^double x1 ^double y1]]
  (if (== x0 x1)
    ##Inf ; infinity is what division below would give for the vertical slope
    (/ (- y1 y0) (- x1 x0))))

(defn slope-from-coords*
  "Given a pair of points on a line, return its slope.  This is also the
  vector direction from the first point to the second.  If the line is
  vertical, returns ##Inf (infinity) to indicate that."
  [^double x0 ^double y0 ^double x1 ^double y1]
  (if (== x0 x1)
    ##Inf ; infinity is what division below would give for the vertical slope
    (/ (- y1 y0) (- x1 x0))))

;; y = mx + b  so  b = y - mx
(defn intercept-from-slope
  "Given a slope and a point on a line, return the line's y intercept."
  [^double slope [^double x ^double y]]
  (- y (* slope x)))

(defn intercept-from-slope*
  "Given a slope and a point on a line, return the line's y intercept."
  [^double slope ^double x ^double y]
  (- y (* slope x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIN POINT ON SEGMENT
;; Functions for calculating the point on a line segment with the mininum
;; distance to another point.  
;; See distanceToAlineSegment.md for derivation.

;; TODO Do I have to provide special handling for vertical lines?

(defn on-seg?
  "Returns true iff (p,q) is on the line segment from (x0,y0) to (x1,y1),
  inclusive.  All scalars should be doubles."
  [x0 y0 x1 y1 p q]
  (and (<= ^double x0 ^double p ^double x1)
       (<= ^double y0 ^double q ^double y1)))

;; FIXME NOT RIGHT  I think my derivation in the md file is wrong.
(defn project-pt-on-line
  "Given a line with slope m and y-intercept b, return the point on the
  line with the minimum distance to point (p,q)."
  [^double m ^double b ^double p ^double q]
  ;(prn m b p q) ; DEBUG
  (let [x (/ (+ (* m (- q b)) p)
             (+ (* m m) 1))
        y (+ (* m x) b)]
    ;(prn x y) ; DEBUG
    [x y]))

;; Version 1, uses on-seg?
#_
(defn near-pt-on-seg
  "Given a line segment from (x0,y0) through (x1,y1), with slope m and
  y-intercept b, return the point on the segment with minimum distance to
  point (p,q).  This will be one of the endpoints if the minimum point is
  not on the segment.  If the slope m and intercept b aren't provided,
  they'll be calculated from the two endpoints."
  ([x0 y0 x1 y1 p q]
   (let [actual-m (slope-from-coords* x0 y0 x1 y1)
         steep (or (infinite? slope)
                   (> (abs actual-m) +steep-slope-inf+))
         m (if steep (/ actual-m) actual-m)
         b (intercept-from-slope* m x0 y0)]
     (near-pt-on-seg x0 y0 x1 y1 m b p q)))
  ([x0 y0 x1 y1 m b p q]
   (let [min-pt (project-pt-on-line m b p q)
         min-x (min-pt 0)
         min-y (min-pt 1)]
     ;(println min-pt (on-seg? x0 y0 x1 y1 min-x min-y)) ; DEBUG
     (cond (on-seg? x0 y0 x1 y1 min-x min-y) [min-x min-y] ;; TODO This test is wasteful; I don't need four <= tests.
           (< (distance-2D* x0 y0 min-x min-y)  ;; TODO don't need the sqrt!
              (distance-2D* x1 y1 min-x min-y)) [x0 y0] ; the min point is closer to (x0,y0)
           :else [x1 y1]))))

;; Version 2, tests x endpoints independently without using on-seg?
#_
(defn near-pt-on-seg
  "Given a line segment from (x0,y0) through (x1,y1), with slope m and
  y-intercept b, return the point on the segment with minimum distance to
  point (p,q).  This will be one of the endpoints if the minimum point is
  not on the segment.  If the slope m and intercept b aren't provided,
  they'll be calculated from the two endpoints."
  ([x0 y0 x1 y1 p q]
   (let [m (slope-from-coords* x0 y0 x1 y1)
         b (intercept-from-slope* m x0 y0)]
     (near-pt-on-seg x0 y0 x1 y1 m b p q)))
  ([x0 y0 x1 y1 m b p q]
   (let [proj-pt (project-pt-on-line m b p q)
         ^double proj-x (proj-pt 0)
         ^double proj-y (proj-pt 1)]
     ;; Since proj-pt is a projection onto the line y = mx + b, if it's
     ;; not in the interval of the line segment, its x coord is < the 
     ;; left endpoint of the segment, or > the right endpoint of the 
     ;; segment. No need to test the y coordinates as well--unless the 
     ;; line is vertical or close to vertical, in which case testing the 
     ;; y coordinate alone is enough: TODO .  So if the x coordinate is
     ;; beyond the x ends of the line segment, choose to use the nearest 
     ;; endpoint rather than the projection:
     (cond (< proj-x ^double x0) [x0 y0]
           (> proj-x ^double x1) [x1 y1]
           :else [proj-x proj-y]))))

;; Version 3, like v2, but when a seg goes from left to right, reverses
;; which endpoint is considered the minimum.  This caused problems in
;; application of v1 and v2.
#_
(defn near-pt-on-seg
  "Given a line segment from (x0,y0) through (x1,y1), with slope m and
  y-intercept b, return the point on the segment with minimum distance to
  point (p,q).  This will be the projection onto the line, or one of the
  endpoints if the projected point is not on the segment.  If the slope m
  and intercept b aren't provided, they'll be calculated from the two
  endpoints."
  ([x0 y0 x1 y1 p q]
   (let [m (slope-from-coords* x0 y0 x1 y1)
         b (intercept-from-slope* m x0 y0)]
     (near-pt-on-seg x0 y0 x1 y1 m b p q)))
  ([x0 y0 x1 y1 m b p q]
   (let [proj-pt (project-pt-on-line m b p q)
         ^double proj-x (proj-pt 0)
         ^double proj-y (proj-pt 1)
         ;; IS THERE A BETTER WAY?:  e.g. choose the comparison operator. cf. joinr's and cnuernber's walks.clj
         left-x (min (double x0) (double x1))
         right-x (max (double x0) (double x1)) ; use prev result to determine this one? 
         left-y (if (= left-x x0) y0 y1) ; is there a better way?  TODO
         right-y (if (= right-x x0) y0 y1)] ; See joinr's and cnuernber's walks.clj
     ;; Since proj-pt is a projection onto the line y = mx + b, if it's
     ;; not in the interval of the line segment, its x coord is < the 
     ;; left endpoint of the segment, or > the right endpoint of the 
     ;; segment. No need to test the y coordinates as well--unless the 
     ;; line is vertical or close to vertical, in which case testing the 
     ;; y coordinate alone is enough: TODO .  So if the x coordinate is
     ;; beyond the x ends of the line segment, choose to use the nearest 
     ;; endpoint rather than the projection:
     (cond (< proj-x left-x)  [left-x left-y]
           (> proj-x right-x) [right-x right-y]
           :else [proj-x proj-y]))))

;; V.4 
;; Notes on code below:
;; Since proj-pt is a projection onto the line y = mx + b, if it's
;; not in the interval of the line segment, its x coord is < the 
;; left endpoint of the segment, or > the right endpoint of the 
;; segment. No need to test the y coordinates as well--unless the 
;; line is vertical or close to vertical, in which case testing the 
;; y coordinate alone is enough, but in that case we swap x and y
;; first: So if the x coordinate is
;; beyond the x ends of the line segment, choose to use the nearest 
;; endpoint rather than the projection.
(defn near-pt-on-seg
  "Given a line segment from (x0,y0) through (x1,y1), with slope m and
  y-intercept b, return the point on the segment with minimum distance to
  point (p,q).  This will be the projection onto the line, or one of the
  endpoints if the projected point is not on the segment.  If the slope m
  and intercept b aren't provided, they'll be calculated from the two
  endpoints."
  ([x0 y0 x1 y1 p q]
   (let [m (slope-from-coords* x0 y0 x1 y1)
         b (intercept-from-slope* m x0 y0)]
     (near-pt-on-seg x0 y0 x1 y1 m b p q)))
  ([x0 y0 x1 y1 m b p q]
   (let [steep (or (infinite? m)
                   (> (abs m) +steep-slope-inf+))
         ^double m m ; get rid of reflection warning on next line
         m (if steep (/ m) m)
         ^doubles data (if steep  ; based on cnuernber's version of walk.clj
                         (hamf/double-array [y0 x0 y1 x1]) ; swap x and y
                         (hamf/double-array [x0 y0 x1 y1])) ; make no change
         x0 (aget data 0)
         y0 (aget data 1)
         x1 (aget data 2)
         y1 (aget data 3)
         proj-pt (project-pt-on-line m b p q)
         ^double proj-x (proj-pt 0)
         ^double proj-y (proj-pt 1)
         ;; IS THERE A BETTER WAY?:  e.g. choose the comparison operator. cf. joinr's and cnuernber's walks.clj
         left-x (min (double x0) (double x1))
         right-x (max (double x0) (double x1)) ; use prev result to determine this one? 
         left-y (if (= left-x x0) y0 y1) ; is there a better way?  TODO
         right-y (if (= right-x x0) y0 y1) ; see joinr's and cnuernber's walks.clj
         near-pt (cond (< proj-x left-x)  (if steep [right-y right-x] [left-x left-y])
                       (> proj-x right-x) (if steep [left-y left-x] [right-x right-y])
                       :else (if steep [proj-y proj-x] [proj-x proj-y]))]
     near-pt)))


(comment
  (project-pt-on-line 1 0 2 2)
  (near-pt-on-seg 0 0 5 5 3 2)
  (near-pt-on-seg 0 0 1 5 3 2)

  (require '[utils.random :as r])
  (require '[forage.core.walks :as w])
  (require '[forage.core.env-mason :as env])
  (require '[forage.viz.hanami :as h])
  (require '[aerial.hanami.common :as hc])
  (require '[aerial.hanami.templates :as ht])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def seed 1334567890123456)
  (def rng (r/make-well19937 seed))
  (def maxlen 200)
  (def levy-vecs (w/make-levy-vecs rng (r/make-powerlaw rng 1 1.25) 5 100)) ; an infinite seq
  (def walk (w/walk-stops [80 80] (w/vecs-upto-len maxlen levy-vecs))) ; seq of points summing to maxlen
  (count walk) ; 9

  ;; Bundled test plot construction function
  (defn testmin
    "Walk is a series of connected line segments. p and q are to be
    coordinates of a single foodspot.  Point colors are labeled with
    endpoints of the line segment the point is supposed to correspond to
    it--i.e. that's supposed to be the point on the segment that has
    minimum distance to the foodspot."
    [walk p q]
    (let [walk- (rest walk)
          env (env/make-env 5 170 [[p q]])
          vega-min-pts (map (fn [[x0 y0] [x1 y1]]
                              (let [[x y] (near-pt-on-seg x0 y0 x1 y1 p q)]
                                {"x" x
                                 "y" y
                                 "label" (str (mapv round [x0 y0 x1 y1]))}))
                            walk walk-)
          env-plot2 (h/vega-env-plot env 600 1.5 :extra-pts vega-min-pts)
          plot (h/vega-envwalk-plot env 600 1.0 1.5 walk :env-plot env-plot2)]
      plot))

  ;; TODO using v4, this looks good, but there appears to be a missing projection point.
  ;; It's missing from the third segment.  The second and third are sharing
  ;; the same min-pt at an apex, but it shouldn't be there fore the third
  ;; segment; there should be a projection into the middle of the segment:
  (oz/view! (testmin walk 97 98)) ; mostly working?
  ;; This exhibits a similar problem on 6th segment.  These are both
  ;; somewhat vertical segments:
  (oz/view! (testmin walk 97 138)) ; mostly working?
  (oz/view! (testmin (take 2 (drop 2 walk)) 97 118)) ; mostly working?
  ;; These don't seem to be working correctly. Why?:
  (def seg (take 2 (drop 4 walk)))
  (oz/view! (testmin seg 97 118))
  (def seg* [[(first (first seg)) 115] (second seg)])
  (def seg* [[(first (first seg)) 15] (second seg)])
  (def seg* [[(first (first seg)) 15] [65 150]])
  (oz/view! (testmin seg* 97 118))
  ;; TODO This seems to have a problem:  (Is it just float slop??  Maybe it's the steepnees.)
  (oz/view! (testmin (take 2 (drop 0 walk)) 97 118)) ; mostly working?

  (oz/view! (testmin walk 100 78)) 

  ;; Why isn't this working?  Not it, is. The projeciton is just to the
  ;; right of the segment.
  (def seg (take 2 (drop 4 walk)))
  (oz/view! (testmin seg 100 78)) 


  ;; standalone
  (def walk- (rest walk))
  (def p 35)
  (def q 70)
  (def vega-walk (h/add-point-labels "walk" walk))
  (def min-pts (map (fn [[x0 y0] [x1 y1]] (near-pt-on-seg x0 y0 x1 y1 p q)) walk- walk))
  (def min-pts (map (fn [[x0 y0] [x1 y1]] (near-pt-on-seg x0 y0 x1 y1 p q)) walk walk-))
  (def vega-min-pts (h/add-point-labels "min-pt" min-pts))
  (def env-plot2 (h/vega-env-plot env 600 1.5 :extra-pts vega-min-pts))
  (def plot (h/vega-envwalk-plot env 600 1.0 1.5 walk :env-plot env-plot2))
  (oz/view! plot)

  ;(def min-pts (map (fn [[x0 y0] [x1 y1]] (near-pt-on-seg x0 y0 x1 y1 p q)) walk walk))
  ;(def vega-min-pts-plus-food (into vega-min-pts [{"x" p "y" q "label" "food"}]))
  ;(def env-plot (h/vega-env-plot env 600 1.5))
  ;(def min-pts-plot (h/vega-food-plot vega-min-pts 120 600 1.0))
  ;(def plot (h/vega-food-plot vega-min-pts-plus-food 120 600 1.5))
  ;(oz/view! env-plot)
  ;(oz/view! min-pts-plot)
  ;(def walk-plot (h/vega-walk-plot 600 130 1.0 vega-walk))
  ;(oz/view! walk-plot)
  ;(def plot (h/vega-envwalk-plot env 600 0.75 2 walk :foodspots-on-top? true))
  ;(def plot2 (hc/xform (ht/layer-chart :LAYER (list min-pts-plot walk-plot env-plot))))
  ;(def plot (h/vega-walk-plot 600 170 0.75 (concat vega-min-pts vega-walk)))
  ;(def plot (merge env-plot walk-plot min-pts-plot))
  ;(def plot (merge env-plot min-pts-plot))


)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
