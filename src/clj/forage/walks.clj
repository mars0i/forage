;; Functions for generating random walks.
;; (Code s/b independent of MASON and plot libs (e.g. Hanami, Vega-Lite).)
(ns forage.walks
    (:require [utils.math :as m]
              [utils.random :as r]
              [clojure.math.numeric-tower :as nt]))

(def number-of-ulps 
  "This is used as the default number of ulp's for calls to 
  utils/math/equalish? in this namespace.  See the documentation for that
  function."
  (nt/expt 2 20)) ; the resulting tolerance will still be quite small

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERATING RANDOM WALKS

(defn step-vector-fn
  "Returns a function of no arguments that returns a random mathematical 
  vector in the form of pair containing a direction dir, in radians, and a
  length len.  dir is uniformly distributed in [0,2pi] using PRNG instance 
  or distribution instance dir-dist, and length is distributed according to
  distribution instance len-dist.  If low and high arguments are given, the
  distribution is truncated so that lengths fall within [low, high].  (These
  vectors represent steps going from one \"stop\" to the next in a random walk.)
  Example use:
    (def step-vecs (repeatedly 
                     (step-vector-fn (make-well19937)
                                     (make-powerlaw 1 2)
                                     1 100)))"
  ([dir-dist len-dist] (fn [] [(r/next-radian dir-dist)
                               (r/next-double len-dist)]))
  ([dir-dist len-dist low high]
   (fn [] [(r/next-radian dir-dist) (r/next-double len-dist low high)])))

(defn subst-init-dir
  "Given a sequence step-seq of step vectors, i.e. [direction length] pairs,
  return a sequence that's the same except that the direction of the first 
  vector has been replaced by init-dir."
  [init-dir step-seq]
  (let [firststep  (vec (first step-seq))
        othersteps (rest step-seq)
        newstep    (assoc firststep 0 init-dir)]
    (cons newstep othersteps)))


;; TODO I should probably rewrite this with loop/recur.  Using reduce for this
;; is just confusing.
(defn vecs-upto-len
  "Given a desired total path length, and a sequence of step vectors, returns 
  a sequence of vectors, from the front of the sequence, whose lengths sum to 
  at least desired-total.  By default, the lengths are made to sum to exactly
  desired-total by reducing the length in the last step vector.  Add 
  ':trim false' or ':trim nil' to return a sequence with the last vector as it
  was in the input vecs sequence."
  [desired-total vecs & {trim :trim :or {trim true}}]
  (reduce 
    (fn [[tot-len out-vecs] [dir len :as v]]
        (if (< tot-len desired-total)          ; if not yet reached total
          [(+ tot-len len) (conj out-vecs v)]  ; keep conj'ing
          (reduced                             ; otherwise
            (if trim   ; by default shorten last len so tot-len = desired-total
              (let [overshoot (- tot-len desired-total) ; how much > desired?
                    [old-dir old-len] (last out-vecs)
                    newlast [old-dir (- old-len overshoot)]] ; subtract extra
                (conj (vec (butlast out-vecs)) newlast)) ; replace old last
              out-vecs)))) ; return constructed seq as is if trim was falsey
    [0 []]
    vecs))

;; Instead of the following, one could use 
;; (count (vecs-upto-len desired-total vecs))
;; This version is more efficient if you don't yet want to separate out 
;; the relevant vecs, but then you might have to trim the last element
;; later.  I might delete count-vecs-upto-len later.
(defn count-vecs-upto-len
  "Given a desired total path length, and a sequence of step vectors,
  returns the number of steps needed to sum lengths to at least 
  desired-total."
  [desired-total vecs]
  (reduce (fn [[tot-len cnt] [_ len]]
            (if (< tot-len desired-total)
              [(+ tot-len len) (inc cnt)]
              (reduced cnt)))
          [0 0]
          vecs))

(defn next-walk-stop
  "Given a mathematical vector in the form of a direction in radians
  and a length, and (a vector in the form of) a coordinate pair, returns 
  a new coordinate pair that's the result of adding the first vector
  to the second.  (This is the next \"stop\" in a walk.)"
  [[dir len] [prevx prevy]]
  (let [[vecx vecy] (m/rotate dir [len, 0]) ; rotate vector lying on x-axis
        nextx (+ prevx vecx)  ; add vector to prev point
        nexty (+ prevy vecy)]
    [nextx nexty]))

;; TODO Should the `lazy-seq` be outside?
(defn walk-stops
  "Generates a (possibly infinite) sequence of next points from an 
  initial-point and a (possibly infinite) sequence of [direction, length]
  vectors, using each in turn, adding it to the previous point.  
  (These points are the \"stops\" in a random walk.)
  Example use, where step-vecs has been generated by repeated calls to 
  next-walk-fn:
     (walk-stops [0 0] step-vecs)"
  [base-pt step-vectors]
  (cons base-pt 
        (when-let [next-step-vec (first step-vectors)] ; nil if no more step-vecs
          (lazy-seq 
            (walk-stops (next-walk-stop next-step-vec base-pt)
                        (rest step-vectors))))))

(defn vecs-path-len
  "Calculate the length of a path specified by a sequence of vector representations
  in the form of [direction, length] pairs."
  [step-vectors]
  (reduce + (map second step-vectors)))

(defn stops-path-len
  "Calculate the length of a path specified by a sequence of stops, i.e. [x y] 
  coordinate pairs representing endpoints of connected line segments."
  [stops]
  (reduce +
          (map m/distance-2D stops (rest stops))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING FOOD

;; See doc/xyshifts.md for detailed notes about this function and find-in-seg.
(defn xy-shifts
  "Given an incremental shift (vector) in the direction of a line specified 
  by its slope and intercept, return a pair [x-eps y-eps] that give
  the shifts in the x and y directions that would produce the desired shift
  (i.e. the vectors along x and y that would sum to the desired shift)."
  [eps slope]
  (if slope ; if not vertical
    (let [a (+ 1 (* slope slope))
          x-eps (/ eps a)
          y-eps (nt/abs (* slope x-eps))]
      [x-eps y-eps])
    [0 eps]))


;; TODO FIXME ?  Why am I checking for vertical slopes here and not
;; in path-with-food?  Doing it here, it has to happen repeatedly.
;; 
;; See doc/xyshifts.md for notes about this function and xy-shifts.
;; Possibly store slope and/or intercept earlier; they were available
;; when the line pair was created.
(defn find-in-seg
  "Given a pair of endpoints [x1 y1] and [x2 y2] on a line segment,
  and a small shift length, starts at [x1 y1] and incrementally checks
  points along the line segment at every shift length locations, checking 
  to see whether look-fn returns a truthy value representing one or more 
  foodspots from the perspective of that location, or a falsey value if
  no foodspots are found.  look-fn should take a single argument, a
  pair representing the coordinates of a location from which to check
  whether a foodspot is perceptible.  If foodspots are found, this function 
  stops searching and returns a pair in which the first element is the 
  coordinate pair for the location from which the foodspots were perceived,
  and the second element is the representation of the foodspots found, which
  may be a collection of foodspot objects, a collection of coordinates of
  foodspot objects, or some other truthy value.  (The kind of value to be
  returned depends on look-fn, which should reflect the way that this 
  function will be used.)  If no foodspots are found by the time [x2 y2]
  is checked, this function returns nil."
  [look-fn eps [x1 y1] [x2 y2]]
  (let [vertical (m/equalish? number-of-ulps x1 x2) ; 
        [[x1 y1] [x2 y2]] (if vertical  ; vertical slope needs special handling 
                            [[y1 x1] [y2 x2]]    ; swap x and y
                            [[x1 y1] [x2 y2]])   ; otherwise make no change
        slope (m/slope-from-coords [x1 y1] [x2 y2])
        x-pos-dir? (<= x1 x2)
        y-pos-dir? (<= y1 y2)
        [x-eps y-eps] (xy-shifts eps slope)     ; x-eps, y-eps always >= 0
        x-shift (if x-pos-dir? x-eps (- x-eps)) ; correct their directions
        y-shift (if y-pos-dir? y-eps (- y-eps))
        x-comp (if x-pos-dir? > <)   ; and choose tests for when we've 
        y-comp (if y-pos-dir? > <)]  ;  gone too far
    (loop [x x1, y y1]
      (let [food (look-fn x y)]
        (cond food  [food (if vertical [y x] [x y])] ; vertical means we swapped x and y
              (and (= x x2) (= y y2))  nil ; last point. check both: horizontal or vertical lines
              :else  (let [xsh (+ x x-shift)
                           ysh (+ y y-shift)]
                       (recur (if (x-comp xsh x2) x2 xsh) ; search from x2 if xsh went too far
                              (if (y-comp ysh y2) y2 ysh))))))))

(defn path-with-food
  "Returns a vector containing first, found foodspots or nil, and second
  the sequence up to and including the location from which foodspots were
  found, or the entire sequence.  More specifically, given a sequence of
  stops (coordinate pairs) representing a random walk, and a small eps
  length, starts at [x1 y1] and uses find-in-segment to incrementally check
  each line segment defined by pairs of stops to see whether look-fn returns
  a truthy value, meaning that foodspots were found.  The sequence stops must
  contain at least two coordinate pairs.  If foodspots are found, returns a
  pair vector containing: first, the foodspot information returned by look-fn,
  and second, a truncated sequence of stops in which the last element is the
  point from which the food was seen, and remaining points have been removed.
  If no food found in the entire sequence, a pair vector containing nil and
  the unchanged sequence is returned."
  [look-fn eps stops]
  (let [stopsv (vec stops)
        numstops- (dec (count stops))] ; stop inc'ing two consecutive idxs one before length of stops vector
    (loop [i 0, j 1]
      (let [from+foodspots (find-in-seg look-fn eps (stopsv i) (stopsv j))]
        (if from+foodspots               ; all done--found food
          [(first from+foodspots)        ; the found food
           (conj (vec (take j stopsv))      ; replace end of stops with point
                 (second from+foodspots))]  ; on path from which food found
          (if (< j numstops-)
            (recur (inc i) (inc j))
            [nil stops])))))) ; no food in any segment; return entire input

(defn levy-foodwalk
  "Generates a random foodwalk starting from point init-loc in direction
  init-dir, and returns a triple vector containing (a) a sequence of found
  foodspots or nil if none found, (b) the generated sequence from start until
  the point from which the foodspots were found, and (c) the entire generated
  sequence including the stops after the foodspots were found.  More 
  specifically, the generated foodwalk consists of a series of line segments
  and ends where a foodspot is first found, or when the sum of segment
  lengths is equal to maxpathlen.  Food search uses look-fn to repeatedly
  check for food at points that are look-eps apart, beginning from init-loc."
  ([look-fn look-eps init-loc maxpathlen init-dir trunclen rng scale exponent]
   (let [len-dist (r/make-powerlaw rng scale exponent)]
     (levy-foodwalk look-fn look-eps init-loc maxpathlen init-dir trunclen rng len-dist)))
  ([look-fn look-eps init-loc maxpathlen init-dir trunclen dir-dist len-dist]
   (let [inf-step-walk (subst-init-dir  ; lazy
                         init-dir
                         (repeatedly
                           (step-vector-fn dir-dist len-dist 1 trunclen)))
         step-walk (vecs-upto-len maxpathlen inf-step-walk) ; should be a vec
         stop-walk (walk-stops init-loc step-walk) ; lazy if no vec wrapper , at least after first cons
         walk-with-food (path-with-food look-fn look-eps stop-walk)] ; it's a vec, but second element is lazy if food was not found; else it's a vec
     (conj walk-with-food stop-walk))))

(defn straight-foodwalk
  "Generates a straight foodwalk starting from point init-loc in direction
  init-dir, and returns a triple vector containing (a) a sequence of found
  foodspots or nil if none found, (b) the generated sequence from start until
  the point from which the foodspots were found, and (c) the entire generated
  sequence including the stops after the foodspots were found.  More
  specifically, the foodwalk consists of a single line segment, which ends 
  where a foodspot is found or when maxpathlen is reached.  Food search uses
  look-fn to repeatedly check for food at points that are look-eps apart,
  beginning from init-loc."
  [look-fn look-eps init-loc maxpathlen init-dir]
  (let [step-walk [[init-dir maxpathlen]] ; a single step of the whole length
        stop-walk (walk-stops init-loc step-walk) ; contains exacty 2 points
        walk-with-food (path-with-food look-fn look-eps stop-walk)]
    (conj walk-with-food stop-walk)))

(defn length-when-found
  "Given a pair consisting of a possibly empty sequence of found foodspots and a
  path of walk stops until they were found (if they were), returns the length
  of the path, or nil if no foodspots were found.  If the argument is a sequence 
  with more than two elements, its remaining elements are silently ignored."
  [[found-foodspots path-until-found]]
  (if (seq found-foodspots)
    (stops-path-len path-until-found)
    nil))

