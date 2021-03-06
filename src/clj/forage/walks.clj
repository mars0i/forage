;; (Code s/b independent of MASON and plot libs (e.g. Hanami, Vega-Lite).)
(ns forage.walks
    (:require [utils.math :as m]
              [utils.random :as r]
              [clojure.math.numeric-tower :as nt]))

;; Try this?
; [fastmath.core :as fm]
; (use-primitive-operators)
; (unuse-primitive-operators)
;; Note find-in-seg passes around the functions <, > .  This
;; might not work with fastmath.


;; Setting this to 1 would give us symmetry between the swapped and
;; unswapped coordinates.  Seems as if it would improve performance
;; slightly to have a higher value, since then the x,y swap operations
;; would happen less often.  However, benchmarking shows otherwise.
;; See steep-slope-inf-benchmarks.txt.
(def steep-slope-inf
  "If a slope is greater than this value, the x and y coordinates will
  be swapped temporarily and then unswapped later.  This is a way to
  deal with both truly vertical slopes (slope = ##Inf) and slopes that are
  so close to vertical that moving through a line segment with this slope
  will be problematic.  It also sidesteps the problem of identifying slopes
  that are actually vertical, but don't appear so because of float slop."
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERATING RANDOM WALKS

(defn step-vector-fn
  "Returns a function of no arguments that returns a random mathematical 
  vector in the form of pair containing a direction dir, in radians, and a
  length len.  dir is uniformly distributed in [0,2pi) using PRNG instance 
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

;; TODO Ideally, I should rewrite this with loop/recur.  Using reduce and
;; reduced is just confusing.
(defn vecs-upto-len
  "Given a desired total path length, and a sequence of step vectors, returns 
  a sequence of step vectors from the front of the sequence, whose lengths sum
  to at least desired-total.  By default, the lengths are made to sum to exactly
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
  "Given an initial point (or a mathematical vector) in the form of a
  coordinate pair, and a mathematical vector in the form of a direction
  in radians and a length, returns a new coordinate pair that's the result
  of adding the vector to the point.  (This is the next \"stop\" in a walk.)"
  [[prevx prevy] [dir len]]
  (let [[vecx vecy] (m/rotate dir [len, 0]) ; rotate vector lying on x-axis
        nextx (+ prevx vecx)  ; add vector to prev point
        nexty (+ prevy vecy)]
    [nextx nexty]))

;; I see no straightforward way to do this with reduce, btw.
(defn walk-stops
  "Generates a sequence of next points from an initial point (a coordinate
  pair) and a (finite) sequence of [direction, length] (mathematical) vectors,
  using each in turn, adding it to the previous point.  (The resulting points
  are the \"stops\" in a random walk.) Example use, where step-vecs has been 
  generated by repeated calls to next-walk-fn: (walk-stops [0 0] step-vecs)"
  [base-pt step-vectors]
  (loop [result [base-pt]
         prev-pt base-pt
         vectors (seq step-vectors)]
    (if vectors
      (let [next-pt (next-walk-stop prev-pt (first vectors))]
        (recur (conj result next-pt)
               next-pt
               (next vectors))) ; next turns empty sequences into nil
      result)))


;; TODO Should the `lazy-seq` be outside?
(defn lazy-walk-stops
  "Generates a (possibly infinite) sequence of next points from an 
  initial-point and a (possibly infinite) sequence of [direction, length]
  vectors, using each in turn, adding it to the previous point.  
  (These points are the \"stops\" in a random walk.)  See walk-stops for
  more.  
  NOTE: Because of chunking, you *cannot* assume that lazy calls to a PRNG
  will happen the same way every time.  See
  is-lazy-sequence-chunking-deterministic at ask.clojure.org."
  [base-pt step-vectors]
  (cons base-pt 
        (when-let [next-step-vec (first step-vectors)] ; nil if no more step-vecs
          (lazy-seq 
            (lazy-walk-stops (next-walk-stop base-pt next-step-vec)
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
          y-eps (abs (* slope x-eps))]
      [x-eps y-eps])
    [0 eps]))


(defn swap-args-fn
  "Given a function that accepts two arguments, wraps it in a function
  that reverses the arguments and passes them to the original function."
  [f]
  (fn [x y] (f y x)))


;; See doc/xyshifts.md for notes about this function and xy-shifts.
;; Possibly store slope and/or intercept earlier; they were available
;; when the line pair was created.
;; 
;; WHY ONLY TEST FOR x = endpoint x2, and not the y coords, too?
;; The code in the function flips coords so that when the test for
;; the end of the inner loop occurs, the slope cannot be vertical,
;; and in fact is very far from vertical.  Therefore if y is changing,
;; x is too.  Note that if the slope is 0, y will always equal the y
;; endpoint (y2).  The problem is that if the slope is *very* close to
;; zero, but not zero, it can happen that adding y-eps to y does not
;; change it.  Then the y coordinate never changes, and as a result
;; it will never become equal to the y endpoint y2.  By testing
;; only for x = x2, we avoid this problem, and since lines are not
;; vertical or even close to vertical, there are no cases in which
;; x stops changing before reaching the end of the segment.
;; Note WHAT THIS MEANS: That we are treating a y coordinate that is 
;; not equal to the y endpoint as if it were equal to it.  Once the
;; x coordinate becomes equal to the x endpoint, we pretend that the
;; y coords are equal even if they aren't.  But that's OK, because
;; if y-eps is so small that this matters, then the difference between
;; the incrementing y coordinate and the y endpoint is so small that
;; it is much smaller then what matters for searching for foodspots.
;; Because the tiny y-eps means that the initial starting y coordinate
;; and the final ending y coordinate are so close that the difference
;; between them will not matter for finding foodspots (or if it does
;; matter, that's a difference that I'm willing to accept).
;; SEE smallSlopeBugNotes052022.txt for further details.
;; 
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
  ;(println [x1 y1] [x2 y2]) (flush) ; DEBUG
  (let [slope (m/slope-from-coords [x1 y1] [x2 y2])
        steep (or (infinite? slope)
                  (> (abs slope) steep-slope-inf))
        slope (if steep (/ slope) slope)
        look-fn (if steep (swap-args-fn look-fn) look-fn)
        [[x1 y1] [x2 y2]] (if steep
                            [[y1 x1] [y2 x2]]    ; swap x and y
                            [[x1 y1] [x2 y2]])   ; make no change
        x-pos-dir? (<= x1 x2)
        y-pos-dir? (<= y1 y2)
        [x-eps y-eps] (xy-shifts eps slope)     ; x-eps, y-eps always >= 0
        x-shift (if x-pos-dir? x-eps (- x-eps)) ; correct their directions
        y-shift (if y-pos-dir? y-eps (- y-eps))
        x-comp (if x-pos-dir? > <)   ; and choose tests for when we've 
        y-comp (if y-pos-dir? > <)]  ;  gone too far
    (loop [x x1, y y1]
      (let [food (look-fn x y)]
        (cond food  [food (if steep [y x] [x y])] ; swap coords back if necess (food is correct)
              ;(and (== x x2) (== y y2))  nil ; OLD VERSION: last point. check both: horizontal or vertical lines
              (== x x2)  nil ; last point: see comment above function def for explanation.
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
            [nil stopsv])))))) ; no food in any segment; return entire input


;; FIXME buggy: plots are not turning out correctly.
(defn trim-full-walk
  "Gvien a foodwalk triple of the kind returned by levy-foodwalk
  or straight-foodwalk, returns a similar triple in which the third
  element, full-walk, has been truncated at the front to include only
  those line segments not included in walk-until-food. Specifically:
  If no food was found, full-walk is replaced by nil since 
  walk-until-food is identical to full-walk.
  If food was found, let n be the number of points in walk-until-food;
  Then the first n-1 points are dropped from full-walk in the return 
  triple.  The last point in walk-until-food is the point from which
  food was found, which is usually not included in full-walk.  Backing
  up by one means that full-walk includes full line segment from within 
  which the food was found, partially.  The original full-walk can be
  reconstructed e.g. by removing the last element from walk-until-food
  and then concatenating the two sequences."
  [[found walk-until-food full-walk]]
  (if-not found
    [found walk-until-food nil] ; the two walks are identical; don't waste time/space by dup'ing
    [found walk-until-food (drop (- (count walk-until-food) 2)
                                 full-walk)]))

(comment
  ;; testing
  (def full [[0 0] [0 1] [2 1] [2 4] [-4 4] [-4 2]])
  (def wuf1  [[0 0] [0 1] [2 1] [2 3]])
  (trim-full-walk [nil wuf1 full])
  (trim-full-walk [["food"] wuf1 full])
  (def wuf2  [[0 0] [0 1] [2 1] [2 4]]) ; finds foodspot from an endpoint
  (trim-full-walk [nil wuf2 full])
  (trim-full-walk [["food"] wuf2 full])
)


(defn levy-foodwalk
  "Generates a random foodwalk starting from point init-loc in direction
  init-dir, and returns a vector triple containing (a) a sequence of found
  foodspots or nil if none found, (b) the generated sequence from start until
  the point from which the foodspots were found or the entire sequence if
  no foodspots were found, and (c) a subsequence containing the remaining
  stops, if any, after the foodspots were found.  If init-dir is falsey, 
  the initial direction will be random.  More specifically, the generated 
  foodwalk consists of a series of line segments and ends where a foodspot
  is first found, or when the sum of segment lengths is equal to maxpathlen.
  Food search uses look-fn to repeatedly check for food at points that are
  look-eps apart, beginning from init-loc. (The environment is to be wrapped
  up in look-fn and carried with it.)"
  ([look-fn look-eps maxpathlen init-dir trunclen rng scale exponent init-pad init-loc]
   (let [len-dist (r/make-powerlaw rng scale exponent)]
     (levy-foodwalk look-fn look-eps maxpathlen init-dir trunclen rng len-dist init-pad init-loc)))
  ([look-fn look-eps maxpathlen init-dir trunclen dir-dist len-dist init-pad init-loc]
   (let [raw-inf-step-walk (repeatedly
                             (step-vector-fn dir-dist len-dist 1 trunclen))
         inf-step-walk (if init-dir
                         (subst-init-dir init-dir raw-inf-step-walk)
                         raw-inf-step-walk)
         step-walk (vecs-upto-len maxpathlen inf-step-walk) ; should be a vec
         first-loc (if init-pad  ; if truthy, shift start in a random dir this much from init-loc
                     (next-walk-stop init-loc [(r/next-radian dir-dist) init-pad])
                     init-loc)
         stop-walk (walk-stops first-loc step-walk) ; walk-stops is no longer lazy btw
         walk-with-food (path-with-food look-fn look-eps stop-walk)] ; a vec
     (trim-full-walk (conj walk-with-food stop-walk)))))


;; TODO: DOES THIS WORK: IS THE STATE THE SAME AT THE END?
(defn levy-foodwalk-flush-state
  "Uses up PRNG state like levy-foodwalk would, but without going through 
  the steps needed to find food.  The only required arguments are dir-dist,
  len-dist, and trunclen; or rng, trunclen, scale, and exponent. Returns nil.
  See levy-foodwalk for the meaning of parameters."
  ([look-fn look-eps maxpathlen init-dir trunclen rng scale exponent init-pad init-loc]
   (let [len-dist (r/make-powerlaw rng scale exponent)]
     (levy-foodwalk-flush-state look-fn look-eps maxpathlen init-dir trunclen rng len-dist init-pad init-loc)))
  ([look-fn look-eps maxpathlen init-dir trunclen dir-dist len-dist init-pad init-loc]
   (levy-foodwalk-flush-state dir-dist len-dist trunclen))
  ([rng trunclen scale exponent]
   (let [len-dist (r/make-powerlaw rng scale exponent)]
     (levy-foodwalk-flush-state rng len-dist trunclen)))
  ([dir-dist len-dist trunclen]
   (let [_ (repeatedly (step-vector-fn dir-dist len-dist 1 trunclen))]
     nil)))


(defn straight-foodwalk
  "Generates a straight foodwalk starting from point init-loc in direction
  init-dir, and returns a vector triple containing (a) a sequence of found
  foodspots or nil if none found, (b) the generated sequence from start until
  the point from which the foodspots were found, and (c) the entire generated
  sequence including the stops after the foodspots were found.  More
  specifically, the foodwalk consists of a single line segment, which ends 
  where a foodspot is found or when maxpathlen is reached.  Food search uses
  look-fn to repeatedly check for food at points that are look-eps apart,
  beginning from init-loc."
  ([look-fn look-eps maxpathlen dir-dist init-pad init-loc init-dir]
   (let [first-dir (if init-dir init-dir (r/next-radian dir-dist))
         first-loc (if init-pad 
                     (next-walk-stop init-loc [(r/next-radian dir-dist) init-pad])
                     init-loc)]
     (straight-foodwalk look-fn look-eps maxpathlen first-loc first-dir)))
  ([look-fn look-eps maxpathlen init-loc init-dir]
   (let [step-walk [[init-dir maxpathlen]] ; a single step of the whole length
         stop-walk (walk-stops init-loc step-walk) ; contains exacty 2 points
         walk-with-food (path-with-food look-fn look-eps stop-walk)]
     (trim-full-walk (conj walk-with-food stop-walk)))))

(defn shift-beyond-radius
  "Given a pair of coordinates from a foodspot, center, starting point, etc.,
  moves pad-dist away from in it a random direction using dir-dist as the 
  random direction generator."
  [dir-dist pad-dist coords]
  (let [dir (r/next-radian dir-dist)]
    (next-walk-stop coords [dir pad-dist])))

(defn path-until-found-length
  "Given a pair consisting of a possibly empty sequence of found foodspots and a
  path of walk stops until they were found (if they were), returns the length
  of the path.  If the argument is a sequence with more than two elements, its
  remaining elements are silently ignored."
  [[found-foodspots path-until-found]]
  (stops-path-len path-until-found))

;; Note nils are converted to empty cells by write-csv.
(defn path-if-found-length
  "Given a pair consisting of a possibly empty sequence of found foodspots and a
  path of walk stops until they were found (if they were), returns the length
  of the path, or nil if no foodspots were found.  If the argument is a sequence 
  with more than two elements, its remaining elements are silently ignored."
  [[found-foodspots path-until-found]]
  (if (seq found-foodspots)
    (stops-path-len path-until-found)
    nil))

;; These next two functions might return different results if foodspots
;; are randomly distributed.

(defn count-successful
  "Returns the number of foodwalks that found any food."
  [foodwalks]
  (reduce (fn [tot walk]
            (+ tot (if (first walk) 1 0)))
          0 foodwalks))

(defn count-found-foodspots
  "Returns the number of foodspots found by the foodwalks.  If it's
  possible for a foodwalk to find multiple foodspots, they'll be counted."
  [foodwalks]
  (reduce (fn [tot walk]
            (+ tot (count (first walk))))
          0 foodwalks))

(defn count-segments-until-found
  "Count segments in a foodwalk until food is found."
  [fw]
  (dec (count (nth fw 1)))) ; dec since endpoints = segments + 1

(defn count-segments-until-found-in-foodwalks
  "Sums results of count-segments-until-found in multiple foodwalks fws."
  [fws]
  (reduce (fn [tot fw] (+ tot (count-segments-until-found fw)))
          0 fws))

(defn count-all-segments
  "Count all segments in a foodwalk, including the couldve segments after 
  found foodspots."
  [fw]
  (+ (count (nth fw 1))
     (count (nth fw 2))
     -3)) ; -3 since there is one more point than segments in each, and they overlap

(defn count-all-segments-in-foodwalks
  "Sums results of count-all-segments in multiple foodwalks fws."
  [fws]
  (reduce (fn [tot fw] (+ tot (count-all-segments fw)))
          0 fws))

(defn sort-foodwalks
  "Sorts a sequence of foodwalks so that walks in which food 
  is found are first."
  [fws]
  (sort-by #(if (first %) 0 1)
           fws))
