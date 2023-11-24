;; FINDING FOOD IN WALKS
(ns forage.core.findfood
    (:require [utils.math :as m]
              [utils.random :as r]
              [forage.core.walks :as w]
              [fastmath.core :as fm]
              [ham-fisted.api :as hf]
              [ham-fisted.hlet :as hfl])
    (:import [clojure.lang IFn$DDO]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(fm/use-primitive-operators)

;; Setting this to 1 gives us symmetry between the swapped and
;; unswapped coordinates.  Seems as if it would improve performance
;; slightly to have a higher value, since then the x,y swap operations
;; would happen less often.  However, benchmarking shows otherwise.
;; See steep-slope-inf-benchmarks.txt.  (That note was from an early stage.
;; I haven't tried changing this value since I started optimizing in 
;; October 2023.)
(def ^:const +steep-slope-inf+ 
  "If a slope is greater than this value, the x and y coordinates will
  be swapped temporarily and then unswapped later.  This is a way to
  deal with both truly vertical slopes (slope = ##Inf) and slopes that are
  so close to vertical that moving through a line segment with this slope
  will be problematic.  It also sidesteps the problem of identifying slopes
  that are actually vertical, but don't appear so because of float slop."
  1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING FOOD

;; See doc/xyshifts.md for detailed notes about this function and find-in-seg.
(defn xy-shifts
  "Given an incremental shift (vector) in the direction of a line specified 
  by its slope and intercept, return a pair [x-eps y-eps] that give
  the shifts in the x and y directions that would produce the desired shift
  (i.e. the vectors along x and y that would sum to the desired shift)."
  [^double eps ^double slope]
  (if slope ; if not vertical
    (let [a (+ 1 (* slope slope))
          x-eps (/ eps a)
          y-eps (abs (* slope x-eps))]
      [x-eps y-eps])
    [0 eps]))

(defn- ->ddo-fn
  ^IFn$DDO [f]
  (when-not (instance? IFn$DDO f)
    (throw (RuntimeException. (str "Function is not double->double->object " f))))
  f)

(defn swap-args-fn
  "Given a function that accepts two arguments, wraps it in a function
  that reverses the arguments and passes them to the original function."
  [^IFn$DDO f]
  ;(let [f (->ddo-fn f)]
    (fn [^double x ^double y] (.invokePrim f y x))) ;)

(defn- lt [^double l ^double r] (< l r))
(defn- gt [^double l ^double r] (> l r))

;; NOTES ON FIND-IN-SEG
;; See doc/xyshifts.md for notes about this function and xy-shifts.
;; Possibly store slope and/or intercept earlier; they were available
;; when the line pair was created.
;;
;; WHY ONLY TEST FOR x = endpoint x2, and not the y coords, too?
;; (See the cond within the loop/recur expression.)
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
;; if y-eps is so small that if this matters, then the difference between
;; the incrementing y coordinate and the y endpoint is so small that
;; it is much smaller then what matters for searching for foodspots.
;; Because the tiny y-eps means that the initial starting y coordinate
;; and the final ending y coordinate are so close that the difference
;; between them will not matter for finding foodspots.  (Or if it does
;; matter, that's a difference that I'm willing to accept.)
;; SEE smallSlopeBugNotes052022.txt for further details.
;; 
;; TODO There are many redundant if tests, etc. in this code.  That doesn't
;; matter as long as look-fn is expensive, which is the case when using MASON
;; for look-fns. If I develop more efficient envs and look-fns, it might be
;; worth optimizing some of the code further.


;; VERSION BASED ON branch cnuerber-main
;; FOR TEST DATA, BENCHMARKING, SEE END OF FILE.
(defn find-in-seg
  "Given a pair of endpoints [x1 y1] and [x2 y2] on a line segment,
  and a small shift length, starts at [x1 y1] and incrementally checks
  points along the line segment at every shift length locations, checking 
  to see whether look-fn returns a truthy value representing one or more 
  foodspots from the perspective of that location, or a falsey value if
  no foodspots are found.  look-fn should take a single argument, a
  pair representing the coordinates of a location from which to check
  whether a foodspot is perceptible.  If foodspots are found, this function 
  stops searching and returns a representation of the foodspots found, which
  may be a collection of foodspot objects, a collection of coordinates of
  foodspot objects, or some other truthy value.  (The kind of value to be
  returned depends on look-fn, which should reflect the way that this 
  function will be used.)  If no foodspots are found by the time [x2 y2]
  is checked, this function returns nil."
  [^IFn$DDO look-fn eps x1 y1 x2 y2]
  ;; TODO replace some of the agets with new ham-fisted structred let
  (hfl/let [slope (m/slope-from-coords* x1 y1 x2 y2)
            steep (or (infinite? slope)
                      (> (abs slope) +steep-slope-inf+))
            slope (if steep (/ slope) slope)
            ^IFn$DDO look-fn (if steep (swap-args-fn look-fn) look-fn)
            [x1 y1 x2 y2] (dbls (if steep
                                  (hf/double-array [y1 x1 y2 x2]) ; swap x and y
                                  (hf/double-array [x1 y1 x2 y2]))) ; make no change
            ;^doubles data (if steep
            ;                (hf/double-array [y1 x1 y2 x2]) ; swap x and y
            ;                (hf/double-array [x1 y1 x2 y2])) ; make no change
            ;x1 (aget data 0)
            ;y1 (aget data 1)
            ;x2 (aget data 2)
            ;y2 (aget data 3)
            x-pos-dir? (<= x1 x2)
            y-pos-dir? (<= y1 y2)
            [x-eps y-eps] (dbls (xy-shifts eps slope)) ; x-eps, y-eps always >= 0
            ;xy-eps (xy-shifts eps slope) ; x-eps, y-eps always >= 0
            ;x-eps (double (xy-eps 0))
            ;y-eps (double (xy-eps 1))
            x-shift (if x-pos-dir? x-eps (- x-eps))             ; correct their directions
            y-shift (if y-pos-dir? y-eps (- y-eps))
            ^IFn$DDO x-comp (if x-pos-dir? gt lt)  ; and choose tests for when we've
            ^IFn$DDO y-comp (if y-pos-dir? gt lt)] ; gone too far
    (loop [x x1, y y1]
      (let [food (.invokePrim look-fn x y)]
        (cond food  [food (if steep [y x] [x y])] ; swap coords back if necess (food is correct)
              (== x x2)  nil ; last point: see comment above function def for explanation.
              :else  (let [xsh (+ x x-shift)
                           ysh (+ y y-shift)]
                       (recur (if (.invokePrim x-comp xsh x2) x2 xsh) ; search from x2 if xsh went too far
                              (if (.invokePrim y-comp ysh y2) y2 ysh))))))))
;; FOR TEST DATA, BENCHMARKING, SEE END OF FILE.


(defn path-with-food
  "Returns a vector containing first, found foodspots or nil, and second
  the sequence up to and including the location from which foodspots were
  found, or the entire sequence.  More specifically, given a sequence of
  stops (coordinate pairs) representing a random walk, and a small eps
  length, starts at [x1 y1] and uses seg-exam-fn [default: find-in-seg] to
  incrementally check each line segment defined by pairs of stops to see
  whether look-fn returns a truthy value, meaning that foodspots were
  found.  The sequence stops must contain at least two coordinate pairs.
  If foodspots are found, returns a pair vector containing: first, the
  foodspot information returned by look-fn, and second, a truncated
  sequence of stops in which the last element is the point from which the
  food was seen, and remaining points have been removed. If no food found
  in the entire sequence, a pair vector containing nil and the unchanged
  sequence is returned.  For backward compatibility, if seg-exam-fn is not
  passed, walks/find-in-seg is used as its default value."
  ([look-fn eps stops]
   (path-with-food find-in-seg look-fn eps stops))
  ([seg-exam-fn look-fn eps stops]
   (let [stopsv (vec stops)
         numstops- (dec (count stops))] ; stop inc'ing two consecutive idxs one before length of stops vector
     (loop [i 0, j 1]
       (let [endpt-i (stopsv i)
             endpt-j (stopsv j)
             from+foodspots (seg-exam-fn look-fn eps
                                         (endpt-i 0) (endpt-i 1)
                                         (endpt-j 0) (endpt-j 1))]
         (if from+foodspots               ; all done--found food
           [(first from+foodspots)        ; the found food
            (conj (vec (take j stopsv))      ; replace end of stops with point
                  (second from+foodspots))]  ; on path from which food found
           (if (< j numstops-)
             (recur (inc i) (inc j))
             [nil stopsv]))))))) ; no food in any segment; return entire input


(defn trim-full-walk
  "Gvien a foodwalk triple of the kind returned by foodwalk, levy-foodwalk,
  or straight-foodwalk, returns a similar triple in which the third
  element, full-walk, has been truncated at the front to include only those
  line segments not included in walk-until-food. That is, if no food was
  found, the current function will return a triple [nil, the full walk,
  nil]. If food was found, then walk-until-food contains the walk only up
  to the point that food was found.  The current function will then return
  a triple [collection containing foodspots, the walk up until the food was
  found, the rest of the walk]. Specifically: If no food was found,
  full-walk is replaced by nil since walk-until-food is identical to
  full-walk. If food was found, let n be the number of points in
  walk-until-food; Then the first n-1 points are dropped from full-walk in
  the return triple.  The last point in walk-until-food is the point from
  which food was found, which is usually not included in full-walk. Backing
  up by one means that full-walk includes full line segment from within
  which the food was found, partially.  The original full-walk can be
  reconstructed e.g. by removing the last element (the point from which the
  food was found--not the endpoint of the segment) from walk-until-food and
  then concatenating the two sequences."
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

;; Abstracted this out of levy-foodwalk so one can provide a sequence of
;; location coordinate pairs rather than a sequence of vectors.  It's
;; easier to construct some walks directly as location coordinate pairs.
;; (Example: a composite walk that contains Archimedean spirals.)
(defn foodwalk
  "Given stop-walk, a representation of a possible walk as a sequence of
  location coordinate pairs, returns a triple produced by trim-full-walk.
  That is, returns a vector triple containing (a) a sequence of found
  foodspots or nil if none found, (b) the generated sequence from start
  until the point from which the foodspots were found or the entire
  sequence if no foodspots were found, and (c) a subsequence containing the
  remaining stops, if any, after the foodspots were found.  seg-exam-fn
  [default: forage.core.walks/find-in-seg] should apply look-fn to each
  segment defined by stop-walk, in order to determine whether it passes
  within perceptual radius of a foodspot."
  ([look-fn look-eps stop-walk]
   (foodwalk find-in-seg look-fn look-eps stop-walk))
  ([seg-exam-fn look-fn look-eps stop-walk]
   (trim-full-walk (conj (path-with-food seg-exam-fn look-fn look-eps stop-walk)
                         stop-walk))))

;; Note this generates a foodwalk as well as searching in it, so it
;; uses functions from walks.clj.
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
   (let [raw-inf-step-walk (w/make-levy-vecs dir-dist len-dist 1 trunclen)
         inf-step-walk (if init-dir
                         (w/subst-init-dir init-dir raw-inf-step-walk)
                         raw-inf-step-walk)
         step-walk (w/vecs-upto-len maxpathlen inf-step-walk) ; should be a vec
         first-loc (if init-pad  ; if truthy, shift start in a random dir this much from init-loc
                     (w/next-walk-stop init-loc [(r/next-radian dir-dist) init-pad])
                     init-loc)
         stops (w/walk-stops first-loc step-walk)] ; walk-stops is no longer lazy btw
     (foodwalk look-fn look-eps stops))))

;; Note this generates a foodwalk as well as searching in it, so it
;; uses functions from walks.clj.
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
                     (w/next-walk-stop init-loc [(r/next-radian dir-dist) init-pad])
                     init-loc)]
     (straight-foodwalk look-fn look-eps maxpathlen first-loc first-dir)))
  ([look-fn look-eps maxpathlen init-loc init-dir]
   (let [step-walk [[init-dir maxpathlen]] ; a single step of the whole length
         stop-walk (w/walk-stops init-loc step-walk) ; contains exacty 2 points
         walk-with-food (path-with-food look-fn look-eps stop-walk)]
     (trim-full-walk (conj walk-with-food stop-walk)))))

(defn path-until-found-length
  "Given a pair consisting of a possibly empty sequence of found foodspots and a
  path of walk stops until they were found (if they were), returns the length
  of the path.  If the argument is a sequence with more than two elements, its
  remaining elements are silently ignored."
  [[found-foodspots path-until-found]]
  (w/stops-path-len path-until-found))

;; Note nils are converted to empty cells by write-csv.
(defn path-if-found-length
  "Given a pair consisting of a possibly empty sequence of found foodspots and a
  path of walk stops until they were found (if they were), returns the length
  of the path, or nil if no foodspots were found.  If the argument is a sequence 
  with more than two elements, its remaining elements are silently ignored."
  [[found-foodspots path-until-found]]
  (if (seq found-foodspots)
    (w/stops-path-len path-until-found)
    nil))

;; These next two functions might return different results if foodspots
;; are randomly distributed.

(defn count-successful
  "Returns the number of foodwalks that found any food."
  [foodwalks]
  (reduce (fn [^long tot walk]
            (+ tot (if (first walk) 1 0)))
          0 foodwalks))

(defn count-found-foodspots
  "Returns the number of foodspots found by the foodwalks.  If it's
  possible for a foodwalk to find multiple foodspots, they'll be counted."
  [foodwalks]
  (reduce (fn [^long tot walk]
            (+ tot (count (first walk))))
          0 foodwalks))

(defn count-segments-until-found
  "Count segments in a foodwalk until food is found."
  ^long [fw]
  (dec (count (nth fw 1)))) ; dec since endpoints = segments + 1

(defn count-segments-until-found-in-foodwalks
  "Sums results of count-segments-until-found in multiple foodwalks fws."
  [fws]
  (reduce (fn [^long tot fw] (+ tot (count-segments-until-found fw)))
          0 fws))

(defn count-all-segments
  "Count all segments in a foodwalk, including the couldve segments after 
  found foodspots."
  ^long [fw]
  (+ (count (nth fw 1))
     (count (nth fw 2))
     -3)) ; -3 since there is one more point than segments in each, and they overlap

(defn count-all-segments-in-foodwalks
  "Sums results of count-all-segments in multiple foodwalks fws."
  [fws]
  (reduce (fn [^long tot fw] (+ tot (count-all-segments fw)))
          0 fws))

(defn sort-foodwalks
  "Sorts a sequence of foodwalks so that walks in which food 
  is found are first."
  [fws]
  (sort-by #(if (first %) 0 1)
           fws))


(comment
  ;; SIMPLE TEST DATA FOR find-in-seg.
  ;; Use with look-fns from the env-null namespace.

  (require '[criterium.core :as crit])

  ;; find-in-seg walks through a line segment from (x1, y1) to (x2, y2) in
  ;; steps of size eps.  At each step, it passes the current location to
  ;; look-fn to see whether there is a "foodspot" sufficiently near.  
  ;; If so, look-fn returns truthy (typically a foodspot or its coordinates);
  ;; if not, look-fn returns falsey.  So you can test find-in-seg with a
  ;; single line segment, i.e. a single pair of points (x1, y1), (x2, y2).
  ;; Or you can call it repeatedly on subsequent pairs of points [normally,
  ;; with overlapping end points].

  ;; Test with the artificial data:
  (require '[forage.core.env-null :as env])

  (def mywalk0 [[0 0] [1 1]])
  
  (crit/quick-bench
    (find-in-seg env/constant-failure-look-fn
                 0.2
                 ;; FIXME:
                 (first mywalk0)
                 (second mywalk0)))

  (crit/quick-bench
    (find-in-seg (env/create-repeated-success-look-fn 5)
                 0.2
                 ;; FIXME:
                 (first mywalk0)
                 (second mywalk0)))


  ;; Test using more realistic (i.e. more like what the simulation uses) data:
  (def seed 1234567890123456)
  (def rng (r/make-well19937 seed))
  (def levy-vecs (make-levy-vecs rng (r/make-powerlaw rng 1 2) 1 100)) ; an infinite seq
  (def maxlen 200)
  (def walk (walk-stops [0 0] (vecs-upto-len maxlen levy-vecs))) ; seq of points summing to maxlen
  (count walk) ; should = 56
  (def walk-rest (rest walk))
  ;; walk is a sequence of coordinate pairs.
  ;; 
  ;; To use it as test data, take at least two pairs and from walk, and
  ;; then pass the first [i.e. x] and second [i.e. y] elements of the pairs
  ;; to find-in-seg.  You will also need to pass a value for eps [0.2 is
  ;; what I've been using in production code], and a value for look-fn.
  ;; There are some dummy look-fn's in forage.core.env-null.

  ;; Now you can perform the test above on the following:
  (def mywalk (take 2 walk)) ; => ([0 0] [0.9878431867800612 1.5734173777113207])

  (crit/quick-bench
    (find-in-seg env/constant-failure-look-fn
                 0.2
                 ;; FIXME:
                 (first mywalk)
                 (second mywalk)))


  ;; Example using all of walk:

                 ;; FIXME:
  (crit/quick-bench
    (mapv (partial find-in-seg env/constant-failure-look-fn 0.2) 
                 walk
                 walk-rest))
  ;; [If you use a version of find-in-seg that expects coordinates as pairs,
  ;; then the pairs from walk and walk-rest can be passed without pulling
  ;; out the coordinates.]

  ;; Example using a look-fn that succeeds every n steps, i.e. at n*eps
  ;; from start or from the last success.  When it succeeds, it stops
  ;; searching, so this will run faster than when using
  ;; constant-failure-look-fn.
                 ;; FIXME:
  (crit/quick-bench
    (mapv (partial find-in-seg
                   (env/create-repeated-success-look-fn 50)
                   0.2) 
          walk
          walk-rest))

)


(fm/unuse-primitive-operators) ; needed when I upgrade to Clojure 1.12
