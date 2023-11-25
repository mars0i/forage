;; GENERATING WALKS
(ns forage.core.walks
    (:require [utils.math :as m]
              [utils.spiral :as spiral]
              [utils.random :as r]
              [clojure.core :as cc] ; for cc/<, cc/> (in find-in-seg), and cc/+ (with reduce).
              [fastmath.core :as fm]))

;; (Code s/b independent of MASON and plot libs (e.g. Hanami, Vega-Lite).)

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
(fm/use-primitive-operators)

;; NOTE Advantages of starting with mathematical vectors (direction,
;; length) pairs over coordinates (x, y location pairs) are:
;;   - It's easier to calculate overall length, since the second element
;;     of every pair is already a length.
;;   - It's easier to paste walks together to make composite walks.
;;     If you start with sequences of coordinates, you have to shift
;;     them all with the value of the last point in the previous walk
;;     in the sequence.  If you start from math-vectors, you can just
;;     concatenate the sequences of vectors for the different subwalks,
;;     and then create the coordinate pair sequences as you normally would.

(defn step-vector-fn
  "Returns a function of no arguments that returns a random mathematical 
  vector in the form of pair containing a direction dir, in radians, and a
  length len.  dir is uniformly distributed in [0,2pi) using PRNG instance 
  or distribution instance dir-dist, and length is distributed according to
  distribution instance len-dist.  If low and high arguments are given, the
  distribution is truncated so that lengths fall within [low, high].  (These
  vectors represent steps going from one \"stop\" to the next in a random walk.)
  Example use:
    (def rng (r/make-well19937))
    (def step-vecs (repeatedly 
                     (step-vector-fn rng (r/make-powerlaw rng 1 2) 1 100)))"
  ([dir-dist len-dist] (fn [] [(r/next-radian dir-dist)
                               (r/next-double len-dist)]))
  ([dir-dist len-dist low high]
   (fn [] [(r/next-radian dir-dist) (r/next-double len-dist low high)])))


(defn make-levy-vecs
  "Returns an infinite sequence of mathematical vectors generated by
  step-vector-rn representing a Lévy walk with direction distribution
  dir-dist, and length distribution len-dist, minimum value low and
  maximum value high."
  [dir-dist len-dist low high]
  (repeatedly (step-vector-fn dir-dist len-dist low high)))

(defn make-n-levy-vecs
  [dir-dist len-dist low high n]
  (when (neg? n) (throw (Exception. (str "make-n-levy-vecs: length of sequence can't be negative: n =" n))))
  (loop [acc [], i n]
    (if (zero? i)
      acc
      (recur (conj acc [(r/next-radian dir-dist) (r/next-double len-dist)])
             (dec i)))))

(comment
  (require '[criterium.core :as crit])
  (def seed (r/make-seed))
  (def rng (r/make-mrg32k3a seed))
  (def dist (r/make-mrg32k3a-powerlaw rng 1 2))
  (time (crit/quick-bench (doall (take 10000 (make-levy-vecs rng dist 1 1000))))) ; 1.68, 1.72 millisecs (1/1000000000) on MBA
  (time (crit/quick-bench (make-n-levy-vecs rng dist 1 1000 10000)))      ; 58, 63 millisecs (1/1000) on MBA
  ;; WHY IS THE LAZY VERSION SO MUCH FASTER? SOMETHING's WRONG
  (def lazy (doall (take 10000 (make-levy-vecs rng dist 1 1000)))) ; 1.68, 1.72 millisecs (1/1000000000) on MBA
  (def eager (make-n-levy-vecs rng dist 1 1000 10000))      ; 58, 63 millisecs (1/1000) on MBA
  ;; OK THEY'RE NOT DOING THE SAME THING:
  (= lazy eager)
  (count lazy)
  (count eager)
  (last lazy)
  (last eager)

  
)

;; NOTE SEE test/forage/walks.clj for experiments and test of 
;; incremental-composite-vecs that were formerly below and above this
;; point.

;; NOTE: Another good way to generate a sequence of vectors is to
;; simply concatenate sequences of vectors generated some other way, as above.
;; This version runs a test on every step.  The might be useful to e.g.
;; test for something about a found target/foodspot.  But my general
;; strategy is to generate walks in a higher-level sense.
;; TODO remove labels arg?  See docstring.
(defn incremental-composite-vecs
  "Returns an infinite sequence of mathematical vectors.  vec-fns is a
  sequence of functions like those generated by step-vector-fn. switch-fns
  is a sequence of functions, each of which can determine, on each step,
  whether to shift cyclically from one step vector function to the next to
  generate the next vector.  Each switch function should return an element
  that's either: (A) a truthy element which both means \"continue with this
  step vector\" function, and contains data to be passed to the same switch
  function on the next iteration in order to help it decide then, or (B)
  nil or false, indicating that the next step vector function should be
  used to generate the next step.  Each switch function should accept two
  arguments: (1) the new step vec, and (2) data from the last application
  of the same switch function (or nil if the previous application used a
  different switch function).  [Example data as second arg of switch
  functions: (i) number of steps in subsequence, (ii) length of
  subsequence, or (iii) the entire subsequence.] labels argument is
  experimental.  Could be used to distinguish subwalks, but only if
  endpoints are duplicated.  SEE ALSO: composite-walk-stops."
  ([switch-fns vec-fns]
   (incremental-composite-vecs switch-fns vec-fns nil))
  ([switch-fns vec-fns labels]
  (letfn [(make-vecs [sw-fns v-fns labls sw-data] ; lazy-seq needs to recurse on fn not loop/recur
            (lazy-seq
              (let [fresh-vec ((first v-fns))
                    ;; TODO Can I get rid of the conditional and always append the label?:
                    new-vec (if labls
                              (conj (vec fresh-vec) (first labls)) ; add label to end of record
                              fresh-vec)]                           ; if available
                (cons new-vec
                      (if-let [new-sw-data ((first sw-fns) new-vec sw-data)] ; truthy means "keep using this vec fn"
                        (make-vecs sw-fns v-fns labls new-sw-data)
                        (make-vecs (next sw-fns) (next v-fns) (next labls) nil))))))]
    (make-vecs (cycle switch-fns) (cycle vec-fns) (cycle labels) nil))))

(defn switch-after-n-steps-fn
  "Generates a dist?fn for use with incremental-composite-vecs.  The returned function
  switches the vec-fn after n steps."
  [^long n]
  (fn [_ ^long step-count]
    (let [new-step-count (if step-count
                           (inc step-count)
                           (long 1))] ; if falsey, initialize count
      (if (>= new-step-count n)
        false
        new-step-count))))

(defn subst-init-dir
  "Given a sequence step-seq of step vectors, i.e. [direction length] pairs,
  return a sequence that's the same except that the direction of the first 
  vector has been replaced by init-dir."
  [init-dir step-seq]
  (let [firststep  (vec (first step-seq))
        othersteps (rest step-seq)
        newstep    (assoc firststep 0 init-dir)]
    (cons newstep othersteps)))

(defn vecs-upto-len
  "Given a desired total path length, and a sequence of step vectors,
  returns a sequence of step vectors (beginning from the front of the
  sequence) whose lengths sum to desired-total (or less if vecs is too
  short).  The lengths are made to sum to exactly desired-total by reducing
  the length in the last step vector.  This function is eager rather than
  lazy."
  [^double desired-total vecs]
  (loop [tot-len 0.0, out-vecs [], in-vecs vecs] ; init with 0.0 to avoid warn-on-reflection error
    (if (empty? in-vecs)
      out-vecs
      (if (< tot-len desired-total)
        (let [v (first in-vecs)
              ^double len (second v)]
          (recur (+ tot-len len)
                 (conj out-vecs v)
                 (rest in-vecs)))
        ;; The length is long enough; now trim any extra length:
        (let [overshoot (- tot-len desired-total)
              [old-dir ^double old-len] (last out-vecs)
              newlast [old-dir (- old-len overshoot)]]
          (conj (vec (butlast out-vecs)) newlast))))))


;; Instead of the following, one could use 
;; (count (vecs-upto-len desired-total vecs))
;; This version is more efficient if you don't yet want to separate out 
;; the relevant vecs, but then you might have to trim the last element
;; later.  I might delete count-vecs-upto-len later.
(defn count-vecs-upto-len
  "Given a desired total path length, and a sequence of step vectors,
  returns the number of steps needed to sum lengths to at least 
  desired-total."
  [^double desired-total vecs]
  (reduce (fn [[^double tot-len ^long cnt] [_ ^double len]]
            (if (< tot-len desired-total)
              [(+ tot-len len) (inc cnt)]
              (reduced cnt)))
          [0 0]
          vecs))

(defn next-walk-stop
  "Given an initial point (or a mathematical vector) in the form of a
  coordinate pair, and a mathematical vector in the form of a direction
  in radians and a length, returns a new coordinate pair that's the result
  of adding the vector to the point.  (This is the next \"stop\" in a walk.)
  If provided, an optional label is the third element of the returneed 
  Clojure vector."
  [[^double prevx ^double prevy] [dir len label]]
  (let [[^double vecx ^double vecy] (m/rotate dir [len, 0]) ; rotate vector lying on x-axis
        nextx (+ prevx vecx)  ; add vector to prev point
        nexty (+ prevy vecy)
        newpt [nextx nexty]]
    (if label (conj newpt label) newpt)))

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


(defn vecs-path-len
  "Calculate the length of a path specified by a sequence of vector representations
  in the form of [direction, length] pairs."
  [step-vectors]
  (reduce cc/+ (map second step-vectors)))

(defn stops-path-len
  "Calculate the length of a path specified by a sequence of stops, i.e. [x y] 
  coordinate pairs representing endpoints of connected line segments."
  [stops]
  (reduce cc/+
          (map m/distance-2D stops (rest stops))))


(defn composite-walk-stops
  "Pastes together the sequences of walk stops, i.e. sequences of location
  coordinate pairs, realizing all of the sequences if they have not been
  realized.  The sequences must be finite."
  [walk-stops-seqs]
  (doall (apply concat (map doall walk-stops-seqs))))

;; NOTE SEE test/forage/walks.clj for experiments and test of 
;; incremental-composite-vecs that were formerly at this point.

(defn shift-beyond-radius
  "Given a pair of coordinates from a foodspot, center, starting point, etc.,
  moves pad-dist away from in it a random direction using dir-dist as the 
  random direction generator."
  [dir-dist pad-dist coords]
  (let [dir (r/next-radian dir-dist)]
    (next-walk-stop coords [dir pad-dist])))


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

(fm/unuse-primitive-operators) ; needed when I upgrade to Clojure 1.12
