;; Functions for minimal, fast environments containing a 
;; few foodspots, without toroidal lookup.
;; The search process runs globally for each segment, and doesn't
;; step through the segment as in env-mason.
(ns forage.core.env-minimal
  (:require [ham-fisted.api :as hf]
            [ham-fisted.hlet :as hfl]
            [fastmath.core :as fm]
            [utils.math :as um]
            [forage.core.food :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(fm/use-primitive-operators)

;; NOTE: This environment type should only be used when targets/foodspots
;; are far apart, and only when there are few targets.  Even then, there 
;; could be something funny that occurs with a long segment.  The reason 
;; is that for each segment, targets are examined in no particular order.
;; If two targets are within percecptual range of a segment, the first one
;; *should* be found, but at present, a different one might be the one
;; returned.  Also, at present, only the first target found is returned;
;; within a segment, we stop looking after that.  So it's theoretically 
;; possible for a walk to skip over a target that it could have found, 
;; and proceed to find another target that is very far away.  This is 
;; unlikely if foodspots are few and far, because a long segment would need
;; to be at just the right orientation, but it's possible.  (We might think
;; of this as a rare perceptual lapse on the part of the forager.)

;; Another reason to use this environment type only with few targets is
;; that for *every* segment, search is linear in the number of *all* 
;; targets in the environment.  For many targets, env-mason is likely
;; to be faster.

;; TODO: Is there a reason to use a Java array rather than a Clojure vector
;; for the foodspots?  Maybe I should make that change.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT THAT CONSISTS OF A COLLECTION of COORDINATE PAIRS.

#_
(defn make-multiple-foodspot-env
  "Make an environment that consists of a vector of Java array pairs of
  doubles representing foodspots. coords should be a sequence of x,y pairs
  of numbers."
  [coords]
  (mapv double-array coords))

;; ENV AS A SINGLE SEQUENCE OF COORDINATES, ALTERNATING X AND Y:
(defn make-multiple-foodspot-env
  "Make an environment that consists of a single Java array with
  alternating x an y coordinstes representing foodspots. coords should be a
  sequence of x,y pairs of numbers."
  ^doubles [coords]
  (double-array (apply concat coords)))

(comment
  (map class (make-multiple-foodspot-env (list '(1 2) [13.0 45.7] (range 2))))
)

;; less slow
(defn perc-multiple-foodspots
  "Returns a vector containing the the first foodspot within perc-radius
  of forager-coords (an x, y Clojure pair), or nil no foodspot is found.
  foodspots are tested in the order they are listed in env."
  [env ^double perc-radius ^double x ^double y] 
  (loop [foodspots env]
    (if (empty? foodspots)
      nil ; none found within perc-radius
      (let [fs (first foodspots)]
        (if (<= (um/distance-2D* x y (fs 0) (fs 1)) perc-radius)
          [fs]
          (recur (rest foodspots)))))))

(comment
  ;; very slow:
  (defn perc-multiple-foodspots
    "Returns a vector containing the the first foodspot within perc-radius
    of forager-coords (an x, y Clojure pair), or nil no foodspot is found.
    foodspots are tested in the order they are listed in env."
    [env ^double perc-radius ^double x ^double y] 
    (loop [foodspots (seq env)]
      (if-let [remaining-foodspots foodspots]
        (let [fs (first remaining-foodspots)]
          (if (<= (um/distance-2D* x y (fs 0) (fs 1))
                  perc-radius)
            [fs]
            (recur (next remaining-foodspots))))
        nil))) ; none found within perc-radius [not when-let since the nil is meaningful]
)

(def foodspot-coords identity)

(defn env-foodspots
  "Returns a sequence of all foodspots in environment env, or nil
  if there are none."
  [env]
  env)

(defn env-foodspot-coords
  "Returns a collection of coordinate pairs of all foodspots in environment
  env, or nil if there are none."
  [env]
  (map vec env))

(comment
  (map class
       (env-foodspot-coords (make-multiple-foodspot-env (list '(1 2) [13.0 45.7] (range 2))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEAVY FIND-IN-SEG, LIGHTWEIGHT MAKE-LOOK-FN

;; env-mason look-fns take a pair of coordinates representing the current
;; location, so that Continuous2D can look for any targets in the nearest bucket.
;; In env-minimal returns the same collection of targets no matter where you are,
;; so there's no need to pass in coordinates.  So make-look-fn returns a
;; function of no arguments that always returns the same targets.
;;
;; I could randomize the order of foodspots with a different look-fn.
#_
(defn make-look-fn
  [env ^double perc-radius]
  (constantly
    (hf/double-array
      (into [perc-radius (+ 1.0 (* 2.0 (count env)))] ; second element is index of last coordinate
            (apply concat env)))))


;; Note that look-fn plays a different role here than in walks/find-in-seg, as it must.
#_
(defn find-in-seg
  "Only returns the first foodspot found.  The search in order that
  foodspots are returned by look-fn."
  [look-fn _ x0 y0 x1 y1]
  (let [^doubles info (look-fn)
        perc-radius (hf/dnth info 0)
        last-index (long (hf/dnth info 1)) ; env size + 2
        near-pt-fn (partial um/near-pt-on-seg x0 y0 x1 y1)] ; Is this a good idea?
    (loop [i 2]
      (let [j (inc i)
            p (hf/dnth info i)
            q (hf/dnth info j)
            near-pt (near-pt-fn p q)
            near-x (hf/dnth near-pt 0)
            near-y (hf/dnth near-pt 1)
            distance (um/distance-2D* near-x near-y p q)]
        (cond (<= distance perc-radius) [[[p q]] [near-x near-y]] ; seq of single foodspot found, where found from
              (= j last-index) nil
              :else (recur (+ 2 i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEAVY MAKE-LOOK-FN, LIGHTWEIGHT FIND-IN-SEG

;; Shifting work from find-in-seg to look-fn to match original conception
#_
(defn make-look-fn
  [env ^double perc-radius]
  (fn [x0 y0 x1 y1]
    (let [last-index (count env)
          near-pt-fn (partial um/near-pt-on-seg x0 y0 x1 y1)] ; Is partial a good idea?
      (loop [foodspots env]
        (let [foodspot (first foodspots)
              p (hf/dnth foodspot 0)
              q (hf/dnth foodspot 1)
              near-pt (near-pt-fn p q)
              near-x (hf/dnth near-pt 0)
              near-y (hf/dnth near-pt 1)
              distance (um/distance-2D* near-x near-y p q)]
          (if (<= distance perc-radius)
            [[[p q]] [near-x near-y]] ; seq of single foodspot found, where found from
            (let [more-foodspots (next foodspots)]
              (if more-foodspots
                (recur more-foodspots)
                nil))))))))

;; VERSION FOR A SINGLE-SEQUENCE ARRAY OF COORDINATES:
(defn make-look-fn
  [^doubles env ^double perc-radius]
  (fn [x0 y0 x1 y1]
    (let [env-len (alength env)
          near-pt-fn (partial um/near-pt-on-seg x0 y0 x1 y1)] ; Is partial a good idea?
      (loop [i 0 j 1]
        (if (= j env-len)
          nil
          (hfl/let [p (hf/dnth env i)
                    q (hf/dnth env j)
                    [near-x near-y] (dbls (near-pt-fn p q))
                    distance (um/distance-2D* near-x near-y p q)]
            (if (<= distance perc-radius) [[[p q]] [near-x near-y]] ; seq of single foodspot found, where found from
              (recur (inc i) (inc j)))))))))

;; Use ham-fisted's primitive invoke?  No, can't because look-fn's
;; return value is too complex.
(defn find-in-seg
  [look-fn _ x0 y0 x1 y1]
  (look-fn x0 y0 x1 y1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHER VERSIONS OF FIND-IN-SEG, MAKE-LOOK-FN

;; The new ham-fisted let is not yet flexible enough for vectors with
;; length only known at runtime, because you don't know in advance what
;; variable names to assign.
;; This was an attempt, but it doesn't work.
;; Note that look-fn plays a different role here than in walks/find-in-seg, as it must.
;; Version using new ham-fisted typed destructuring macros
#_
(defn find-in-seg
  "Only returns the first foodspot found.  The search in order that
  foodspots are returned by look-fn."
  [look-fn _ x0 y0 x1 y1]
  (hfl/let [[perc-radius last-index target-coords] (dbls (look-fn))]
    (println (class target-coords))
    (let [last-index (long last-index)
          near-pt-fn (partial um/near-pt-on-seg x0 y0 x1 y1)] ; Is this a good idea?
    (loop [i 0]
      (hfl/let [j (inc i)
                p (hf/dnth target-coords i)
                q (hf/dnth target-coords j)
                [near-x near-y] (near-pt-fn p q)
                distance (um/distance-2D* near-x near-y p q)]
        (cond (<= distance perc-radius) [[[p q]] [near-x near-y]] ; seq of single foodspot found, where found from
              (= j last-index) nil
              :else (recur (inc i))))))))


;; Another version
#_
(defn make-look-fn
  [env ^double perc-radius]
  (constantly
    (hf/double-array
      (conj [perc-radius (+ 1.0 (* 2.0 (count env)))] ; second element is index of last coordinate
            (apply concat env)))))
