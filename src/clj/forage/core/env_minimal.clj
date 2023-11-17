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
(defn make-env
  "Make an environment that consists of a vector of Java array pairs of
  doubles representing foodspots. coords should be a sequence of x,y pairs
  of numbers."
  [coords]
  (mapv double-array coords))

;; ENV AS A SINGLE SEQUENCE OF COORDINATES, ALTERNATING X AND Y:
(defn make-env
  "Make an environment that consists of a single Java array with
  alternating x an y coordinstes representing foodspots. coords should be a
  sequence of x,y pairs of numbers."
  ^doubles [coords]
  (double-array (apply concat coords)))

(comment
  (map class (make-env (list '(1 2) [13.0 45.7] (range 2))))
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
  (mapv vec (partition 2 env)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEAVY MAKE-LOOK-FN, LIGHTWEIGHT FIND-IN-SEG

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
