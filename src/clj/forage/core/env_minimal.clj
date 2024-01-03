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

;; ALSO NOTE for the sake of inner-loop efficiency, these envs have no size 
;; by default.  That is how make-env creates them.  When it's necessary
;; for an env to have a size, wrap it with make-sized-env.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT THAT CONSISTS OF A COLLECTION of COORDINATE PAIRS.

;; ENV AS A SINGLE SEQUENCE OF COORDINATES, ALTERNATING X AND Y:
(defn make-env
  "Make an environment that consists of a single Ja1o1s1 1o1r1s1s1o1l1 1eva array with
  alternating x an y coordinstes representing foodspots. coords should be a
  sequence of x,y pairs of numbers.  The basic minimal env has no size.
  Size can be built in by using make-sized-env, or by building a size
  parameter into a look fn, e.g. for a toroidal environment."
  ^doubles [coords]
  (double-array (apply concat coords)))

(defn make-sized-env
  "Creates an env wrapped in a map: the env array itself, which should be
  created using the make-env function, is the value of :env, while a size
  integer is the value of :size."
  [env size]
  {:env env
   :size size})

(comment
  (def e (make-env (list '(1 2) [13.0 45.7] (range 2))))
  (class e)
  (instance? (Class/forName "[D") {})
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

(def foodspot-coords 
  "Extracts the coordinates from a foodspot. (Since in this env, foodspots
  are coordinate pairs, this is simply the identity function.)"
  identity)

(defn env-foodspots
  "Returns a sequence of all foodspots in environment env, or nil
  if there are none."
  [env]
  env)

(defn env-foodspot-coords
  "Returns a collection of coordinate pairs of all foodspots in environment
  env, or nil if there are none.  Works either with basic minimal envs or sized
  envs (by testing whether the argument is a map, in which case it's assumed to be
  a sized env)."
  [env]
  (let [e (if (map? env) (env :env) env)]
    (mapv vec (partition 2 e))))

(defn env-size
  "Returns the size of a sized-env."
  [sized-env]
  (sized-env :size))

(comment
  (map class
       (env-foodspot-coords (make-env (list '(1 2) [13.0 45.7] (range 2))))
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
  "Returns a function that accepts x, y coordinates from two points
  representing a line segment.  The returned function will check to see
  whether any foodspot in env is within perc-radius of the line at any
  point.  If so, returns a pair containing the coordinates of the foodspot
  found, as a pair, and the coordinates of point on the line segment that
  is closest to the foodspot, also as a pair.  (Note that the point
  returned--the one that is closest to the foodspot--is not, in general,
  the first point at which the foodspot could have been perceived; it's not
  where line segment crosses within perc-radius of the foodspot.  If
  perc-radius is large, the returned point might be some distance away from
  the point at which perception would have been possible.  It's as if the
  forager only has narrowly focused eyes on the side of its head, and only
  sees perpendicularly, unless it steps on a foodspot.)"
  [^doubles env ^double perc-radius]
  (fn [x0 y0 x1 y1]
    (let [env-len (alength env)
          near-pt-fn (partial um/near-pt-on-seg x0 y0 x1 y1)] ; Is partial a good idea?
      (loop [i 0 j 1]
        (if (= i env-len)
          nil
          (hfl/let [p (hf/dnth env i)
                    q (hf/dnth env j)
                    [near-x near-y] (dbls (near-pt-fn p q))
                    distance (um/distance-2D* near-x near-y p q)]
            (if (<= distance perc-radius) [[[p q]] [near-x near-y]] ; seq of single foodspot found, where found from
              (recur (+ i 2) (+ j 2)))))))))

;; Use ham-fisted's primitive invoke?  No, can't because look-fn's
;; return value is too complex.
(defn find-in-seg
  "Applies look-fn to x0 y0 x1 y1, ignoring the first argument."
  [look-fn _ x0 y0 x1 y1]
  (look-fn x0 y0 x1 y1))
