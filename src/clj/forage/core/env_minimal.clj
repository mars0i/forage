;; Functions for minimal, fast environments containing a 
;; few foodspots, without toroidal lookup.
(ns forage.core.env-minimal
  (:require [ham-fisted.api :as hf]
            [fastmath.core :as fm]
            [forage.core.food :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(fm/use-primitive-operators)

;; TODO: Is there a reason to use a Java array rather than a Clojure vector
;; for the foodspots?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT THAT CONSISTS OF A COLLECTION of COORDINATE PAIRS.

(defn make-multiple-foodspot-env
  "Make an environment that consists of a vector of vector pairs of doubles
  representing foodspots. coords should be a sequence of x,y pairs of
  numbers."
  [coords]
  (mapv #(mapv double %) coords))

(comment
  (make-multiple-foodspot-env (list '(1 2) [13.0 45.7] (range 2)))
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
  env)


;; I could randomize the order of foodspots with a different look-fn.
(defn make-look-fn
  ^doubles [env ^double perc-radius]
  (constantly
    (hf/double-array
      (into [perc-radius (inc (count env))] ; second element is index of last coordinate
            (apply concat env)))))



;; Note that look-fn plays a different role here than in walks/find-in-seg,
;; as it must.
(defn find-in-seg
  "Only returns the first foodspot found.  The search in order that
  foodspots are returned by look-fn."
  [look-fn _ x0 y0 x1 y1]
  (let [^doubles info (look-fn)
        perc-radius (hf/dnth info 0)
        last-index (long (hf/dnth info 1)) ; env size + 1
        near-pt-fn (partial (um/near-pt-on-seg x0 y0 x1 y1))] ; Is this a good idea?
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
              :else (recur (inc i)))))))


