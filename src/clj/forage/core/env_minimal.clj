;; Functions for minimal, fast environments containing a 
;; few foodspots, without toroidal lookup.
(ns forage.core.env-minimal
  (:require [utils.math :as um]
            [forage.core.food :as f]))

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

#_
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
  env)
