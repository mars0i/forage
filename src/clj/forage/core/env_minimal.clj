;; Functions for minimal, fast environments containing a 
;; single or a few foodspots, without toroidal lookup.
(ns forage.core.env-minimal
  (:require [utils.math :as um]
            [forage.core.food :as f]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT THAT CONSISTS OF A BARE COORDINATE PAIR

;; NOTE Since no size, doesn't support toroidal lookup.
(defn make-single-foodspot-env
  "Make an environment that consists of a single foodspog, and only a
  single point.  The environment returned is is simply a pair [x y] of
  coordinates for that foodspot."
  [x y]
  [x y])

(defn perc-single-foodspot
  "Returns a collection containing the sole foodspot if it's within
  perc-radius of forager-coords (an x, y Clojure pair), or nil if not.
  This is designed to work *only* with environments that are nothing more
  than a pair representing a single foodspot, as created by
  make-single-food-spot-env. If the env has only a single foodspot,
  but it's in a collection, use perc-mulitple-foodspots instead.
  Example usage:
     (def look-fn (partial env (params :perc-radius)))"
  [env perc-radius forager-coords]
  (if (<= (um/distance-2D forager-coords env) perc-radius)
    [env]
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT THAT CONSISTS OF A COLLECTION of COORDINATE PAIRS.

(defn make-multiple-foodspot-env
  "Make an environment that consists of a vector of pairs representing
  foodspots. coords should be a sequence of x,y pairs."
  [coords]
  (vec coords))

(defn perc-multiple-foodspots
  "Returns a vector containing the the first foodspot within perc-radius
  of forager-coords (an x, y Clojure pair), or nil no foodspot is found.
  foodspots are tested in the order they are listed in env."
  [env perc-radius forager-coords]
  (loop [foodspots env]
    (if (empty? foodspots)
      nil ; none found within perc-radius
      (let [fs (first foodspots)]
        (if (<= (um/distance-2D forager-coords fs) perc-radius)
          [fs]
          (recur (rest foodspots)))))))
