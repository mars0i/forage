;; Functions for minimal, fast environments containing a 
;; single foodspot, without toroidal lookup.
(ns forage.core.env-single
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
  [env ^double perc-radius ^double x ^double y] 
  (if (<= (um/distance-2D* x y (env 0) (env 1)) perc-radius)
    [env]
    nil))

(def foodspot-coords 
  "Extracts the coordinates from a foodspot. (Since in this env, foodspots
  are coordinate pairs, this is simply the identity function.)"
  identity)

(defn env-foodspots
  "Returns a collection containing the sole foodspot in environment env, or
  nil if there are none."
  [env]
  [env])

(defn env-foodspot-coords
  "Returns a collection containing the sole coordinate pair of a foodspot
  in environment env, or nil if there are none."
  [env]
  [env])