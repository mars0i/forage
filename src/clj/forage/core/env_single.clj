;; Functions for a minimal, fast environment containing a 
;; single foodspot, without toroidal lookup.
(ns forage.core.env-single
  (:require [utils.math :as um]
            [forage.core.food :as f]))

;; NOTE Since no size, doesn't support toroidal lookup.
(defn make-env
  [x y]
  [x y])

(defn perc-foodspots
  "Returns a vector containing the sole foodspot if it's within perc-radius
  of forager-coords (an x, y Clojure pair), or nil if not.  Example usage:
    (def look-fn (partial env (params :perc-radius)))"
  [env perc-radius forager-coords]
  (if (<= (um/distance-2D forager-coords env) perc-radius)
    [env]
    nil))

;; No need to use this, but it exists in other env namespaces.
(defn foodspot-coords [env] env)
