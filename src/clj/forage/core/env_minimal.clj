;; Functions for a minimal, fast environment containing a 
;; single or a few foodspots, without toroidal lookup.
(ns forage.core.env-minimal
  (:require [utils.math :as um]
            [forage.core.food :as f]))

;; NOTE Since no size, doesn't support toroidal lookup.
(defn make-single-foodspot-env
  [x y]
  [x y])

(defn make-multiple-foodspot-env
  "coords should be a sequence of x,y pairs."
  [coords]
  (vec coords))

(defn perc-single-foodspot
  "Returns a vector containing the sole foodspot if it's within perc-radius
  of forager-coords (an x, y Clojure pair), or nil if not.  Example usage:
    (def look-fn (partial env (params :perc-radius)))"
  [env perc-radius forager-coords]
  (if (<= (um/distance-2D forager-coords env) perc-radius)
    [env]
    nil))


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
