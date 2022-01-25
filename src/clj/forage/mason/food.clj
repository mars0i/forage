(ns forage.mason.food
    (:import [sim.field.continuous Continuous2D]
             [sim.util Double2D])
    (:require [forage.food :as f]))

(deftype Food [x y nutrients])

(defn add-foodspots
  "Add foodspots at coordinates listed in locs to env, which is
  a MASON Continuous2D.  nutrients will be set to 1."
  [env locs]
  (doseq [[x y] locs]
     (.setObjectLocation env (->Food x y 1) (Double2D. x y))))
