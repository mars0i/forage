(ns forage.mason.food
    (:import [sim.field.continuous Continuous2D]
             [sim.util Double2D]) ; Bag
    (:require [forage.food :as f]))

;; In the MASON 20 manual, Sean Luke recommends storing an object's
;; coordinates within it so that you don't have to look up the coordinates
;; in a Continuous2D if you have the object in hand.  I don't think there
;; will be such a need with Foodspots, but it's no big deal to store
;; their coordinates.  Also, I don't initially vary nutritiousness of
;; foodpots, but I can imagine this might be worth varying later.
(deftype Foodspot [x y nutritiousness])

(defn add-foodspots
  "Given an env which is a MASON Continuous2D, adds Foodspots to it at 
  coordinates listed in locs.  The nutritiousness field of each Foodspot
  will be set to 1."
  [env locs]
  (doseq [[x y] locs]
     (.setObjectLocation env (->Food x y 1) (Double2D. x y))))

(defn make-env
  "Returns a MASON Continuous2D to function as a square environment in 
  which foodspots can be placed.  size is the length of sizes of the
  square.  discretization is the size, in length, of buckets
  into which the environment is divided.  (discretization doesn't affect 
  functionality, but its choice can affect efficiency of foodspot searches.
  See chapter 6 of the MASON v20 manual for more.)  If locs is provided,
  it should be a sequence of coordinate pairs for foodspots, which will be 
  added to the new environment."
  ([discretization size] (Continuous2D. discretization size size))
  ([discretization size locs]
   (let [env (make-env discretization size)]
     (add-foodspots env locs)
     env)))

(defn perceptible-foodspots
  "Returns a sequence of foodspots within perc-radius of (x,y), or nil 
  if there are none."
  [env perc-radius [x y]]
  (seq (.getNeighborsExactlyWithinDistance env
                                           (Double2D. x y)
                                           perc-radius)))
