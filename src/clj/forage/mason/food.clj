;; Functions for working with foodspots using MASON's Continuous2D
;; as an environment to allow using MASON's 2D floating point locating
;; functions.
(ns forage.mason.food
    (:import [sim.field.continuous Continuous2D]
             [sim.util Double2D]) ; Bag
    (:require [forage.food :as f]))

;; The class annotations below (^Foodspot, ^Continuous2D) make the
;; functions faster, with a speed improvement of about 20X when used
;; as part of a food search run.

(deftype Foodspot [x y nutrition])

(defn make-foodspot
  ([x y] (->Foodspot x y 1))
  ([x y nutrition] (->Foodspot x y nutrition)))

(defn foodspot-coords
  [^Foodspot foodspot]
  [(.x foodspot) (.y foodspot)])

(defn add-foodspots
  "Given an env which is a MASON Continuous2D, adds Foodspots to it at 
  coordinate pairs listed in locs.  If a sequence of nutrition-values is
  not provided, the nutrition field of all new foodspots will be set to 1."
  ([env locs] (add-foodspots env locs (repeat (count locs) 1)))
  ([^Continuous2D env locs nutrition-values]
   (doseq [[[x y] nutrition] (map vector locs nutrition-values)]
          (.setObjectLocation env
                              (->Foodspot x y nutrition)
                              (Double2D. x y)))))

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

;; NOTE: I call seq on results below to turn the result into nil
;; if the result is empty.

(defn all-foodspots
  "Returns a sequence of all foodspots in environment env, or nil
  if there are none."
  [^Continuous2D env]
  (seq (.getAllObjects env)))

(defn all-foodspot-coords
  "Returns coordinate pairs of all foodspots in environment env, or nil
  if there are none."
  [^Continuous2D env]
  (seq (map foodspot-coords (.getAllObjects env))))

(defn perc-foodspots-exactly
  "Returns a sequence of foodspots within perc-radius of (x,y),
  or nil if there are none.  Uses Continuous2D's local cell lookup."
  [^Continuous2D env perc-radius [x y]]
  (seq (.getNeighborsExactlyWithinDistance env (Double2D. x y) perc-radius)))

(defn perc-foodspot-coords-exactly
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  or nil if there are none.  Uses Continuous2D's local cell lookup."
  [env perc-radius [x y]]
  (seq (map foodspot-coords
            (perc-foodspots-exactly env perc-radius [x y]))))

;; Once we have all possible foodspot coordinates, we don't need to
;; use MASON lookup for a linear search.   This is probably slower
;; than using MASON's cell-based search.
(defn perc-foodspot-coords-linear
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  with possible additional ones, or nil if there are none.  Performs a linear 
  search through all foodspots in env."
  [env perc-radius coords]
  (f/perc-foodspot-coords-in-coll (all-foodspot-coords env)
                                  perc-radius coords))



;; DON'T USE: CAN RETURN FOODSPOT--IN CELL--THAT'S TOO FAR
(defn dont-use-perc-foodspots-plus
  "Returns a sequence of foodspots within perc-radius of (x,y), possibly 
  with additional ones in the same Continous2D cell, or nil if there are none."
  [^Continuous2D env perc-radius [x y]]
  (seq (.getNeighborsWithinDistance env (Double2D. x y) perc-radius)))

;; DON'T USE: CAN FOODSPOT--IN CELL--THAT'S TOO FAR
(defn dont-use-perc-foodspot-coords-plus
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  with possible additional ones from foodspots in the same Continuous2D cell, 
  or nil if there are none."
  [env perc-radius [x y]]
  (seq (map foodspot-coords
            (dont-use-perc-foodspots-plus env perc-radius [x y]))))
