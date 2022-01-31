(ns forage.mason.food
    (:import [sim.field.continuous Continuous2D]
             [sim.util Double2D]) ; Bag
    (:require [forage.food :as f]))

(deftype Foodspot [x y nutrition])

(defn make-foodspot
  ([x y] (->Foodspot x y 1))
  ([x y nutrition] (->Foodspot x y nutrition)))

(defn foodspot-coords
  [foodspot]
  [(.x foodspot) (.y foodspot)])

(defn add-foodspots
  "Given an env which is a MASON Continuous2D, adds Foodspots to it at 
  coordinate pairs listed in locs.  If a sequence of nutrition-values is
  not provided, the nutrition field of all new foodspots will be set to 1."
  ([env locs] (add-foodspots env locs (repeat (count locs) 1)))
  ([env locs nutrition-values]
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
  [env]
  (seq (.getAllOjects env)))

(defn all-foodspot-coords
  "Returns coordinate pairs of all foodspots in environment env, or nil
  if there are none."
  [env]
  (seq (map foodspot-coords (.getAllOjects env))))

(defn perceptible-foodspots
  "Returns a sequence of foodspots within perc-radius of (x,y),
  or nil if there are none."
  [env perc-radius [x y]]
  (seq (.getNeighborsExactlyWithinDistance env (Double2D. x y)
                                                perc-radius)))

(defn perceptible-foodspot-coords
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  or nil if there are none."
  [env perc-radius [x y]]
  (seq (map foodspot-coords
            (perceptible-foodspots env perc-radius [x y]))))
