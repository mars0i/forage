;; Functions for working with foodspots using MASON's Continuous2D
;; as an environment to allow using MASON's 2D floating point locating
;; functions.
(ns forage.core.env-mason
    (:import [sim.field.continuous Continuous2D]
             [sim.util Double2D]) ; Bag
    (:require [forage.core.food :as f]))

;; The class annotations below (^Foodspot, ^Continuous2D) make the
;; functions faster, with a speed improvement of about 20X when used
;; as part of a food search run.

;; NOTE DON'T USE .getNeighborsWithinDistance .
;; It can return foodspots that are too far away.


;; NOTE ENV-MASON treats perceptual radius as a property of the
;; forager, so it's not represented in the env.  To get the perceptual
;; radius into the look-fn, it has to be supplied from an independent
;; source from the env.
;; By contrast, ENV-MATRIX treats perceptual radii as properties of
;; foodspots, so they are specified in the env, and look-fns have
;; to get them from the env (in effect).

(deftype Foodspot [x y nutrition])

(defn make-foodspot
  ([x y] (->Foodspot x y 1))
  ([x y nutrition] (->Foodspot x y nutrition)))

(defn foodspot-coords
  "Returns the coordinates of a foodspot."
  [^Foodspot foodspot]
  [(.x foodspot) (.y foodspot)])

(defn foodspot-coords*
  "Does the same thing as foodspot-coords, but without the type hint.  Therefore
  this function might be slightly slower in some contexts, but it can be more
  flexible when this file is repeatedly reloaded during testing."
  [foodspot]
  [(.x foodspot) (.y foodspot)])

(defn add-foodspots!
  "Given an env which is a MASON Continuous2D, adds Foodspots to it at 
  coordinate pairs listed in locs.  If a sequence of nutrition-values is
  not provided, the nutrition field of all new foodspots will be set to 1."
  ([env locs] (add-foodspots! env locs (repeat (count locs) 1)))
  ([^Continuous2D env locs nutrition-values]
   (doseq [[[x y] nutrition] (map vector locs nutrition-values)]
          (.setObjectLocation env
                              (->Foodspot x y nutrition)
                              (Double2D. x y)))))

(def add-foodspots add-foodspots!) ; backwards compatibility

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
     (add-foodspots! env locs)
     env)))

(defn env-size
  "Return the width (= height) of env."
  [^Continuous2D env]
  (.width env))

;; NOTE: I call seq on results below to turn the result into nil
;; if the result is empty.

;; TODO ? Don't call seq?
(defn env-foodspots
  "Returns a sequence of all foodspots in environment env, or nil
  if there are none."
  [^Continuous2D env]
  (seq (.getAllObjects env)))

;; alias for older code
(def all-foodspots env-foodspots)

(defn env-foodspot-coords
  "Returns a collection of coordinate pairs of all foodspots in environment
  env, or nil if there are none."
  [^Continuous2D env]
  (seq (map foodspot-coords (.getAllObjects env))))

;; alias for older code
(def all-foodspot-coords env-foodspot-coords)

;; Testing for emptiness of a MASON Bag and possibly returning it
;; as is faster than calling seq to convert it into a Clojure sequence.
;; Note that Bags are still truthy, and they can be converted into seqs--
;; usually transparently--at any time.
(defn perc-foodspots-exactly
  "Returns a MASON Bag of foodspots within perc-radius of (x,y),
  or nil if there are none.  Uses Continuous2D's local cell lookup."
  [^Continuous2D env perc-radius x y]
  (let [foodspots-bag (.getNeighborsExactlyWithinDistance env
                                                          (Double2D. x y)
                                                          perc-radius)]
    (if (.isEmpty foodspots-bag) nil foodspots-bag)))

(defn perc-foodspots-exactly-toroidal
  "Returns a MASON Bag of foodspots within perc-radius of (x,y), or 
  nil if there are none.  Uses Continuous2D's local toroidal cell lookup."
  [^Continuous2D env perc-radius x y]
  (let [foodspots-bag (.getNeighborsExactlyWithinDistance env
                                                          (Double2D. x y)
                                                          perc-radius true)]
    (if (.isEmpty foodspots-bag) nil foodspots-bag)))

;; TODO ? Don't call seq?
(defn perc-foodspot-coords-exactly
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  or nil if there are none.  Uses Continuous2D's local cell lookup."
  [env perc-radius x y]
  (seq (map foodspot-coords
            (perc-foodspots-exactly env perc-radius x y))))

;; Once we have all possible foodspot coordinates, we don't need to
;; use MASON lookup for a linear search.   This is probably slower
;; than using MASON's cell-based search.
(defn perc-foodspot-coords-linear
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  with possible additional ones, or nil if there are none.  Performs a linear 
  search through all foodspots in env."
  [env perc-radius coords]
  (f/perc-foodspot-coords-in-coll (env-foodspot-coords env)
                                  perc-radius coords))

(defn foodspot-coords-if-found
  "Given a sequence of foodspots returned by walks/levy-foodwalk or
  walks/straight-foodwalk, returns the coordinates of the first found
  foodspot, or nil if there are none."
  [found-foodspot-seq]
  (if found-foodspot-seq
    (foodspot-coords (first found-foodspot-seq))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New general foodspot finding methods with
;; same names and interfaces as those in env-matrix


;; Note that while matrix-perc-foodspots has env-getxy as first
;; parameter, this has perc-radius instead.  These two parameters
;; perform different functions.  In env-matrix, env-getxy implements
;; toroidal vs trim.  Here we handle that differently, but need to
;; pass perc-radius, which isn't needed here for in env-matrix since
;; it handles perceptual radii.
(defn mason-perc-foodspots
  [toroidal? perc-radius order-found ^Continuous2D env x y]
  (let [location (Double2D. x y)
        foodspot-here (first (.getObjectsAtLocation env location)) ; assume only one
        foodspots-here-and-near (.getNeighborsExactlyWithinDistance env
                                                                    location
                                                                    perc-radius
                                                                    toroidal?)
        foodspots-near (remove (identical? foodspot-here) foodspots-here-and-near)
        coords-here (foodspot-coords foodspot-here)
        coords-near (map foodspot-coords foodspots-near)]
    (order-found coords-here coords-near)))

(defn radius-perc-foodspots-trimmed
  "Runs mason-perc-foodspots with perc-radius, ignoring coordinates that
  fall outside the boundaries of env.  See mason-perc-foodspots for further
  information about parameters."
  [perc-radius order-found env x y]
  (mason-perc-foodspots false perc-radius order-found env x y))

(defn radius-perc-foodspots-toroidally
  "Runs mason-perc-foodspots perc-radius, with toroidal wrapping of
  coordinates that fall outside the boundaries of env.  See
  mason-perc-foodspots for further information about parameters."
  [perc-radius order-found env x y]
  (mason-perc-foodspots true perc-radius order-found env x y))
