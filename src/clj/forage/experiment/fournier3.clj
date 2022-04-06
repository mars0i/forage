;; SPARSE FOURNIER ENVIRONMENT WITHOUT CENTER
;; Large env, large food distance, small Fournier multiplier.
;; Both Levy and straight walks defined.
(ns forage.experiment.fournier3
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))

(def all-exponents [1.001 1.5 2.0 2.5])
(comment (count all-exponents) )

(def walks-per-combo 500)

(def half-size 50000) ; half the full width of the env
(def food-distance 10000)
(def params (sorted-map ; sort so labels match values
             :food-distance     food-distance
             :perc-radius       1  ; distance that an animal can "see" in searching for food
             :powerlaw-min      1
             :env-size          (* 2 half-size)
             :env-discretization food-distance
             :init-loc          [half-size half-size] ; i.e. center of env
             :maxpathlen        half-size  ; for straight walks, don't go too far
             :trunclen          half-size ; max length of any line segment
             :look-eps          0.1    ; increment within segments for food check
             :num-dirs          100    ; split range this many times + 1 (includes range max); nil for random
             :max-frac          0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     2    ; levels in addition to the top level
             :fournier-multiplier 0.05 ; how much to shrink distance for each Fournier level
            ))
;; with food-distance = 10000, multiplier = 0.05, threer levels means
;; adding ;; points at these distances from the points that are 10000 apart:
;; 500, 25, and 1.25.  
;; The last is not OK--so don't use 3 levels with that distance and multiplier.

;; 
(def grid-env (mf/make-env (params :env-discretization)
                                       (params :env-size)
                                       (f/centerless-rectangular-grid (params :food-distance)
                                                           (params :env-size)
                                                           (params :env-size))))

;; Fournier env without center:
(def env
  (mf/make-env (params :env-discretization)
               (params :env-size)
               (f/fournierize (mf/all-foodspot-coords grid-env)
                              food-distance
                              (params :fournier-multiplier)
                              (params :fournier-levels))))

(comment
  (require '[forage.run :as fr])
  (require '[utils.random :as r])

  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params all-exponents walks-per-combo))

  (time (def data (fr/straight-experiments fr/default-file-prefix env params)))
)

