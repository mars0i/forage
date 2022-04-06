(ns forage.explore.grid5
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))

;(def seed (inc (r/make-seed)))
;(println "SEED:" seed)

(def exponents [2])
(def walks-per-combo 1)
(def half-size 50000) ; half the full width of the env
(def food-distance 400)
(def params (sorted-map ; sort so labels match values
              :food-distance     food-distance
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :powerlaw-min      1
              :env-size          (* 2 half-size)
              :env-discretization food-distance
              :init-loc          [half-size half-size] ; i.e. center of env
              :maxpathlen        half-size  ; for straight walks, don't go too far
              :trunclen          half-size  ; max length of any line segment
              :look-eps          0.1  ; increment within segments for food check
              :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
              :num-dirs          nil ; split range this many times + 1 (includes range max); nil for random
             ))

(def grid-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))


(comment
  (time (fr/levy-experiments fr/default-file-prefix grid-env (r/make-seed) params exponents walks-per-combo))
)

