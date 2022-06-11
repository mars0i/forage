;; FOURNIER ENVIRONMENT WITHOUT CENTER
;; Odd thing: ballistic searches go outside env, but then
;; I restart them in center.
(ns forage.experiment.fournier1
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))

(def all-exponents [1.001 1.5 2.0 2.5 3])
(comment (count all-exponents) )
(def exponents1 (vec (take 2 all-exponents)))
(def exponents2 (vec (take 2 (drop 2 all-exponents))))
(def exponents3 [3])

(def walks-per-combo 1000)

(def half-size 20000) ; half the full width of the env
(def food-distance 5000)
(def params (sorted-map ; sort so labels match values
             :food-distance     food-distance
             :perc-radius       1  ; distance that an animal can "see" in searching for food
             :powerlaw-min      1
             :env-size          (* 2 half-size)
             :env-discretization food-distance
             :init-loc          [half-size half-size] ; i.e. center of env
             ;; Note long paths; ballistic searches will go outside the garden:
             :maxpathlen        (* 4 half-size)  ; for straight walks, don't go too far
             :trunclen          (* 4 half-size) ; max length of any line segment
             :look-eps          0.1  ; increment within segments for food check
             :num-dirs          nil ; split range this many times + 1 (includes range max); nil for random
             :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
            ))

(def fournier-levels 4)
(def fournier-multiplier 0.2)
;; example:
;; with food-distance = 5000, multiplier = 0.1, four levels means adding
;; points at these distances from the points that are 5000 apart:
;; 500, 50, and 5, 0.5
;; With multiplier = 0.2, the additional distances are:
;; 1000, 200, 40, 8

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
                              food-distance fournier-multiplier fournier-levels)))

(comment
  (require '[forage.run :as fr])
  (require '[utils.random :as r])

  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params all-exponents walks-per-combo))

  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params exponents1 walks-per-combo))
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params exponents2 walks-per-combo))
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params exponents3 walks-per-combo))
)

