(ns forage.explore.fournier
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [forage.viz.hanami :as h]
            [utils.random :as r]
            [utils.math :as m]))


(def seed (r/make-seed))
(println seed)
(def rng (r/make-well19937 seed))


(def exponents [2])
;(def exponent 2)
(def walks-per-combo 1)
;(def half-size 50000) ; half the full width of the env
(def half-size 20000) ; half the full width of the env
;(def food-distance 500)
(def food-distance 5000)
(def params (sorted-map ; sort so labels match values
              :food-distance     food-distance
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :powerlaw-min      1
              :env-size          (* 2 half-size)
              :env-discretization food-distance
              :init-loc          [half-size half-size] ; i.e. center of env
              :maxpathlen        (* 4 half-size)  ; for straight walks, don't go too far
              :trunclen          (* 4 half-size ) ; max length of any line segment
              :look-eps          0.1  ; increment within segments for food check
              :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
              :num-dirs          nil ; split range this many times + 1 (includes range max); nil for random
             ))

(def grid-env (mf/make-env (params :env-discretization)
                           (params :env-size)
                           (f/centerless-rectangular-grid (params :food-distance)
                                                          (params :env-size)
                                                          (params :env-size))))

(def grid-look-fn (partial mf/perc-foodspots-exactly grid-env (params :perc-radius)))

(def fournier-multiplier 0.2)
;; example:
;; with food-distance = 5000, multiplier = 0.1, four levels means adding
;; points at these distances from the points that are 5000 apart:
;; 500, 50, and 5, 0.5
;; With multiplier = 0.2, the additional distances are:
;; 1000, 200, 40, 8

(def fourn-env (mf/make-env (params :env-discretization)
                            (params :env-size)
                            (f/fournierize (mf/all-foodspot-coords grid-env)
                                           food-distance fournier-multiplier 4)))

(def fourn-look-fn (partial mf/perc-foodspots-exactly fourn-env (params :perc-radius)))


(def grid-env-with-center (mf/make-env (params :env-discretization)
                                       (params :env-size)
                                       (f/rectangular-grid (params :food-distance)
                                                           (params :env-size)
                                                           (params :env-size))))

(def fourn-env-with-center (mf/make-env (params :env-discretization)
                            (params :env-size)
                            (f/fournierize (mf/all-foodspot-coords grid-env-with-center)
                                           food-distance fournier-multiplier 4)))

(def fourn-with-center-look-fn
  (partial mf/perc-foodspots-exactly fourn-env-with-center (params :perc-radius)))



(comment
  ;; centerless Fournier:
  (time (def ffw+ (fr/levy-run (r/make-well19937) fourn-look-fn nil params 2)))
  (oz/view! (h/vega-envwalk-plot fourn-env 1100 50 [ffw+]))

  ;; Fournier with center:
  (time (def cfw+ (fr/levy-run (r/make-well19937) fourn-with-center-look-fn nil params 3)))
  (oz/view! (h/vega-envwalk-plot fourn-env-with-center 1100 50 [cfw+]))
  (first cfw+)
  (mf/foodspot-coords (first (first cfw+)))

  ;; centerless grid:
  (time (def gfw+ (fr/levy-run (r/make-well19937) grid-look-fn nil params 2)))
  (oz/view! (h/vega-envwalk-plot grid-env 1100 50 [gfw+]))

  (require '[oz.core :as oz])
  (oz/start-server!)

  (do
    ;; centerles Fournier
    (def seed (r/make-seed))
    (def rng (r/make-well19937 seed))
    (def ffw+ (fr/levy-run rng fourn-look-fn nil params 2))
    (oz/view! (h/vega-envwalk-plot fourn-env 1100 20 [ffw+]))
   )

  (time (fr/levy-experiments fr/default-file-prefix grid-env (r/make-seed) params exponents walks-per-combo))
)

