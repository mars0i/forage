(ns forage.explore.grid5
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
(def half-size 10000) ; half the full width of the env
;(def food-distance 500)
(def food-distance 2000)
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

(def fourn-env (mf/make-env (params :env-discretization)
                            (params :env-size)
                            (f/fournierize (mf/all-foodspot-coords grid-env)
                                           1000 0.2 3)))

(def fourn-look-fn (partial mf/perc-foodspots-exactly fourn-env (params :perc-radius)))

(comment
  (do
    (def seed (r/make-seed))
    (def rng (r/make-well19937 seed))
    (def ffw+ (fr/levy-run rng fourn-look-fn nil params 2))
    (oz/view! (h/vega-envwalk-plot fourn-env 1100 20 [ffw+]))
   )

  (def ffw+ (fr/levy-run (r/make-well19937) fourn-look-fn nil params 3))
  (oz/view! (h/vega-envwalk-plot fourn-env 1100 20 [ffw+]))

  (def gfw+ (fr/levy-run rng grid-look-fn nil params 2))
  (def gfw+ (fr/levy-run (r/make-well19937) grid-look-fn nil params 2))
  (oz/view! (h/vega-envwalk-plot grid-env 1100 50 [gfw+]))

  (require '[oz.core :as oz])
  (oz/start-server!)

  (time (fr/levy-experiments fr/default-file-prefix grid-env (r/make-seed) params exponents walks-per-combo))
)

