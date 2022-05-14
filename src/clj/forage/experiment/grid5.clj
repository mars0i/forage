(ns forage.experiment.grid5
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))


(def half-size 5000) ; half the full width of the env
(def init-food 200)

(def params (sorted-map ; sort so labels match values
             :food-distance       init-food ; ignored??
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  init-food
             :init-loc            [half-size half-size] ; i.e. center of env
             :maxpathlen          (* 4 half-size)  ; for straight walks, don't go too far
             :trunclen            (* 4 half-size) ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     nil
             :fournier-multiplier nil
            ))

(def straight-params (assoc params :num-dirs 100))

(def env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def look-fn (partial mf/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(comment

  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  (def mu 2.0)
  (def fw (fr/levy-run rng look-fn nil params mu))
  (time (def fws (doall (repeatedly 18 #(fr/levy-run rng look-fn nil params mu)))))
  (count (filter first fws))
  (def sorted-fws (w/sort-foodwalks fws))
  (fr/write-foodwalk-plots 
           (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu" mu)
           :svg seed env 800 9 3 50 mu params sorted-fws)

  (fr/straight-experiments 
           (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yostraight")
           env straight-params)

)
