(ns forage.experiment.grid5
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))


;; with notes about preliminary results:
(def half-size 5000) ; half the full width of the env
(def init-food 200) ; about 18% success straight, 30% success mu=2, 75% success mu=1.5 since food so dense and regular

(def half-size 5000) ; half the full width of the env
(def init-food 400)
   ; straight: 6/101
   ;---
   ; mu=3.0: 21/360
   ; mu=3.0: 30/360
   ; mu=3.0: 26/360
   ;---
   ; mu=2.5: 49/360
   ; mu=2.5: 44/360
   ;---
   ; mu=2.0: 69/360
   ; mu=2.0: 77/360
   ;---
   ; mu=1.5: 81/360
   ; mu=1.5: 89/360
   ;---
   ; mu=1.001: 84/360
   ; mu=1.001: 81/360

(def half-size 5000) ; half the full width of the env
(def init-food 1000)

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

  (def mu 3.0)
  (def mu 2.5)
  (def mu 2.0)
  (def mu 1.5)
  (def mu 1.1001)
  ;(def fw (fr/levy-run rng look-fn nil params mu))
  ;(time (def fws (doall (repeatedly 18 #(fr/levy-run rng look-fn nil params mu)))))
  ;(time (def fws (doall (repeatedly 24 #(fr/levy-run rng look-fn nil params mu)))))
  (time (def fws (doall (repeatedly 360 #(fr/levy-run rng look-fn nil params mu)))))
  (count (filter first fws))
  (def sorted-fws (w/sort-foodwalks fws))
  (fr/write-foodwalk-plots 
           (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu" mu)
           :svg seed env 800 12 3 50 mu params sorted-fws)
           ;:svg seed env 800 9 3 50 mu params sorted-fws)

  (fr/straight-experiments 
           (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yostraight")
           env straight-params)

)
