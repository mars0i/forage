;; Identical to grid7 except for trunclen look-eps, and init-pad.
;; Identical to grid6 except for trunclen, look-eps, init-pad, and maxpathlen.
;; i.e. up to the experiments comment section.
;; Similar to grid5.
(ns forage.experiment.grid8
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.math :as m]))


(def half-size 5000) ; half the full width of the env
(def init-food 1000)

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       init-food
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn  (constantly [half-size half-size])
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          (* 1000 half-size) ; max total length of search path
             :trunclen            10000 ; max length of any line segment
             :look-eps            0.2    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     nil
             :fournier-multiplier nil
            ))

;; PARAMS FOR NON-RANDOM STRAIGHT RUNS that systematically try a series of directions:
;; For Levy walks, :num-dirs is set to nil to ensure random initial directions.
;; So this has to be overridden for a pre-specified spread of straight walks:
(def straight-params (assoc params :num-dirs 100))

;; PARAMS FOR NON-DESTRUCTIVE SEARCH
;(def nondestr-params (assoc params :init-pad (+ (* 2 (params :look-eps)) (params :perc-radius))))
;(def nondestr-params (assoc params :init-pad (* 2 (params :perc-radius))))
(def nondestr-params (assoc params :init-pad (* 5 (params :perc-radius))))
;(def nondestr-params (assoc params :init-pad (* 10 (params :perc-radius))))
;(def nondestr-params (assoc params :init-pad (* 50 (params :perc-radius))))

(def nocenter-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def centered-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def noctr-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))
(def ctrd-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))


(comment

  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data-file-generating exeriment: nondestructive foraging

  (def seed (r/make-seed))

  (def data-and-rng  (time (fr/levy-experiments fr/default-file-prefix centered-env nondestr-params [2.0 2.5 3.0] 1000 seed ctrd-look-fn)))

  (def nondestr-params-shorttrunclen (assoc nondestr-params :trunclen 5000))
  (def data-and-rng-short  (time (fr/levy-experiments fr/default-file-prefix centered-env nondestr-params-shorttrunclen [2.0 2.5 3.0] 1000 seed ctrd-look-fn)))

  (def nondestr-params-shorttrunclen (assoc nondestr-params :trunclen 2500))
  (def data-and-rng-short  (time (fr/levy-experiments fr/default-file-prefix centered-env nondestr-params-shorttrunclen [2.0 2.5 3.0] 1000 seed ctrd-look-fn)))

  (def rng (:rng data-and-rng))
  (def data-and-rng2 (time (fr/levy-experiments fr/default-file-prefix centered-env nondestr-params [1.001 1.5] 1000 seed ctrd-look-fn rng)))

  (def nondestr-params-shorttrunclen (assoc nondestr-params :trunclen 2000))
  (def data-and-rng-2000  (time (fr/levy-experiments fr/default-file-prefix centered-env nondestr-params-shorttrunclen [1.001 1.5 2.0 2.5 3.0] 1000 seed ctrd-look-fn)))

  (def nondestr-params-shorttrunclen (assoc nondestr-params :trunclen 1500))
  (def data-and-rng-1500  (time (fr/levy-experiments fr/default-file-prefix centered-env nondestr-params-shorttrunclen [1.001 1.5 2.0 2.5 3.0] 1000 seed ctrd-look-fn)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Plotting
  ;; The mu's here are merely used for informational output.
  (let [env nocenter-env
        mu 2.0
        n-to-plot 1]
    (time
     (fr/write-foodwalk-plots 
      (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu" mu)
      :svg seed env 800 1 1 100 500 mu params (take n-to-plot (w/sort-foodwalks fws)))))
  ;:svg seed env 800 12 3 nil 50 mu params (take n-to-plot (w/sort-foodwalks fws)))

)
