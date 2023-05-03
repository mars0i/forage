;; Same as grid8.clj but with a little bit of cleanup and minor mods, 
;; e.g. lengthening maxpathlen, 
;; moving assoc variants in the comment experiments into the main params map.
;; Grid8 was identical to grid7 except for trunclen look-eps, and init-pad.
;; and to grid6 except for trunclen, look-eps, init-pad, and maxpathlen.
;; i.e. up to the experiments comment section.
;; Similar to grid5.
(ns forage.experiment.grid9
  (:require [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
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
             :init-loc-fn         (constantly [half-size half-size]) ; function to return initial location given nil or prev foodwalk
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          (* 2000 half-size) ; max total length of search path
             :trunclen            1500 ; max length of any line segment
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


;; PARAMS FOR NON-DESTRUCTIVE/ASSYMETRIC SEARCH
(def assym-params (assoc params :init-pad (* 2 (params :perc-radius))))


;; ENV FOR DESTRUCTIVE/SYMMETRIC SEARCH;
(def nocenter-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/centerless-rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def noctr-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))


;; ENV FOR NONDESTRUCTIVE/ASYMMETRIC SEARCH;
(def centered-env (mf/make-env (params :env-discretization)
                      (params :env-size)
                      (f/rectangular-grid (params :food-distance)
                                                     (params :env-size)
                                                     (params :env-size))))

(def ctrd-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))


(comment

  (def seed (r/make-seed))
  ;; nondestructive/assymetric:
  (def data-rng-assym (time (fr/levy-experiments fr/default-file-prefix centered-env assym-params [1.001 1.5 1.8 2.0 2.5 3.0] 1000 seed ctrd-look-fn)))
  ;; destructive/symetric:
  (def data-rng-symm  (time (fr/levy-experiments fr/default-file-prefix nocenter-env params       [1.001 1.5 1.8 2.0 2.5 3.0] 1000 seed noctr-look-fn)))

  (def yo (time (fr/levy-experiments fr/default-file-prefix nocenter-env 
                                     params [1.5 2.0 3.0] 5 seed noctr-look-fn)))


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
