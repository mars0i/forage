;; Modified version of grid9.clj: small but toroidal env
;; This version adds use of food/slide-grid in construction of envs.
;; Also uses a very long trunclen, whereas grid9 used a short trunclen.
;;
;; grid9 was same as grid8.clj but with a little bit of cleanup and minor mods, 
;; e.g. lengthening maxpathlen, 
;; moving assoc variants in the comment experiments into the main params map.
;; Grid8 was identical to grid7 except for trunclen look-eps, and init-pad.
;; and to grid6 except for trunclen, look-eps, init-pad, and maxpathlen.
;; i.e. up to the experiments comment section.
;; Similar to grid5.
(ns forage.experiment.grid17slide
  (:require [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-mason :as mf]
            [utils.random :as r]
            [utils.math :as m]))


(def half-size 5000) ; half the full width of the env
(def food-distance 1000)
(def slide-shift 900) ; optional right shift of every other foodspot
(def maxpathlen (* 2000 half-size)) ; max total length of search path

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance 
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size]) ; function to return initial location given nil or prev foodwalk
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            maxpathlen
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV FOR NONDESTRUCTIVE/ASYMMETRIC SEARCH--UNSHIFTED GRID
(def assym-params (assoc params :init-pad (* 2 (params :perc-radius))))

(def centered-env (mf/make-env (params :env-discretization)
                               (params :env-size)
                               (f/slide-grid (params :food-distance)
                                             0 0 ; slide shifts [could use rectangular-grid]
                                             (params :env-size) (params :env-size))))
(def ctrd-look-fn (partial mf/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))

(def shift-centered-env (mf/make-env (params :env-discretization)
                                       (params :env-size)
                                       (f/slide-grid (params :food-distance) 
                                                     slide-shift 0
                                                     (params :env-size) (params :env-size))))
(def shift-ctrd-look-fn (partial mf/perc-foodspots-exactly-toroidal
                                 shift-centered-env (params :perc-radius)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV FOR DESTRUCTIVE/SYMMETRIC SEARCH
(def nocenter-env (mf/make-env (params :env-discretization)
                               (params :env-size)
                               (f/remove-center (params :env-size) (params :env-size)
                                                (f/slide-grid (params :food-distance)
                                                              0 0 ; slide shifts
                                                              (params :env-size)
                                                              (params :env-size)))))
(def noctr-look-fn (partial mf/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))

(def shift-nocenter-env
  (mf/make-env (params :env-discretization)
               (params :env-size)
               (f/remove-center (params :env-size) (params :env-size)
                                (f/slide-grid (params :food-distance)
                                              slide-shift 0
                                              (params :env-size)
                                              (params :env-size)))))
(def shift-noctr-look-fn (partial mf/perc-foodspots-exactly-toroidal
                                  shift-nocenter-env (params :perc-radius)))


(comment
  (def five-exponents [1.001 1.5 2.0 2.5 3.0])
  (def nine-exponents [1.001 1.25 1.5 1.75 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's
  (def seven-exponents [1.001 1.5 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's only at the high end

  (def seed (r/make-seed))

  ;; NONDESTRUCTIVE/ASSYMETRIC:
  (def data-rng-assym
    (time (fr/levy-experiments fr/default-file-prefix centered-env assym-params
                               five-exponents 1000 seed ctrd-look-fn)))
  (def shift-data-rng-assym
    (time(fr/levy-experiments fr/default-file-prefix shift-centered-env assym-params
                              five-exponents 1000 seed shift-ctrd-look-fn)))

  ;; DESTRUCTIVE/SYMETRIC:
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-file-prefix nocenter-env params
                               five-exponents 1000 seed noctr-look-fn)))
  (def shift-data-rng-symm 
    (time (fr/levy-experiments fr/default-file-prefix shift-nocenter-env params
                               give-exponents 1000 seed shift-noctr-look-fn)))

)
