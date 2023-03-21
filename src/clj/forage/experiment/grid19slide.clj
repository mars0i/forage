;; grid19slide.clj
;; (grid18slide.clj was broken.)
;; Like grid17slide.clj but with much shorter maxpathlen.
;; And then I increased food-distance.
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
(ns forage.experiment.grid19slide
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.env-mason :as em]
            [utils.random :as r]))


;; FOOD-DISTANCE SHOULD DIVIDE HALF-SIZE EVENLY, OR THERE WON't BE FOOD AT CENTER,
;; WHICH IS WHERE THE SEARCH STARTS.
(def half-size 4500) ; half the full width of the env
(def food-distance 1500)
(def slide-shift90 1300) ; 0.90 * 1500
(def slide-shift95 1425) ; 0.95 * 1500
(def slide-shift99 1485) ; 0.99 * 1500
(def slide-shift997 1495) ; five units away from unshifted pint

(def maxpathlen (* 5 half-size)) ; max total length of search path

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance 
             :slide-shift         slide-shift95`
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

(def centered-env (em/make-env (params :env-discretization)
                               (params :env-size)
                               (f/slide-grid (params :food-distance)
                                             0 0 ; slide shifts [could use rectangular-grid]
                                             (params :env-size) (params :env-size))))
(def ctrd-look-fn (partial em/perc-foodspots-exactly-toroidal centered-env (params :perc-radius)))

(def shift-centered-env (em/make-env (params :env-discretization)
                                       (params :env-size)
                                       (f/slide-grid (params :food-distance) 
                                                     (params :slide-shift) 0
                                                     (params :env-size) (params :env-size))))
(def shift-ctrd-look-fn (partial em/perc-foodspots-exactly-toroidal
                                 shift-centered-env (params :perc-radius)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV FOR DESTRUCTIVE/SYMMETRIC SEARCH
(def nocenter-env (em/make-env (params :env-discretization)
                               (params :env-size)
                               (f/remove-center (params :env-size) (params :env-size)
                                                (f/slide-grid (params :food-distance)
                                                              0 0 ; slide shifts
                                                              (params :env-size)
                                                              (params :env-size)))))
(def noctr-look-fn (partial em/perc-foodspots-exactly-toroidal nocenter-env (params :perc-radius)))

(def shift-nocenter-env
  (em/make-env (params :env-discretization)
               (params :env-size)
               (f/remove-center (params :env-size) (params :env-size)
                                (f/slide-grid (params :food-distance)
                                              (params :slide-shift) 0
                                              (params :env-size)
                                              (params :env-size)))))
(def shift-noctr-look-fn (partial em/perc-foodspots-exactly-toroidal
                                  shift-nocenter-env (params :perc-radius)))

(comment
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def grid1 (f/slide-grid 10 0 0 0 0 100 100))
  (def vgrid1 (map h/make-foodspot grid1))
  (def food1 (h/vega-food-plot vgrid1 100 400 1))
  (oz/view! food1)
  
  (def grid2 (f/slide-grid 10 8 8 0 0 100 100))
  (def vgrid2 (map h/make-foodspot grid2))
  (def food2 (h/vega-food-plot vgrid2 100 400 1))
  (oz/view! food2)
  ;; With different colors:
  (def vgrids2 (h/split-foodgrid grid2))
  (def foods2 (h/vega-food-plot vgrids2 100 400 1))
  (oz/view! foods2)
)

(comment
  (def five-exponents [1.001 1.5 2.0 2.5 3.0])
  (def nine-exponents [1.001 1.25 1.5 1.75 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's
  (def seven-exponents [1.001 1.5 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's only at the high end

  (def seed (r/make-seed))

  ;; NONDESTRUCTIVE/ASSYMETRIC:
  (def data-rng-assym
    (time (fr/levy-experiments fr/default-file-prefix centered-env assym-params
                               nine-exponents 5000 seed ctrd-look-fn)))
  (def shift-data-rng-assym
    (time (fr/levy-experiments fr/default-file-prefix shift-centered-env assym-params
                               nine-exponents 5000 seed shift-ctrd-look-fn)))

  ;; DESTRUCTIVE/SYMETRIC:
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-file-prefix nocenter-env params
                               nine-exponents 5000 seed noctr-look-fn)))
  (def shift-data-rng-symm 
    (time (fr/levy-experiments fr/default-file-prefix shift-nocenter-env params
                               five-exponents 100 seed shift-noctr-look-fn)))

)
