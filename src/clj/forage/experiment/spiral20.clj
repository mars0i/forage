;; spiral20.clj
;; Copied from grid19slide.clj and heavily modified.
(ns forage.experiment.spiral20
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.env-mason :as em]
            [utils.random :as r]))


;; FOOD-DISTANCE SHOULD DIVIDE HALF-SIZE EVENLY, OR THERE WON't BE FOOD AT CENTER,
;; WHICH IS WHERE THE SEARCH STARTS.
(def half-size 500) ; half the full width of the env
(def maxpathlen (* 20 half-size)) ; max total length of search path
(def fooddistance nil) ; won't be used

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance 
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size]) ; TODO MODIFY THIS?
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            half-size
             :look-eps            0.2    ; TODO WILL THIS WORK WITH SHORTER SPIRAL SEGMENTS?
             ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV FOR DESTRUCTIVE/SYMMETRIC SEARCH
(def env (em/make-env (params :env-discretization)
                      (params :env-size)
                      (f/remove-center (params :env-size) (params :env-size)
                                       (f/slide-grid (params :food-distance)
                                                     0 0 ; slide shifts
                                                     (params :env-size)
                                                     (params :env-size)))))

(def look-fn (partial em/perc-foodspots-exactly-toroidal env (params :perc-radius)))


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
    (time (fr/levy-experiments fr/default-file-prefix env params
                               nine-exponents 5000 seed look-fn)))
  (def shift-data-rng-symm 
    (time (fr/levy-experiments fr/default-file-prefix shift-env params
                               five-exponents 100 seed shift-look-fn)))

)
