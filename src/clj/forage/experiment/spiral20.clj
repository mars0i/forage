;; spiral20.clj
;; Copied from grid19slide.clj and heavily modified.
(ns forage.experiment.spiral20
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.env-mason :as em]
            [utils.random :as r]))

(def default-dirname "../../data.foraging/forage/")

;; FOOD-DISTANCE SHOULD DIVIDE HALF-SIZE EVENLY, OR THERE WON't BE FOOD AT CENTER,
;; WHICH IS WHERE THE SEARCH STARTS.
(def half-size 500) ; half the full width of the env
(def maxpathlen (* 20 half-size)) ; max total length of search path
(def food-distance nil) ; won't be used

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
             :basename            (str default-dirname "spiral20_")
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SINGLE-FOODSPOT ENV

(defn make-single-target-env
  "Make an env with a single foodspot between the center of the core env
  and the right edge along the equator."
  [denom nomin]
  (let [half-size (/ (params :env-size) 2)] ; probably already defined as var, but should get from params
    (em/make-env (params :env-discretization)
                 (params :env-size)
                 [[(long (+ half-size (* (/ nomin denom) half-size))) ; coerce to long: avoid probs later with Ratio, BigInt
                   half-size]])))
                 
(def envs (mapv (partial make-single-target-env 10)
                (range 1 10)))

(defn make-toroidal-look-fn
  [env]
  (partial em/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(defn make-unbounded-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (partial em/perc-foodspots-exactly env (params :perc-radius)))


(comment
  ;; Let's see what these envs look like:
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)
  ;; Piecemeal:
  (def v-env (map h/make-foodspot (em/env-foodspot-coords (envs 0))))
  (def food-plot (h/vega-food-plot v-env 1000 400 20))
  (oz/view! food-plot)
  ;; All at once, animated:
  (dotimes [i 9]
    (oz/view! (h/vega-food-plot (map h/make-foodspot (em/env-foodspot-coords (envs i)))
                                1000 400 20))
    (Thread/sleep 1000))




  (def five-exponents [1.001 1.5 2.0 2.5 3.0])
  (def nine-exponents [1.001 1.25 1.5 1.75 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's
  (def seven-exponents [1.001 1.5 2.0 2.25 2.5 2.75 3.0]) ; w/ additional mu's only at the high end

  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  ;; OBSOLETE--REPLACE THE STUFF BELOW:
  ;; NONDESTRUCTIVE/ASSYMETRIC:
  (def data-rng-assym
    (time (fr/levy-experiments fr/default-dirname centered-env assym-params
                               nine-exponents 5000 seed ctrd-look-fn)))
  (def shift-data-rng-assym
    (time (fr/levy-experiments fr/default-dirname shift-centered-env assym-params
                               nine-exponents 5000 seed shift-ctrd-look-fn)))

  ;; DESTRUCTIVE/SYMETRIC:
  (def data-rng-symm
    (time (fr/levy-experiments fr/default-dirname env params
                               nine-exponents 5000 seed look-fn)))
  (def shift-data-rng-symm 
    (time (fr/levy-experiments fr/default-dirname shift-env params
                               five-exponents 100 seed shift-look-fn)))

)
