;; BUG was using wrong grid-env in second fournier env
;; SPARSE FOURNIER ENVIRONMENT WITHOUT CENTER
;; Large env, large food distance, small Fournier multiplier.
;; Both Levy and straight walks defined.
;; Two envs available:
;;   - no center cluster
;;   - center cluster without center foodspot
;;
(ns forage.experiment.fournier3
  (:require [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.env-mason :as mf]
            [utils.random :as r]
            [utils.math :as m]))

(def all-exponents [1.001 1.5 2.0 2.5])
(comment (count all-exponents) )

(def walks-per-combo 500)

(def half-size 50000) ; half the full width of the env
(def food-distance 10000)

;; FOR LEVY WALKS
(def params (sorted-map ; sort so labels match values
             :food-distance     food-distance
             :perc-radius       1  ; distance that an animal can "see" in searching for food
             :powerlaw-min      1
             :env-size          (* 2 half-size)
             :env-discretization food-distance
             :init-loc-fn  (constantly [half-size half-size])
             :maxpathlen        half-size  ; for straight walks, don't go too far
             :trunclen          half-size ; max length of any line segment
             :look-eps          0.1    ; increment within segments for food check
             :num-dirs          nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac          0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     2    ; levels in addition to the top level
             :fournier-multiplier 0.05 ; how much to shrink distance for each Fournier level
            ))
;; with food-distance = 10000, multiplier = 0.05, threer levels means
;; adding ;; points at these distances from the points that are 10000 apart:
;; 500, 25, and 1.25.  
;; The last is not OK--so don't use 3 levels with that distance and multiplier.

;; FOR STRAIGHT-WALKS
(def straight-params (assoc params :num-dirs 100))

;; Fournier env without center cluster:
(def grid-env
  (mf/make-env (params :env-discretization) (params :env-size)
               (f/centerless-rectangular-grid (params :food-distance)
                                              (params :env-size)
                                              (params :env-size))))
(def env
  (mf/make-env (params :env-discretization)
               (params :env-size)
               (f/fournierize (mf/all-foodspot-coords grid-env)
                              food-distance
                              (params :fournier-multiplier)
                              (params :fournier-levels))))

;; Fournier env with center cluster but no center foodspot:

(def grid-env-with-center
  (mf/make-env (params :env-discretization)
               (params :env-size)
               (f/rectangular-grid (params :food-distance)
                                   (params :env-size)
                                   (params :env-size))))
(def env-with-cluster
  (mf/make-env (params :env-discretization) (params :env-size)
               (f/remove-center
                 (params :env-size)
                 (params :env-size)
                 (f/fournierize (mf/all-foodspot-coords grid-env) ;; FIXME
                                food-distance
                                (params :fournier-multiplier)
                                (params :fournier-levels)))))

(comment
  (require '[forage.core.run :as fr])
  (require '[utils.random :as r])

  ;; TESTING PARAMS
  (def all-exponents [2.0])
  (def walks-per-combo 1)

  ;; REAL EXPERIMENTS
  ;; center cluster without center point:
  (time (fr/levy-experiments fr/default-file-prefix env-with-cluster (r/make-seed) params all-exponents walks-per-combo))

  ;; REAL EXPERIMENTS
  ;; no cluster in center:
  (time (fr/levy-experiments fr/default-file-prefix env (r/make-seed) params all-exponents walks-per-combo))
  
  ;; REAL EXPERIMENTS
  ;; straight:
  (time (def data (fr/straight-experiments fr/default-file-prefix env straight-params)))

  ;; display straight walk:
  (require '[forage.core.env-mason :as mf])
  (require '[utils.math :as m])
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius)))
  (time (def raw (mapv (partial fr/straight-run look-fn params)
                       (mapv (partial * (/ (* m/pi (params :max-frac)) (params :num-dirs)))
                             (range (inc (params :num-dirs)))))))

  (oz/view! (h/vega-envwalk-plot env 1100 50 raw))

)

