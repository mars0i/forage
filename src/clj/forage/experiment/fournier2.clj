;; STRAIGHT RUNS - FOURNIER ENVIRONMENT WITHOUT CENTER
(ns forage.experiment.fournier2
  (:require [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.env-mason :as mf]
            [utils.random :as r]
            [utils.math :as m]))

(def half-size 20000) ; half the full width of the env
(def food-distance 5000)
(def params (sorted-map ; sort so labels match values
             :food-distance     food-distance
             :perc-radius       1  ; distance that an animal can "see" in searching for food
             :powerlaw-min      1
             :env-size          (* 2 half-size)
             :env-discretization food-distance
             :init-loc-fn  (constantly [half-size half-size])
             ;; Note long paths; ballistic searches will go outside the garden:
             :maxpathlen        (* 4 half-size)  ; for straight walks, don't go too far
             :trunclen          (* 4 half-size) ; max length of any line segment
             :look-eps          0.1  ; increment within segments for food check
             :num-dirs          200 ; split range this many times + 1 (includes range max); nil for random
             :max-frac          0.5 ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels 4
             :fournier-multiplier 0.2 ; how much to shrink distance for each Fournier level
            ))

(def grid-env (mf/make-env (params :env-discretization)
                                       (params :env-size)
                                       (f/centerless-rectangular-grid (params :food-distance)
                                                           (params :env-size)
                                                           (params :env-size))))

;; Fournier env without center:
(def env
  (mf/make-env (params :env-discretization)
               (params :env-size)
               (f/fournierize (mf/all-foodspot-coords grid-env)
                              food-distance
                              (params :fournier-multiplier)
                              (params :fournier-levels))))

(comment
  (require '[forage.core.run :as fr])

  (time (def summary (fr/straight-experiments fr/default-file-prefix env params)))



  (require '[forage.core.env-mason :as mf])
  (require '[utils.math :as m])
  (def look-fn (partial mf/perc-foodspots-exactly env (params :perc-radius)))
  (time (def raw (mapv (partial fr/straight-run look-fn params)
                       (mapv (partial * (/ (* m/pi (params :max-frac)) (params :num-dirs)))
                             (range (inc (params :num-dirs)))))))

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (oz/view! (h/vega-envwalk-plot env 1100 20 raw))
)

