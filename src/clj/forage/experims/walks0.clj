(ns forage.experims.walks0
    (:require 
      ;[aerial.hanami.common :as hc]
      ;[aerial.hanami.templates :as ht]
      [clojure.math.numeric-tower :as nt]
      [forage.viz.hanami :as h]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.food :as mf]
      [utils.math :as m]
      [utils.random :as r]))


(def perc-radius 5) 
(def food-distance 50)
(def env-size 1000)
(def quadrant-size (nt/round (/ env-size 2)))
(def powerlaw-scale 1)
(def maxpathlen 2000)
(def trunclen 500)

;; For Hanami/vega-lite plots:
(def plot-dim 700)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOOD
  
(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     (/ env-size 2)
                                                     (/ env-size 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALKS

;; \mu = 3: Brownian; \mu = 2: Levy optimal; \mu near 1: "balistic":
;(def mus [1.000001 2 2.2 3])

(def seed (inc (r/make-int-seed)))
(def rng (r/make-well19937 seed))

(def dist (r/make-powerlaw rng powerlaw-scale 2))

;; Path of (direction,length) pairs
(def step-seq (w/vecs-upto-len ; generate a path with total length maxpathlen
                maxpathlen 
                (repeatedly ; generate walks with steps <= trunclen
                  (w/step-vector-fn rng dist 1 trunclen))))

;; Corresponding path of coordinates:
(def stop-seq (w/walk-stops [0 0] step-seq))

(def gridwalk-plot (h/vega-gridwalk-plot
                     (h/vega-foodgrid-plot quadrant-size plot-dim
                                           food-distance perc-radius)
                     (h/vega-walk-plot quadrant-size plot-dim 
                                       (h/add-walk-labels "walk" stop-seq))
                     perc-radius maxpathlen powerlaw-scale [(count stop-seq)]))

;; Now view gridwalk-plot e.g. with
(comment
(require '[oz.core :as oz])
(oz/start-server!)
(oz/view! gridwalk-plot)
)
