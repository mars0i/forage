(ns forage.experims.foodwalk1
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


(def perc-radius 5)  ; distance that an animal can "see" in searching for food
(def food-distance 40)
(def env-size 200)
(def quadrant-size (/ env-size 2))
(def powerlaw-scale 1) ; scale parameter of distribution
(def maxpathlen 1000) ; max length of a path (sequence of line segments)
(def trunclen 100)   ; max length of any line segment
(def intra-seg-epsilon 0.01) ; increment within line segments for food check

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 700)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOOD
  
(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     (/ env-size 2)
                                                     (/ env-size 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALKS

;(def seed (inc (r/make-int-seed)))
(def seed 41221)
(println "SEED:" seed)
(def rng (r/make-well19937 seed))

;; mu=3: Brownian; mu=2: Levy optimal; mu near 1: "ballistic":
(def dist (r/make-powerlaw rng powerlaw-scale 2))

;; Path consisting of (direction,length) pairs
(def step-walk (w/vecs-upto-len ; generate a path
                 maxpathlen    ;  with total length maxpathlen
                 (repeatedly ; generate steps with lengths <= trunclen
                    (w/step-vector-fn rng dist 1 trunclen))))

;; Corresponding path of coordinate pairs:
(def stop-walk (w/walk-stops [0 0] step-walk))

(println "Made stop-walk; starting food-walk construction")

(def walk-with-food (w/path-with-food 
                      (partial mf/perceptible-foodspots env perc-radius)
                      intra-seg-epsilon
                      stop-walk))

(def food-walk (first walk-with-food))

(println "Made food-walk")

;; ghost walk is the full walk that would have taken place if food wasn't found
(def gridwalk-plot (h/vega-gridwalk-plot
                     perc-radius maxpathlen powerlaw-scale [(count food-walk)
                                                            (count stop-walk)]
                     (h/vega-foodgrid-plot env-size plot-dim
                                           food-distance perc-radius)
                     (h/vega-walk-plot env-size plot-dim 
                                       (h/add-walk-labels
                                         "a ghost walk" stop-walk))
                     (h/vega-walk-plot env-size plot-dim 
                                       (h/add-walk-labels
                                         "food walk" food-walk))))

;; Now view gridwalk-plot e.g. with
(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! gridwalk-plot)
)
