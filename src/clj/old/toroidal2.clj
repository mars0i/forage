;; Food walk experiment for testing toroidal paths in Vega-Lite
;; using a random Levy walk
(ns forage.explore.toroidal2
    (:require 
      ;[aerial.hanami.common :as hc]
      ;[aerial.hanami.templates :as ht]
      [clojure.math.numeric-tower :as nt]
      [forage.viz.hanami :as h]
      [forage.viz.toroidal :as t]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.foodspot :as mf]
      [utils.math :as m]
      [utils.random :as r]))

;(def seed (inc (r/make-seed)))
;(def seed 1646130811754)
(def seed 1646924435253)

(println "SEED:" seed)


(def perc-radius 5)  ; distance that an animal can "see" in searching for food
(def food-distance 100)
(def env-size 1000) ; full width of env
(def half-size (/ env-size 2))
(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def maxpathlen 5000) ; max length of a path (sequence of line segments)
(def trunclen 5000)   ; max length of any line segment
(def intra-seg-epsilon 0.1) ; increment within line segments for food check

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 700)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOOD
  
(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALKS

(def rng (r/make-well19937 seed))

;; mu=3: Brownian; mu=2: Levy optimal; mu near 1: "ballistic":
(def dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))

;; Path consisting of (direction,length) pairs
(def step-walk (w/vecs-upto-len ; generate a path
                 maxpathlen    ;  with total length maxpathlen
                 (repeatedly ; generate steps with lengths <= trunclen
                    (w/step-vector-fn rng dist 1 trunclen))))

;; Corresponding path of coordinate pairs:
(def stop-walk (w/walk-stops [half-size half-size] step-walk))

;(println "Made stop-walk; starting food-walk construction")

(def walk-with-food (w/path-with-food 
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      intra-seg-epsilon
                      stop-walk))

(def food-walk (first walk-with-food))

(def walk-segments
  (map (partial h/add-walk-labels "toroidal")
       (t/toroidal-wrapped-partition env-size env-size food-walk)))

(def walk-plots 
  (map (partial h/vega-walk-plot plot-dim)
       walk-segments))

(def gridwalk-plot 
                (
                 apply
                  h/vega-gridwalk-plot ; overall plot config
                  perc-radius
                  maxpathlen
                  powerlaw-scale 
                  [(count food-walk) (count stop-walk)]
                  (h/vega-foodgrid-plot env-size plot-dim
                                        food-distance perc-radius)
;; this shows toroidal functions not working:
                  (h/vega-walk-plot plot-dim  ; food search path
                                    (h/add-walk-labels
                                      "full walk" food-walk))
                  walk-plots
                  ))

                       ;(h/vega-walk-plot env-size plot-dim   ; full path without food stop
                       ;                  (h/add-walk-labels
                       ;                    "a ghost walk" stop-walk))

;; Now view gridwalk-plot e.g. with:
(comment
 ; (oz/start-server!)  not needed after require: view! calls it if needed
  (require '[oz.core :as oz])
  (oz/view! gridwalk-plot)
)
