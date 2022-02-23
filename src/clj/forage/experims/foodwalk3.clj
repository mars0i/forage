(ns forage.experims.foodwalk3
    (:require 
      [forage.viz.hanami :as h]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.foodspot :as mf]
      [utils.math :as m]
      [utils.random :as r]))

(def seed (inc (r/make-seed)))
(println "SEED:" seed)
(def rng (r/make-well19937 seed))

;; NOTE display-radius is much larger than actual perc-radius, so paths
;; appear to see foodspots, but they don't.  (But if food-distance is set to
;; 100, many paths succeed.)
(def perc-radius 1)  ; distance that an animal can "see" in searching for food
(def display-radius 50) ; if want foodspots to be displayed larger
(def food-distance 200)
(def env-size 20000) ; full width of env
(def half-size (/ env-size 2))
(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def maxpathlen half-size) ; max length of a path (sequence of line segments)
(def trunclen half-size)   ; max length of any line segment
(def look-eps 0.1) ; increment within line segments for food check

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 700)

(def dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))

(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

(def lfw+
  (w/levy-foodwalk 
    (partial mf/perc-foodspots-exactly env perc-radius)
    look-eps [half-size half-size] 0 maxpathlen trunclen rng dist))

(def sfw+
  (w/straight-foodwalk
    (partial mf/perc-foodspots-exactly env perc-radius)
    look-eps [half-size half-size] 0.25 maxpathlen))

(defn make-gridwalk-plot
  [foodwalk+]
  (let [[fw food stops inf-steps] foodwalk+]
    (h/vega-gridwalk-plot perc-radius 
                          maxpathlen
                          powerlaw-scale
                          [(count fw) (count stops)]
                          (h/vega-foodgrid-plot env-size plot-dim   ; place food circles
                                                food-distance perc-radius)
                          (h/vega-walk-plot plot-dim   ; full path without food stop
                                            (h/add-walk-labels
                                              "a ghost walk" stops))
                          (h/vega-walk-plot plot-dim  ; food search path
                                            (h/add-walk-labels
                                              "food walk" fw)))))


(def gridwalk-plot (make-gridwalk-plot lfw+))

