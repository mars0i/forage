(ns forage.experims.foodwalk3
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


(def perc-radius 5)  ; distance that an animal can "see" in searching for food
(def food-distance 50)
(def env-size 400) ; full width of env
(def half-size (/ env-size 2))
(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def maxpathlen 1000) ; max length of a path (sequence of line segments)
(def trunclen 1000)   ; max length of any line segment
(def intra-seg-epsilon 0.1) ; increment within line segments for food check

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 600)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOOD
  
(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

(def stop-walk
  [[200 200] [200 300] [300 300] [300 350] ; inside region
   [250 450] [200 450]                     ; above upper edge
   [200 350] [350 300]                     ; inside
   [450 300] [450 350] [425 375]           ; beyond right edge
   [350 450] [350 425] [300 425]           ; above upper
   [300 300] [300 200] [200 200]           ; back inside
   ])

(def toroidal-subwalks
  (t/toroidal-wrapped-partition env-size env-size stop-walk))

(def toroidal-plots 
  (map (partial h/vega-walk-plot plot-dim)
       (map (partial h/add-walk-labels "a toroidal walk")
            toroidal-subwalks)))

  
;(def walk-with-food (w/path-with-food 
;                      (partial mf/perc-foodspots-exactly env perc-radius)
;                      intra-seg-epsilon
;                      stop-walk))
;
;(def food-walk (first walk-with-food))

;; ghost walk is the full walk that would have taken place if food wasn't found
(def gridwalk-plot (apply h/vega-gridwalk-plot
                     perc-radius maxpathlen powerlaw-scale [(count food-walk)
                                                            (count stop-walk)]
                     (h/vega-foodgrid-plot env-size plot-dim   ; place food circles
                                           food-distance perc-radius)
                     (h/vega-walk-plot plot-dim   ; full path without food stop
                                       (h/add-walk-labels
                                         "walk" stop-walk))
                     toroidal-plots
                     ))

;; Now view gridwalk-plot e.g. with:
(comment
  (require '[oz.core :as oz]) ; (oz/start-server!) ; view! will call start-server! if needed
  (oz/view! gridwalk-plot)
)
