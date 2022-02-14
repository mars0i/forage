(ns forage.experims.foodwalk2
    (:require 
      ;[aerial.hanami.common :as hc]
      ;[aerial.hanami.templates :as ht]
      [clojure.math.numeric-tower :as nt]
      [forage.viz.hanami :as h]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.foodspot :as mf]
      [utils.math :as m]
      [utils.random :as r]))

;(def seed (inc (r/make-seed)))

;; FIXME
;; WITH THE FOLLOWING SEED THE PATH GOES ABOVE THE TOP OF THE PLOT.
;; WITH autosize=none (see below) THIS CAUSES THE TEXT AT THE
;; TOP TO BE CUT OFF.  AUTOSIZE ALSO SEEMS TO CAUSE THE LEGEND
;; TO BE CUT OFF.  CAN I FIX THIS BY EMBEDDING AUTOSIZE IN AN
;; INNER LAYER??  DOESN'T SEEM TO WORK.  SEE hanami.clj.
(def seed 
  1646130811754)

(println "SEED:" seed)


(def perc-radius 4)  ; distance that an animal can "see" in searching for food
(def food-distance 100)
(def env-size 500)
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
(def stop-walk (w/walk-stops [0 0] step-walk))

;(println "Made stop-walk; starting food-walk construction")

(def walk-with-food (w/path-with-food 
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      intra-seg-epsilon
                      stop-walk))

(def food-walk (first walk-with-food))

;(println "Made food-walk")

;; ghost walk is the full walk that would have taken place if food wasn't found
(def gridwalk-plot ;(->
                     (h/vega-gridwalk-plot ; overall plot config
                       perc-radius maxpathlen powerlaw-scale [(count food-walk)
                                                              (count stop-walk)]
                       (h/vega-foodgrid-plot env-size plot-dim   ; place food circles
                                             food-distance perc-radius)
                       ;(h/vega-walk-plot env-size plot-dim   ; full path without food stop
                       ;                  (h/add-walk-labels
                       ;                    "a ghost walk" stop-walk))
                       (h/vega-walk-plot env-size plot-dim  ; food search path
                                         (h/add-walk-labels
                                           "μ=2 walk" food-walk)))
                     ;; See https://vega.github.io/vega-lite/docs/size.html#autosize
                     ;(assoc "autosize" {"type" "none",
                     ;                   "contains" "content"})
                          )

;; Now view gridwalk-plot e.g. with:
(comment
 ; (oz/start-server!)  not needed after require: view! calls it if needed
  (require '[oz.core :as oz])
  (oz/view! gridwalk-plot)
)
