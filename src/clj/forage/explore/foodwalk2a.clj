(ns forage.explore.foodwalk2a
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

;; THIS MODEL Illustrates that with straight paths, and a sparse rectangular
;; grid of foodspots, and small perceptual radius, only a few angles find food.
;; You can tell when a foodwalk finds food because you see the blue "ghost
;; walk" that shows where it would have gone if it didn't find food.
;; (Note that the added Levy path is *much* longer than the straight paths,
;; and has a lot of trouble finding food.  I ran this many times and it
;; mostly did not find food.  The saved seed is for one case in which it
;; finally did.)

;; NOTE display-radius is much larger than actual perc-radius, so paths
;; appear to see foodspots, but they don't.  (But if food-distance is set to
;; 100, many paths succeed.)
(def perc-radius 1)  ; distance that an animal can "see" in searching for food
(def display-radius 100) ; if want foodspots to be displayed larger
(def food-distance 1000)
(def env-size 20000) ; full width of env
(def half-size (/ env-size 2))
(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def maxpathlen 50000) ; max length of a path (sequence of line segments)
(def trunclen 50000)   ; max length of any line segment
(def intra-seg-epsilon 0.1) ; increment within line segments for food check

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 700)

;(def seed (inc (r/make-seed)))
(def seed 1645858441039) ; THIS SEED ALLOWS LEVY PATH TO FIND FOOD
(println "SEED:" seed)
(def rng (r/make-well19937 seed))
(def dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOOD
  
(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

;;;;;;;;;;;;;;;;;;;;;;;
;; STRAIGHT PATHS

;; FIXME Can't go to the vertical, i.e. pi/2, until fix forage.walks to handle it
(def step-walks (map (fn [t] [[(* (/ t 80) m/pi) half-size]]) (range 40)))

;; Corresponding path of coordinate pairs:
(def stop-walks (map (fn [step-walk] 
                         (w/walk-stops [half-size half-size] step-walk))
                     step-walks))

(def walks-with-food 
  (map (fn [stop-walk]
           (w/path-with-food 
             (partial mf/perc-foodspots-exactly env perc-radius)
             intra-seg-epsilon
             stop-walk))
       stop-walks))

(def food-walks (map first walks-with-food))

(def double-plots
  (apply concat
         (map (fn [stop-walk food-walk]
                  [(h/vega-walk-plot plot-dim   ; full path without food stop
                                     (h/add-walk-labels
                                       "a ghost walk" stop-walk))
                   (h/vega-walk-plot plot-dim  ; food search path
                                     (h/add-walk-labels
                                       "food walk" food-walk))])
              stop-walks food-walks)))

;;;;;;;;;;;;;;;;;;;;;;;
;; A RANDOM lEVY PATH

(def step-walk (w/vecs-upto-len ; generate a path
                 maxpathlen    ;  with total length maxpathlen
                 (repeatedly ; generate steps with lengths <= trunclen
                    (w/step-vector-fn rng dist 1 trunclen))))
(def stop-walk (w/walk-stops [half-size half-size] step-walk))
(def walk-with-food (w/path-with-food 
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      intra-seg-epsilon
                      stop-walk))
(def food-walk (first walk-with-food))



;; ghost walk is the full walk that would have taken place if food wasn't found
(def gridwalk-plot (apply
                     h/vega-gridwalk-plot ; overall plot config
                     perc-radius maxpathlen powerlaw-scale [(count food-walk)
                                                            (count stop-walk)]
                     (h/vega-foodgrid-plot env-size plot-dim   ; place food circles
                                           food-distance display-radius)
                     ;; random Levy path:
                     (h/vega-walk-plot plot-dim   ; full path without food stop
                                       (h/add-walk-labels
                                         "a ghost walk" stop-walk))
                     (h/vega-walk-plot plot-dim  ; food search path
                                       (h/add-walk-labels
                                         "food walk" food-walk))
                     ;; straight paths:
                     double-plots))

;; Now view gridwalk-plot e.g. with:
(comment
  (require '[oz.core :as oz]) ; (oz/start-server!) ; view! will call start-server! if needed
  (oz/view! gridwalk-plot)
)
