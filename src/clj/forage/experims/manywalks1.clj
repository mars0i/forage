(ns forage.experims.manywalks1
    (:require 
      [forage.viz.hanami :as h]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.foodspot :as mf]
      [utils.math :as m]
      [utils.random :as r]))

(def seed (inc (r/make-seed)))
;(def seed 1645758210410)
(println "SEED:" seed)
(def rng (r/make-well19937 seed))

;; NOTE display-radius is much larger than actual perc-radius, so paths
;; appear to see foodspots, but they don't.  (But if food-distance is set to
;; 100, many paths succeed.)
(def perc-radius 1)  ; distance that an animal can "see" in searching for food
(def food-distance 200)
(def env-size 20000) ; full width of env
(def half-size (/ env-size 2))

(def maxpathlen half-size) ; max length of a path (sequence of line segments)
(def trunclen half-size)   ; max length of any line segment
(def default-init-dir 0)

(def powerlaw-scale 1) ; scale parameter of distribution
(def powerlaw-exponent 2) ; must be > 1; 2 supposed to be optimal sparse targets
(def look-eps 0.1) ; increment within line segments for food check

(def display-radius 50) ; if want foodspots to be displayed larger
(def plot-dim 700) ; For Hanami/vega-lite plots, size of plot display:

(def dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))

(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

(def levy-fn (fn [] (w/levy-foodwalk 
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      look-eps [half-size half-size] maxpathlen 
                      trunclen default-init-dir rng dist)))

(def straight-fn (fn [init-dir]
                     (w/straight-foodwalk
                       (partial mf/perc-foodspots-exactly env perc-radius)
                       look-eps [half-size half-size] maxpathlen init-dir)))

(def lws (repeatedly levy-fn))
(def sws (map (fn [t] (straight-fn (* (/ t 200) m/pi))) (range 201)))


(defn make-gridwalk-plot
  [env-size plot-dim food-distance display-radius foodwalks+]
  (apply h/vega-gridwalk-plot
         perc-radius 
         maxpathlen
         powerlaw-scale
         []
         (h/vega-foodgrid-plot env-size plot-dim food-distance perc-radius)
         (apply 
           concat
           (map 
             (fn [fw+]
                 (let [[fw food stops inf-steps] fw+]
                   [(h/vega-walk-plot plot-dim (h/add-walk-labels "could've" stops))
                    (h/vega-walk-plot plot-dim (h/add-walk-labels "walk" fw))]))
             foodwalks+))))
