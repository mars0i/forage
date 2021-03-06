(ns forage.explore.manywalks1
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

(def maxpathlen half-size) ; for straight walk should be <= env-size/2 because will start at (env-size, env-size)
(def trunclen maxpathlen)   ; max length of any line segment
(def initial-direction 0)

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

(def levy-walk (fn [] (w/levy-foodwalk 
                      (partial mf/perc-foodspots-exactly env perc-radius)
                      look-eps maxpathlen 
                      initial-direction trunclen rng dist [half-size half-size])))

(def straight-walk (fn [init-dir]
                     (w/straight-foodwalk
                       (partial mf/perc-foodspots-exactly env perc-radius)
                       look-eps [half-size half-size] maxpathlen init-dir)))

(defn straight-walks
  "Return a sequence of n+1 straight walks using straight-walk with directions 
  evenly spaced between 0 and pi, inclusive."
  [n]
  (map (fn [t] (straight-walk (* m/pi (/ t n))))
       (range (inc n))))


;; TODO Redo this using hanami/vega-env-plot
;; (to make sure that display isn't out of sync with env).
(defn make-gridwalk-plot
  [env-size plot-dim food-distance perc-radius display-radius foodwalks+]
  (apply h/vega-gridwalk-plot
         perc-radius 
         maxpathlen
         powerlaw-scale
         []
         (h/vega-foodgrid-plot env-size plot-dim food-distance display-radius)
         (apply 
           concat
           (map 
             (fn [fw+]
               (let [[food fw stops] fw+]
                 [(h/vega-walk-plot plot-dim (h/add-walk-labels "could've" stops))
                  (h/vega-walk-plot plot-dim (h/add-walk-labels "walk" fw))]))
             foodwalks+))))


(def lws (repeatedly levy-walk))
(def sws2 (map (fn [t] (straight-walk (* (/ t 200) m/pi))) (range 201)))
(def sws1 (map (fn [t] (straight-walk (* (/ t 100) (/ m/pi 2)))) (range 100)))

(defn swses
  [times]
  (dotimes [n times]
    (let [sws (map (fn [t] (straight-walk (* (/ t 200) m/pi))) (range 201))]
      (prn (count sws))))) ; make sure the compiler thinks needs evaluating

(comment
  (println "yow")
  (require '[oz.core :as oz])
  (oz/view! (make-gridwalk-plot env-size plot-dim food-distance perc-radius display-radius sws1))
  (oz/view! (make-gridwalk-plot env-size plot-dim food-distance perc-radius display-radius sws2))
  (oz/view! (make-gridwalk-plot env-size plot-dim food-distance perc-radius display-radius [(nth lws 0)]))

  (oz/view! (make-gridwalk-plot env-size plot-dim food-distance perc-radius display-radius 
                                [(nth (filter #(first %) (take 200 lws)) 0)])) ; first Levy walk that found food

  ;; How many found food?
  (count (filter #(first %) (take 200 lws)))
  (count (filter #(first %) sws1))
)
