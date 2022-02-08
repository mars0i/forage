(ns forage.experims.xyshiftsbench
    (:require
      ;[clojure.math.numeric-tower :as nt]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.food :as mf]
      [utils.math :as m]
      [utils.random :as r]
      [forage.viz.hanami :as h] ; if I want to display the paths
      [criterium.core :as criterium]))

(def perc-radius 1)
(def powerlaw-exponent 2) ; must be < 1; 2 supposed to be optimal sparse targets
(def powerlaw-scale 1) ; scale parameter of distribution
(def food-distance 100)
(def env-size 400)
(def maxpathlen 2000) ; max length of a path (sequence of line segments)
(def trunclen 1000)   ; max length of any line segment

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 700)


(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALKS

(def seed (inc (r/make-seed)))
(println "SEED:" seed)
(def rng (r/make-well19937 seed))
(def dist (r/make-powerlaw rng powerlaw-scale powerlaw-exponent))

;; Path consisting of (direction,length) pairs
(def step-walk (w/vecs-upto-len ; generate a path
                 maxpathlen    ;  with total length maxpathlen
                 (repeatedly ; generate steps with lengths <= trunclen
                    (w/step-vector-fn rng dist 1 trunclen))))

;; Corresponding path of coordinate pairs:
(def stop-walk (w/walk-stops [0 0] step-walk))

(defn run-bench
  "Runs Criterium 'bench' on food searches with all intra-seg epsilons
  passed, using a random walk and the environment defined with parameters 
  defined in this version of this file.  Reports parameters and benchmark 
  results to stdout, followed by a report of whether the same foodspots 
  were found with each epsilon.  Then returns the resulting walk-with-food
  pairs so that they can be examined."
  [epsilons]
  (println 
    "seed:" seed ", perc-radius:" perc-radius,
    ", powerlaw-exponent:" powerlaw-exponent, ", powerlaw-scale:" powerlaw-scale,
    "\nfood-distance:" food-distance, ", env-size:" env-size,
    ", maxpathlen:" maxpathlen, ", trunclen:" trunclen)
  (let [wwfs (map 
              (fn [eps]
                  (println "\n---------------------------"
                           "\nintra-seg-epsilon =" eps)
                  (flush)
                  (criterium/bench
                    (w/path-with-food 
                      (partial mf/perceptible-foodspots env perc-radius)
                      eps
                      stop-walk))) 
              epsilons)]
    (println "\nDid they all find the same foodspots?" (apply = wwfs))
    wwfs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME not working
;; To view the paths:
(defn make-gridwalk-plots
  [walks]
  (let [walk-num$ (atom 0)]
    (apply h/vega-gridwalk-plot
           perc-radius maxpathlen powerlaw-scale [(map count walks)]
           (h/vega-foodgrid-plot env-size plot-dim
                                 food-distance perc-radius)
           (map (fn [walk]
                    (h/vega-walk-plot env-size plot-dim 
                                      (h/add-walk-labels
                                        (str "food walk " @walk-num$)
                                        walk)))
                walks))))


(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! (make-gridwalk-plots (map first wwfs)))
)
