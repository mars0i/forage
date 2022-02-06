(ns forage.experims.xyshiftsbench
    (:require
      ;[clojure.math.numeric-tower :as nt]
      [forage.walks :as w]
      [forage.food :as f]
      [forage.mason.food :as mf]
      [utils.math :as m]
      [utils.random :as r]
      [criterium.core :as criterium]))

(def intra-seg-epsilons [1 0.1 0.01 0.001])
(def perc-radius 5)
(def powerlaw-exponent 2) ; must be < 1; 2 supposed to be optimal sparse targets
(def powerlaw-scale 1) ; scale parameter of distribution
(def food-distance 100)
(def env-size 400)
(def maxpathlen 2000) ; max length of a path (sequence of line segments)
(def trunclen 1000)   ; max length of any line segment

(def env (mf/make-env food-distance env-size
                      (f/centerless-rectangular-grid food-distance
                                                     env-size
                                                     env-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALKS

(def seed (inc (r/make-seed)))
;(def seed 41221)
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
    "seed=" seed, "perc-radius=" perc-radius,
    "powerlaw-exponent=" powerlaw-exponent, "powerlaw-scale=" powerlaw-scale,
    "food-distance=" food-distance, "env-size=" env-size,
    "maxpathlen=" maxpathlen, "trunclen=" trunclen)
  (let [wwf (map 
              (fn [eps]
                  (println "intra-seg-epsilon =" eps)
                  (flush)
                  (criterium/bench
                    (w/path-with-food 
                      (partial mf/perceptible-foodspots env perc-radius)
                      eps
                      stop-walk))) 
              epsilons)]
    (println "Did they all find the same foodspots?" (apply = wwf))
    wwf))

;(def food-walk (first walk-with-food))
