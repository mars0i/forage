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

(def powerlaw-exponent 2) ; must be < 1; 2 supposed to be optimal sparse targets
(def powerlaw-scale 1) ; scale parameter of distribution
(def env-size 1000)
(def maxpathlen 2000) ; max length of a path (sequence of line segments)
(def trunclen 1000)   ; max length of any line segment

(def perc-radius 1)
(def food-distance 100)
;(def discretization food-distance)
(def discretization 20)

;; For Hanami/vega-lite plots, size of plot display:
(def plot-dim 700)

(def grid-coords (f/centerless-rectangular-grid food-distance
                                                env-size
                                                env-size))

(def env (mf/make-env discretization env-size grid-coords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALKS

(def seed (inc (r/make-seed)))
;(def seed 1645681372124)
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

(defn bench-foodwalks
  "Runs Criterium 'bench' on food searches with all combinations of parameters
  passed, using a random walk and the environment defined with parameters 
  defined in this version of this file.  All arguments should be
  sequendces.  Reports parameters and benchmark 
  results to stdout, followed by a report of whether the same foodspots 
  were found with each epsilon.  Then returns the resulting collections of 
  walk-with-food pairs so that they can be examined.  Note that since
  Criterium runs the process many times, what you get back will be 
  collections of the same results many times.  Try something like this
  on the return value: (map (comp foodspot-coords first second) wwfs)."
  [epsilons discretizations look-fns]
  (println 
    "seed:" seed ", perc-radius:" perc-radius,
    ", powerlaw-exponent:" powerlaw-exponent, ", powerlaw-scale:" powerlaw-scale,
    "\nfood-distance:" food-distance, ", env-size:" env-size,
    ", maxpathlen:" maxpathlen, ", trunclen:" trunclen)
  (let [wwfs$ (atom [])] ; bench doesn't return result of computation, so we need to add results to an atom inside it
    (doseq [epsilon epsilons
            discretization discretizations
            look-fn look-fns]
           (println "\n---------------------------"
                    "\nintra-seg-epsilon =" epsilon,
                    "discretization:" discretization, "\n" look-fn)
           (flush)
           (let [firstrun$ (atom true)]
             (criterium/bench
               (let [wwfs (w/path-with-food 
                            look-fn
                            epsilon
                            stop-walk)]
                 (when @firstrun$           ; including in timing, but 
                   (swap! firstrun$ not)    ;  cost should be negligible
                   (swap! wwfs$ conj wwfs))))))
    (println "Are the same foodspots found?" (apply = @wwfs$))
    @wwfs$))

;; TODO: Revise to use internal same internal logic as preceding bench-epsilons-and-discretizations
;; OR REMOVE (DEPRECATED)
(defn bench-epsilons
  "Runs Criterium 'bench' on food searches with all intra-seg epsilons
  passed, using a random walk and the environment defined with parameters 
  defined in this version of this file.  Reports parameters and benchmark 
  results to stdout, followed by a report of whether the same foodspots 
  were found with each epsilon.  Then returns the resulting collections of 
  walk-with-food pairs so that they can be examined.  Note that since
  Criterium runs the process many times, what you get back will be 
  collections of the same results many times.  Try something like this
  on the return value: (map (comp foodspot-coords first second) wwfs)."
  [epsilons]
  (println 
    "seed:" seed ", perc-radius:" perc-radius,
    ", powerlaw-exponent:" powerlaw-exponent, ", powerlaw-scale:" powerlaw-scale,
    "\nfood-distance:" food-distance, ", env-size:" env-size,
    ", maxpathlen:" maxpathlen, ", trunclen:" trunclen, ",\n"
    "discretization:" discretization)
  (let [wwfs$ (atom [])] ; bench doesn't return result of computation
    (run!                ; so we need to add results to an atom inside it
          (fn [eps]
              (println "\n---------------------------"
                       "\nintra-seg-epsilon =" eps)
              (flush)
              (let [firstrun$ true]
                (criterium/bench
                  (let [wwfs (w/path-with-food 
                               (partial mf/perc-foodspots-exactly
                                        env perc-radius)
                               eps
                               stop-walk)]
                    (when @firstrun$
                      (swap! firstrun$ not)
                      (swap! wwfs$ conj wwfs))))))

          epsilons)
    (println "\nDid they all find the same foodspots?" (apply = @wwfs$))
    @wwfs$))

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
