(ns forage.experiment.examplelevywalks
  (:require [forage.walks :as w]
            [utils.random :as r]
            [forage.viz.hanami :as h]
            [oz.core :as oz]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI SPACE PARAMETERS:

(def env-size 5000)
(def half-size (/ env-size 2))
(def plot-dim 700)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA PARAMETERS:

;; $\mu \approx 1$ is is a "ballistic" Levy walk.
;; $\mu = 2$ is the theoretical optimum for searches.
;; $\mu = 3$ is a Brownian walk.
;(def mus [1.000001 2 2.2 3])  ; mu near 1: decimal digits have big effect on speed
(def mus [1.01 2 3])  ; mu near 1: decimal digits have big effect on speed
;(def mus [2])
;(def scales [1])
;(def scales [1 8])
;(def scales [1 10 100])
;(def scales [1 100 1000])
;(def scale-mus (for [scale scales, mu mus] [scale mu]))

(def maxpathlen 5000)
(def trunclen 5000)
(def perceptual-radius 20) 
(def food-distance 200)

(def powerlaw-scale 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNGS and DISTS:

(def seed (inc (r/make-seed)))
(def rng (r/make-well19937 seed))

(def dists (map (fn [mu] (r/make-powerlaw rng 1 mu)) mus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RANDOM WALK DATA GENERATION:

;; Add this to the embedded fn below to constrain all seqs to start in the
;; same direction: (w/subst-init-dir init-dir ...)

;; Infinite seqs of direction, length step vectors.  Used by two defs below.
(def inf-step-seqs (map
                     (fn [dist]
                         (repeatedly
                           (w/step-vector-fn rng dist 1 trunclen))) dists))

(def step-seqs (map (partial w/vecs-upto-len maxpathlen) inf-step-seqs))


;; Infinite sequences of steps that result from the step vectors
(def stop-seqs (map (fn [step-seq]
                      (w/walk-stops [half-size half-size] step-seq))
                    step-seqs))

;; Infinite sequences of stops labeled for Hanami/Vega-Lite:
(def vl-stop-seqs (map (fn [mu stop-seq]
                           (h/add-walk-labels (str "μ=" mu) stop-seq))
                       mus stop-seqs))

;; Number of steps in each path:
(def n-steps (map count step-seqs))

;; Construct walk data for Hanami/Vega-Lite:
(def walks (doall (apply concat (map take n-steps vl-stop-seqs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI/VEGA-LITE

(def gridwalk-plot (h/vega-gridwalk-plot
                     perceptual-radius maxpathlen powerlaw-scale n-steps
                     (h/vega-linegrid-plot env-size plot-dim food-distance) ; kludge to force uniform scaling and dims
                     (h/vega-walk-plot plot-dim walks)))


;; Now view gridwalk as vega-lite, e.g. with
(comment
  ;(require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! gridwalk-plot)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OLD CODE

(comment
(def half-size 1000)
(def food-distance 100)
(def params {:food-distance       food-distance
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  food-distance
             :init-loc            [half-size half-size] ; i.e. center of env
             :maxpathlen          half-size  ; for straight walks, don't go too far
             :trunclen            half-size ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     nil
             :fournier-multiplier nil
           })

(def rng (r/make-well19937))

(def fw1 (fr/levy-run rng fr/ignore-food nil params 1.01))
(def fw2 (fr/levy-run rng fr/ignore-food nil params 2))
(def fw3 (fr/levy-run rng fr/ignore-food nil params 3))

(def mu1vegawalk (h/add-walk-labels "mu=1.001" (second fw1)))
;(def mu1vegawalk (h/add-walk-labels "μ=1.001" (second fw1)))

(def mu2vegawalk (h/add-walk-labels "mu=2" (second fw2)))
;(def mu2vegawalk (h/add-walk-labels "μ=2" (second fw2)))

(take 10 mu2vegawalk)

(def mu3vegawalk (h/add-walk-labels "mu=3" (second fw3)))
;(def mu3vegawalk (h/add-walk-labels "μ=3" (second fw3)))

(def mu1plot (h/vega-walk-plot 800 mu1vegawalk))
(def mu2plot (h/vega-walk-plot 800 mu2vegawalk))
(def mu3plot (h/vega-walk-plot 800 mu3vegawalk))

;; If you are making plots for a paper:
(def mu1plot (-> mu1plot (assoc :background "white")))
(def mu2plot (-> mu2plot (assoc :background "white")))
(def mu3plot (-> mu3plot (assoc :background "white")))


(comment
  (oz/start-server!)
  (oz/view! mu3plot)
  (oz/view! mu2plot)
  (oz/view! mu1plot)
)
)
