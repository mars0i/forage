;; Compare effects of on walks of different values for mu.
;; No food search.
(ns forage.explore.comparemus
    (:require 
      [aerial.hanami.common :as hc]
      [aerial.hanami.templates :as ht]
      [forage.viz.hanami :as h]
      [forage.walks :as w]
      ;[forage.food :as f]
      [utils.math :as m]
      [utils.random :as r]))


; (require '[aerial.hanami.common :as hc] '[aerial.hanami.templates :as ht] '[foond.hanami-space :as hs])

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
;(def mus [1.000001 2 3])  ; mu near 1: decimal digits have big effect on speed
(def mus [2])  ; mu near 1: decimal digits have big effect on speed
;(def scales [1 8])
;(def scales [1 10 100])
(def scales [1 100 1000])
(def scale-mus (for [scale scales, mu mus] [scale mu]))

(def maxpathlen 5000)
(def trunclen 2005)
(def perceptual-radius 20) 
(def food-distance 200)

(def powerlaw-scale 1)

(def init-dir 0) ; NOT USED YET BUT could be: initial direction of all paths


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNGS and DISTS:

(def seed (inc (r/make-seed)))
(def rng (r/make-well19937 seed))

;(def dists (map (partial r/make-powerlaw rng powerlaw-scale) mus))
(def dists (map (fn [[scale mu]] (r/make-powerlaw rng scale mu))
                scale-mus))



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
(def vl-stop-seqs (map (fn [[scale mu] stop-seq]
                           (h/add-walk-labels (str "mu=" mu ",scale=" scale)
                                            stop-seq))
                       scale-mus stop-seqs))

;; Number of steps in each path:
(def n-steps (map count step-seqs))

;; Construct walk data for Hanami/Vega-Lite:
(def walks (apply concat (map take n-steps vl-stop-seqs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI/VEGA-LITE

(def gridwalk-plot (h/vega-gridwalk-plot
                     perceptual-radius maxpathlen powerlaw-scale n-steps
                     (h/vega-foodgrid-plot env-size plot-dim
                                           food-distance perceptual-radius)
                     (h/vega-walk-plot plot-dim plot-dim 1 walks)))


;; Now view gridwalk as vega-lite, e.g. with
(comment
  (require '[oz.core :as oz])
  (oz/view! gridwalk-plot)

  (def dist (r/make-powerlaw rng 100 2))
  (def min100max1000xs (repeatedly #(r/next-double dist 100 1000)))
  (def ys (take 500000 min100max1000xs))
  (def zs (map (partial r/powerlaw-cumulative 100 1000 2) ys))
  (def zsmean (/ (reduce + zs) 500000))




)
