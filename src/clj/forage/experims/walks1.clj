(ns forage.experims.walks1
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

(def quadrant-size 1000)
(def figure-size 700)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA PARAMETERS:

;; $\mu \approx 1$ is is a "ballistic" Levy walk.
;; $\mu = 2$ is the theoretical optimum for searches.
;; $\mu = 3$ is a Brownian walk.
(def mus [1.000001 2 2.2 3])  ; mu near 1: decimal digits have big effect on speed
; (def mus [2])

(def maxpathlen 2000)
(def trunclen 2000)
(def perceptual-radius 5) 
(def food-distance 300)

(def powerlaw-scale 1)

(def init-dir 0) ; NOT USED YET BUT could be: initial direction of all paths


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNGS and DISTS:

(def seed (inc (r/make-int-seed)))
(def rng (r/make-well19937 seed))

(def dists (map (partial r/make-powerlaw rng powerlaw-scale) mus))


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
(def stop-seqs (map (fn [step-seq] (w/walk-stops [0 0] step-seq)) step-seqs))

;; Infinite sequences of stopes labeled for Hanami/Vega-Lite:
(def vl-stop-seqs (map (fn [mu stop-seq]
                           (h/add-walk-labels (str "mu=" mu) stop-seq))
                       mus stop-seqs))

;; Number of steps in each path:
(def n-steps (map count step-seqs))

;; Construct walk data for Hanami/Vega-Lite:
(def walks (apply concat (map take n-steps vl-stop-seqs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI/VEGA-LITE

(def walk-plot (h/vega-walk-plot quadrant-size figure-size walks))

(def foodgrid
  (hc/xform ht/point-chart 
            :DATA (h/make-foodgrid food-distance quadrant-size quadrant-size) 
            :X "x"
            :Y "y"
            :COLOR "type"
            :MSIZE (h/food-spot-mark-size perceptual-radius)
            :OPACITY 0.5  ; default is 0.7
            :WIDTH  figure-size
            :HEIGHT figure-size))

(def gridwalk (hc/xform
                ht/layer-chart
                :LAYER [foodgrid walk-plot]
                :TITLE (str "perceptual radius = " perceptual-radius ";  "
                            "max path len = " maxpathlen ";  "
                            "scale = " powerlaw-scale ";  "
                            "steps per path: " (vec n-steps))))

;; Now view gridwalk as vega-lite, e.g. with
;(require '[oz.core :as oz])
;(oz/start-server!)
;(oz/view! gridwalk)
