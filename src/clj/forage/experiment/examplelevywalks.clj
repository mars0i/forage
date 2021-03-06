(ns forage.experiment.examplelevywalks
  (:require [forage.walks :as w]
            [utils.random :as r]
            [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [forage.viz.hanami :as h]))

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
;(def mus [1.01 2 3])  ; mu near 1: decimal digits have big effect on speed
;(def mus [1.3 2 3])  ; mu near 1: decimal digits have big effect on speed
(def mus [2 3])  ; mu near 1: decimal digits have big effect on speed
;(def mus [2])
;(def scales [1])
;(def scales [1 8])
;(def scales [1 10 100])
;(def scales [1 100 1000])
;(def scale-mus (for [scale scales, mu mus] [scale mu]))

;(def maxpathlen half-size)
;(def maxpathlen env-size)
(def maxpathlen (* 4 env-size)) ; good for larger mus
(def trunclen maxpathlen)
(def perceptual-radius 20) 
(def food-distance 200)

(def powerlaw-scale 1)

(def mu-str "mu")
;(def mu-str "μ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNGS and DISTS:

(def seed (inc (r/make-seed)))
;(def seed -2925834069415719830)
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
                           (h/add-walk-labels (str mu-str "=" mu) stop-seq))
                       mus stop-seqs))

;; Number of steps in each path:
(def n-steps (map count step-seqs))

;; Construct walk data for Hanami/Vega-Lite:
(def each-walk (doall (map take n-steps vl-stop-seqs))) ; three seqs, one for each mu
(def all-walks (apply concat each-walk)) ; all mus together in one seq


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI/VEGA-LITE

(defn gridwalk-plot 
  [plot-dim walks]
  (-> (h/vega-gridwalk-plot
       perceptual-radius maxpathlen powerlaw-scale n-steps
       []
       (h/vega-walk-plot plot-dim env-size 1 walks)) ; "category20"
      (assoc :background "white")))


;; FIXME Doesn't work
(defn multiplot
  [plot-dim walks]
  (hc/xform
    ht/hconcat-chart
      :TITLE "Two walks"
      :TOFFSET 10 ; Space between overall title and the individual plot regions
      :DATA walks
      :HCONCAT [(hc/xform (gridwalk-plot plot-dim [first walks]))
                (hc/xform (gridwalk-plot plot-dim [second walks]))]))


;; Now view gridwalk as vega-lite, e.g. with
(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! (gridwalk-plot plot-dim all-walks))
  (oz/view! (gridwalk-plot plot-dim (nth each-walk 0)))
  (oz/view! (gridwalk-plot plot-dim (nth each-walk 1)))
  (oz/view! (gridwalk-plot plot-dim (nth each-walk 2)))

  (oz/view! (multiplot 500 each-walk))



)
