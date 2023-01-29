(ns forage.experiment.examplelevywalks
  (:require [forage.walks :as w]
            [utils.random :as r]
            [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [forage.viz.hanami :as h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI SPACE PARAMETERS:

;(def env-size 5000)
(def env-size 3000)
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
;(def maxpathlen (* 4 env-size)) ; good for larger mus
(def maxpathlen 20000)
(def trunclen maxpathlen)
(def perceptual-radius 20) 
(def food-distance 200)

(def powerlaw-scale 1)

(def mu-str "mu")
;(def mu-str "μ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNGS and DISTS:

(def seed (inc (r/make-seed)))
;(def seed 6588465356956694642) ; version "500"
;(def seed -3481899242910030715) ; version "501"
;(def seed 3606716751769783334) ; version "600"
;(def seed -8570974758410904124) ; version "601"
;(def seed -3499416562441271185) ; version "602"
(def seed -5280359680172809672) ; version "603"

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
                      ;(w/walk-stops [half-size half-size] step-seq))
                      (w/walk-stops [2500 2500] step-seq))
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
(def reverse-all-walks (apply concat (reverse each-walk)))




;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HANAMI/VEGA-LITE

;; This works OK, but for the path coloring, you're at the mercy of
;; whatever colorschemes you can choose.  This is because h/vega-walk-plot
;; uses Hanami's :COLOR, which puts a color expression in the encoding
;; block in a way that automatically assigns colors.  The alternative is
;; to specific colors more rigidly and explicitly in the mark blocks.
;; The Hanami version of this is :MCOLOR.
(defn gridwalk-plot 
  [plot-dim walks]
  (-> (h/vega-gridwalk-plot
     "N/A" maxpathlen powerlaw-scale n-steps
       []
       ;(h/vega-walk-plot plot-dim env-size 1.5 walks)) 
       (h/vega-walk-plot plot-dim 750 3400 1.25 false walks :color-scheme "greys")) ;; ZoomOut setting used for version 603
       ;(h/vega-walk-plot plot-dim 2000 2700 0.75 false walks "greys")) ;; ZoomIn settting used for version 603
       ;(h/vega-walk-plot plot-dim 2250 2700 0.5 false walks "grays")) ;; ZoomIn for Brownian walk alone
      (assoc :background "white")))


(defn gridwalk-layered-plot
  [plot-dim gridwalk-plots]
  (hc/xform ht/layer-chart :LAYER gridwalk-plots))


;; Now view gridwalk as vega-lite, e.g. with
(comment

  ;  (defn kludgewalk-plot 
  ;    [plot-dim walks]
  ;    (let [walk0 (list (first walks))
  ;          walk1 (list (second walks))]
  ;      (-> (h/vega-gridwalk-plot
  ;            "N/A" maxpathlen powerlaw-scale n-steps
  ;            []
  ;            (h/vega-walk-plot plot-dim 800 3500 1.0 false walk0))
  ;          (assoc :background "white"))))

  (def all-walks-plot (gridwalk-plot plot-dim all-walks))

  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! (gridwalk-plot plot-dim all-walks))
  (oz/view! (gridwalk-plot plot-dim reverse-all-walks))
  (oz/view! (gridwalk-plot plot-dim (nth each-walk 0)))
  (oz/view! (gridwalk-plot plot-dim (nth each-walk 1)))

  ;; Zoomed-out, two-mu plot with black mu=3 and a middle, clear gray for mu=2:
  (oz/view!
    (hc/xform
      ht/layer-chart
      :LAYER 
      [(h/vega-walk-plot plot-dim 750 3400 1.25 false (nth each-walk 0) :mark-color "#808080")
       (h/vega-walk-plot plot-dim 750 3400 1.25 false (nth each-walk 1) :mark-color "black")]))




)
