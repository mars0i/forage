;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [utils.random :as r]
   [forage.walks :as w]
   [forage.toroidal :as t]
   [forage.viz.hanami :as h]   ; don't load with cljplot
   [forage.viz.cljplot :as cp] ; don't load with hanami
   [oz.core :as oz]
   ))

(comment

  (def walk1 (h/add-walk-labels "segs1" [[5 5] [5 6] [8 1] [9 4]]))
  (def walk2 (h/add-walk-labels "segs2" [[9 3] [3 2] [4 6] [7 8]]))
  (def walk2 [{"x" 9, "y" 3, "ord" 4, "label" "segs2"}
              {"x" 3, "y" 2, "ord" 5, "label" "segs2"}
              {"x" 4, "y" 6, "ord" 6, "label" "segs2"}
              {"x" 7, "y" 8, "ord" 7, "label" "segs2"}])
  (oz/view! (h/vega-walk-plot 400 10 1 (concat walk1 walk2)))

  (def seed (r/make-seed))
  (def seed 7790000679590803178)
  (def rng (r/make-well19937 seed))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 1000)))
  (def stops (w/walk-stops [0 0] (w/vecs-upto-len 4000 step-vector-pool)))
  (def stops- (drop 200 stops))
  (def wrapped-stops- (t/wrap-path -200 200 stops-))
  (count stops)
  (count stops-)

  (oz/view!   (h/vega-walk-plot 400 -200 200 1 (t/add-walk-labels "segs" stops-)))
  (oz/export! (h/vega-walk-plot 400 -200 200 1 (t/add-walk-labels "segs" stops-))
              "unmod7790000679590803178.svg")

  (oz/view!   (h/vega-walk-plot 400 -200 200 1 (t/toroidal-to-vega-lite "segs" wrapped-stops-)))
  (oz/export! (h/vega-walk-plot 400 -200 200 1 (t/toroidal-to-vega-lite "segs" wrapped-stops-))
              "loose7790000679590803178.svg")

  (oz/view! (h/vega-walk-plot 200 -200 200 1 (t/toroidal-to-vega-lite "segs" wrapped-stops-)))

  (cp/plot-walk 725 200 wrapped-stops- "loose.jpg")

  (cp/three-plots 600 200 stops-)
  (first stops-)
  (cp/three-plots 500 200 stops)

)
