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

"test2"

(comment

  (def walk1 (h/add-walk-labels "segs1" [[5 5] [5 6] [8 1] [9 4]]))
  (def walk2 (h/add-walk-labels "segs2" [[9 3] [3 2] [4 6] [7 8]]))
  (def walk2 [{"x" 9, "y" 3, "ord" 4, "label" "segs2"}
              {"x" 3, "y" 2, "ord" 5, "label" "segs2"}
              {"x" 4, "y" 6, "ord" 6, "label" "segs2"}
              {"x" 7, "y" 8, "ord" 7, "label" "segs2"}])

  (def seed (r/make-seed))
  (def seed 7790000679590803178)
  (def rng (r/make-well19937 seed))

  (oz/start-server!)
  (oz/view! example {:port 10667} )

  (require '[aerial.hanami.common :as hc])
  (require '[aerial.hanami.templates :as ht])


)
