;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [utils.random :as r]
   [forage.walks :as w]
   [forage.toroidal :as t]
   [forage.viz.cljplot :as cp]))

(comment

  (def seed (r/make-seed))
  (def seed 7790000679590803178)
  (def rng (r/make-well19937 seed))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def len-dist (r/make-powerlaw rng 1 2.5))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 1000)))
  (def stops (w/walk-stops [0 0] (w/vecs-upto-len 4000 step-vector-pool)))
  (def stops- (drop 200 stops))
  (count stops)
  (count stops-)

  (cp/three-plots 500 200 stops-)
  (first stops-)
  (cp/three-plots 500 200 stops)

)
