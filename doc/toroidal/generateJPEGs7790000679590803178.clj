;; Generate three plots as jpegs to illustrate toroidal wrapping
(ns generateJPEGs7790000679590803178
  (:require
   [utils.random :as r]
   [forage.walks :as w]
   [forage.toroidal :as t]
   [forage.viz.cljplot :as cp]))

;(def seed (r/make-seed))
(def seed 7790000679590803178)
(def rng (r/make-well19937 seed))
(def len-dist (r/make-powerlaw rng 1 2))
(def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 1000)))
(def stops (w/walk-stops [0 0] (w/vecs-upto-len 4000 step-vector-pool)))
(def stops- (drop 200 stops))
(cp/three-plots 500 200 stops-)

