(ns tips.hanami
  (:require [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [oz.core :as oz]
            [forage.viz.hanami :as fh]
            [utils.random :as r]))

(def dist (r/make-powerlaw 1 2))
(def levy-nums (repeatedly #(r/next-double dist)))
(def inverse-nums (map #(.cumulativeProbability dist %) levy-nums))

(defn vegalize
  [xs]
  (map (fn [x] {"x" x}) xs))
