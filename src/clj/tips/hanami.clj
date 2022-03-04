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
  (map (fn [x] {"num" x}) xs))

(defn vl-levy-hist
  [n]
  {:data (vegalize (take n levy-nums))
   :mark "bar"
   :encoding {
              :x {:bin true, :field "num"}
              :y {:aggregate "mean"}
             }
  })



;; NOT RIGHT
(defn levy-hist
  [n]
  (hc/xform ht/bar-chart
            :DATA (take n levy-nums)
            :HEIGHT 300
            :WIDTH 400
            :X "val"
            :Y "prob"
            :XTITLE "Random value"
            :YTITLE "Probability"))

