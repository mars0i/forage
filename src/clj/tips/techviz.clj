(ns tips.techviz
  (:require [tech.viz.vega :as tv]
            [utils.random :as r]))

(def dist (r/make-powerlaw 1 2))

(def levy-nums (repeatedly #(r/next-double dist)))

(def inverse-nums (map #(.cumulativeProbability dist %)
                       levy-nums))

(defn hist
  "Slightly more convenient than tv/histogram, but with limitations.
  Example usage:
     (hist (take xs 1000) 100)"
  ([xs] (hist xs 50))
  ([xs bins] (tv/histogram xs "x" {:bin-count bins})))
