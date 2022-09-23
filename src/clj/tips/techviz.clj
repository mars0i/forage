(ns tips.techviz
  (:require [tech.viz.vega :as tv]
            [utils.random :as r]))

(def seed (r/make-seed))
(def rng (r/make-well19937 seed))
(def dist (r/make-powerlaw rng 1 2))

(def levy-nums (repeatedly #(r/next-double dist)))
(def inverse-nums (map (partial r/pareto-cumulative 1 2) levy-nums))

;; obsolete I think--worked with earlier version Apache libs I was using:
;(def inverse-nums (map #(.cumulativeProbability dist %) levy-nums))

(defn hist
  "Slightly more convenient than tv/histogram, but with limitations.
  Example usage:
     (hist (take xs 1000) 100)"
  ([xs] (hist xs 50))
  ([xs bins] (tv/histogram xs "x" {:bin-count bins})))

(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! (hist (take 100 levy-nums)))
  (oz/view! (hist (take 100000 inverse-nums)))
)

