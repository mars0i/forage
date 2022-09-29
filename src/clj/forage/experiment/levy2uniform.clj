;; Demonstration that a cumulative pareto distribution
;; function transforms a Lévy distribution into a uniform
;; distribution.  (Originally, this was a copy of tips/techviz.clj)
(ns experiment.levy2uniform
  (:require [tech.viz.vega :as tv]
            [utils.math :as m]
            [utils.random :as r]))

(def mu 2)
(def minval 10)
(def maxval 1500) ; for truncated Lévy walk

(def seed (r/make-seed))
(def rng (r/make-well19937 seed))
(def dist (r/make-powerlaw rng minval mu))

(def pure-levy-nums (repeatedly #(r/next-double dist)))
(def pure-inverse-nums (map (partial r/powerlaw-cumulative mu minval) pure-levy-nums))

(def trunc-levy-nums (repeatedly #(r/next-double dist minval maxval)))
(def trunc-inverse-nums (map (partial r/powerlaw-cumulative mu minval maxval) trunc-levy-nums))

(defn hist
  "Slightly more convenient than tv/histogram, but with limitations.
  Example usage:
     (hist (take xs 1000) 100)"
  ([xs] (hist xs 50))
  ([xs bins] (tv/histogram xs "x" {:bin-count bins})))

(comment
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def n 100000)

  (oz/view! (hist (take n trunc-levy-nums)))
  (oz/view! (hist (take n trunc-inverse-nums)))
  (m/mean (take n trunc-inverse-nums))

  (oz/view! (hist (take n pure-levy-nums)))
  (oz/view! (hist (take n pure-inverse-nums)))
  (m/mean (take n pure-inverse-nums))

)

