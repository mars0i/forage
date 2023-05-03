;; Demonstration that a cumulative pareto distribution
;; function transforms a Lévy distribution into a uniform
;; distribution.  (Originally, this was a copy of tips/techviz.clj)
(ns forage.experiment.levy2uniform
  (:require [tech.viz.vega :as tv]
            [utils.math :as m]
            [utils.random :as r]))

(def mu 2)
(def minval 10)
(def maxval 1500) ; for truncated Lévy walk

(def seed (r/make-seed))
;; (def rng (r/make-well19937 seed))
(def rng (r/make-well44497 seed))
(def dist (r/make-powerlaw rng minval mu))

(def pure-levy-nums (repeatedly #(r/next-double dist)))
(def pure-inverse-nums (map (partial r/powerlaw-cumulative mu minval) pure-levy-nums))

(def trunc-levy-nums (repeatedly #(r/next-double dist minval maxval)))
(def trunc-inverse-nums (map (partial r/powerlaw-cumulative mu minval maxval) trunc-levy-nums))

(def unif-nums (repeatedly #(r/next-double rng)))

(defn hist
  "Slightly more convenient wrapper of tech.viz.vega/histogram, but
  with limitations. Example usage:
     (hist (take xs 1000) 100)"
  ([xs] (hist xs 50))
  ([xs bins] (tv/histogram xs "x" {:bin-count bins})))

(comment

  ;(def unifrecs (map vector unifnums)) ; wrap each number in a vector
  (require 'forage.core.run :reload)
  (forage.core.run/spit-csv "data.txt" (take 100000000 (map vector (repeatedly #(r/next-double rng)))))

  (require '[oz.core :as oz])
  (oz/start-server!)

  (def n 100000)
  (def m 10000)

  (def trunchist (hist (take 400000 trunc-levy-nums)))
  (oz/view! trunchist)
  (count (:values (first (:data trunchist)))) ;=> 50 by default
  ;; Extract heights from histogram bin data:
  (def bincounts (map :count (:values (first (:data trunchist)))))
  ;; There's probably a more direct method, but this works

  (oz/view! (hist (take m trunc-levy-nums)))
  (oz/view! (hist (take m trunc-inverse-nums)))
  (m/mean (take n trunc-inverse-nums))
  (oz/view! (hist (take n unif-nums)))
  (m/mean (take n unif-nums))

  (oz/view! (hist (take m pure-levy-nums)))
  (oz/view! (hist (take n pure-inverse-nums)))
  (m/mean (take n pure-inverse-nums))

)

