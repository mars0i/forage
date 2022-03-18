;; Experiments with conversion of Levy distributed numbers to 
;; uniformly distributed numbers.
(ns forage.experims.levytounif
  (:require [tech.viz.vega :as tv]
            [utils.random :as r]))

(def truncation-max 100)

(def seed (inc (r/make-seed)))
(println "SEED:" seed)
(def rng (r/make-well19937 seed))

(defn truncated-nums
  "Return a sequence made from levy-nums by removing all elements that
  are >= maxval.  (Note you can also perform truncation using next-double.)"
  [xs maxval]
  (filter (partial >= maxval) xs))


(def mu2dist (r/make-powerlaw rng 1 2))

(def unif-nums (repeatedly #(r/next-double rng)))

(def levy-nums (repeatedly #(r/next-double mu2dist)))
(def inverse-levy-nums (map (partial r/cumulative mu2dist) levy-nums))
(def levy-diffs (map - unif-nums inverse-levy-nums)) ; interesting plot: triangular

(def trunc-levy-nums (repeatedly #(r/next-double mu2dist 1 truncation-max)))
(def inverse-trunc-levy-nums
  (map (partial r/cumulative mu2dist 1 truncation-max) trunc-levy-nums))
(def trunc-levy-diffs (map - unif-nums inverse-levy-nums))

(defn hist
  "Slightly more convenient than tech.viz.vega/histogram, but with limitations.
  Example usage:
     (hist (take xs 1000) 100)"
  ([xs] (hist xs 50))
  ([xs bins] (tv/histogram xs "x" {:bin-count bins})))

(comment
  (require '[oz.core :as oz])

  (oz/view! (hist (take 100000 unif-nums)))

  (oz/view! (hist (take 100000 levy-nums)))
  (oz/view! (hist (take 100000 inverse-levy-nums)))
  (oz/view! (hist (take 100000 levy-diffs)))

  (oz/view! (hist (take 100000 trunc-levy-nums)))
  (oz/view! (hist (take 100000 inverse-trunc-levy-nums)))
  (oz/view! (hist (take 100000 trunc-levy-diffs)))
)

