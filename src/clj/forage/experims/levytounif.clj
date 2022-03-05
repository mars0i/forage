;; Experiments with conversion of Levy distributed numbers to 
;; uniformly distributed numbers.
(ns tips.levytounif
  (:require [tech.viz.vega :as tv]
            [utils.random :as r]))

(def seed (inc (r/make-seed)))
(println "SEED:" seed)
(def rng (r/make-well19937 seed))

(def dist (r/make-powerlaw rng 1 2))

(def unif-nums (repeatedly #(r/next-double rng)))
(def levy-nums (repeatedly #(r/next-double dist)))
(def inverse-nums (map #(.cumulativeProbability dist %) levy-nums))
(def diffs (map - unif-nums inverse-nums)) ; interesting plot: triangular

;; Note you can't just apply .cumulativeProbability to the result
;; to get an inverse truncated distribution; the cummlative probability
;; is different for a truncated distribution.  See PowerlawAndPareto.md
;; and paretodistnotes.md.
(defn truncated-nums
  "Return a sequence made from levy-nums by removing all elements that
  are >= maxval.  (Note you can also perform truncation using next-double.)"
  [xs maxval]
  (filter (partial >= maxval) xs))

(defn hist
  "Slightly more convenient than tv/histogram, but with limitations.
  Example usage:
     (hist (take xs 1000) 100)"
  ([xs] (hist xs 50))
  ([xs bins] (tv/histogram xs "x" {:bin-count bins})))

(comment
(oz/view! (hist (take 100000 inverse-nums)))
(oz/view! (hist (take 100000 unif-nums)))
(oz/view! (hist (take 100000 (truncated-levy 25))))
)

