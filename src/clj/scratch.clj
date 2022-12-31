;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [utils.random :as r]
   [utils.toroidal :as t]
   [forage.walks :as w]
   [forage.viz.hanami :as h]   ; don't load with cljplot
   [forage.viz.cljplot :as cp] ; don't load with hanami
   [oz.core :as oz]
   [clojure.math.numeric-tower :as m]
   ))

"loaded"

;; By John Collins at https://stackoverflow.com/a/68476365/1455243
(defn irange
  "Inclusive range function: end element is included."
  ([start end step]
   (take-while (if (pos? step) #(<= % end) #(>= % end)) (iterate #(+ % step) start)))
  ([start end]
   (irange start end 1))
  ([end]
   (irange 0 end))
  ([] (range)))

(comment


  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  (def lambda 4.3)
  (defn logistic [x] (* lambda x (- 1 x)))
  (def a (+ 1/2 (m/sqrt (- 1/4 (/ lambda)))))
  (def lower (irange 0 a 0.01))
  (def upper (irange (- 1 a) 1 0.01))
  (map logistic lower)
  (map logistic upper)

  (oz/start-server!)
  (oz/view! example {:port 10667} )

  (require '[aerial.hanami.common :as hc])
  (require '[aerial.hanami.templates :as ht])


)
