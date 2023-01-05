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
  (def rng (r/make-well44497 seed))

  (def lambda 4.3)
  (defn logisticV1 [x] (* lambda x (- 1 x)))
  (def a (+ 1/2 (m/sqrt (- 1/4 (/ lambda)))))
  (def lower (irange 0 a 0.01))
  (def upper (irange (- 1 a) 1 0.01))
  (map logistic lower)
  (map logistic upper)

  (defn logistic [r x] (* r x (- 1 x)))
  (def l4 (partial logistic 4))
  (def xs (irange 0 1 0.0001))

  (defn l4 [x] (* 4 x (- 1 x)))

  (defn iterl4 [x] (iterate l4 x))

  ;; test of claim by Devaney in section 1.0.
  (defn looping? [limit xs]
    "Checks whether any elements after xs up to index limit are equal to
    the first element. If so, returns the cycle length; else returns nil."
    (let [x (first xs)]
      (loop [restxs (rest xs), i 1]
        (cond (empty? xs) nil
              (>= i limit) nil
              (= x (first restxs)) i
              :else (recur (rest restxs) (inc i))))))
      
  (def x0 (r/next-double rng))
  (def xs (iterl4 x0))
  (looping? 1000000 xs)

  (def result (let [x0 (r/next-double rng)
                    xs (iterl4 x0)
                    loop-length (looping? 1000000 xs)]
                (println x0)
                (when loop-length
                  (println "Found loop of length" loop-length)
                  [x0 xs])))

  (take 100 xs)



  (oz/start-server!)
  (oz/view! example {:port 10667} )

  (require '[aerial.hanami.common :as hc])
  (require '[aerial.hanami.templates :as ht])


)
