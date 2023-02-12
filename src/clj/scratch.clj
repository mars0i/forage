;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [utils.random :as r]
   [utils.toroidal :as t]
   [forage.food :as f]
   [forage.walks :as w]
   [forage.viz.hanami :as h]   ; don't load with cljplot
   ;[forage.viz.cljplot :as cp] ; don't load with hanami
   [oz.core :as oz]
   [clojure.math.numeric-tower :as m]
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]
   ))

"loaded"

(comment
  (oz/start-server!)

  (def grid1 (f/slide-grid 10 0 0 0 0 100 100))
  (def vgrid1 (map h/make-foodspot grid1))
  (def food1 (h/vega-food-plot vgrid1 100 400 1))
  (oz/view! food1)
  
  (def grid2 (f/slide-grid 10 8 8 0 0 100 100))

  (def vgrid2 (map h/make-foodspot grid2))
  (def food2 (h/vega-food-plot vgrid2 100 400 1))
  (oz/view! food2)

  (def vgrids2 (h/split-foodgrid grid2))
  (def foods2 (h/vega-food-plot vgrids2 100 400 1))
  (oz/view! foods2)

)



(comment
  (def plot (hc/xform ht/line-chart :UDATA [{"x" 0 "y" 1} {"x" 1 "y" 0} {"x" 2 "y" 2}]))
  (oz/export! plot "yo.svg")

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


)
