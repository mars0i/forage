;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require [forage.viz.hanami :as h]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [forage.walks :as w]
            [forage.food :as f]
            [forage.run :as fr]
            [forage.mason.foodspot :as mf] 
            [cljplot.build :as cb]
            [cljplot.render :as cr]
            [cljplot.core :as cc]
            [utils.math :as m]
            [utils.random :as r]))


(comment

  (def rng (r/make-well19937))

  (def len-dist (r/make-powerlaw rng 1 2))
  (def steps (repeatedly (w/step-vector-fn rng len-dist 1 2000)))
  (def indexed-steps (map conj steps (range)))
(take 4 steps)

  ;; Based on https://github.com/generateme/cljplot/blob/master/sketches/vega.clj#L570
  (def plot-result
    (let [data (take 100 steps)]
      (-> 
        ;(cb/series [:grid] [:line data {:stroke {:size 2} :point {:type \O}}])
       (cb/series [:grid] [:line data {:stroke {:size 1}}])
       (cb/preprocess-series)
       ;(cb/update-scale :x :ticks 4)
       (cb/update-scale :x :domain [-100 100])
       (cb/update-scale :y :domain [-100 100])
       (cb/add-axes :bottom)
       (cb/add-axes :left)
       (cb/add-label :bottom "x")
       (cb/add-label :left "y")
       (cr/render-lattice {:width 400 :height 400 :border 20})
       ;(cc/save "yo.jpg")
       (cc/show)
       )))


  (def env (mf/make-env 10 1000))
  (def look-fn (partial mf/perc-foodspots-exactly-toroidal env 1))
  (def fw (w/levy-foodwalk look-fn 0.1 10000 false 10000 rng 1 2 [0 0]))

  (map count fw)
  (= (nth fw 1) (nth fw 2))



  (defn ignore-food [x y] nil)

  (require '[oz.core :as oz])
  (oz/start-server!)

  (def data [{"x" 100, "y" 200, "ord" 1, "label" "yow"}
             {"x" 500, "y" 300, "ord" 2, "label" "yow"}
             {"x" 100, "y" 500, "ord" 3, "label" "yeah"}
             {"x" 800, "y" -300, "ord" 4, "label" "yeah"}
             {"x" 250, "y" -200, "ord" 5, "label" "yeah"}
             ])

  (keep identity [1 2 3 nil 4 false 5])
  (filter identity [1 2 3 nil 4 false 5])

  (defn select-by-label
    [data label]
    (filter #(= (get % "label") label) 
            data))

  (select-by-label data "yow")
  (select-by-label data "yeah")

  (defn add-ord
    [chart]
    (assoc-in chart [:encoding :order :field] "ord"))

  (def plot
    (->
     (hc/xform
      ht/vconcat-chart
      :TITLE "Yow, yeah!"
      :TOFFSET "10" ; space between meta-title and plots
      :VCONCAT [(hc/xform
                 ht/line-chart
                 :TITLE "Yow plot"
                 :DATA (select-by-label data "yow")
                 :MSIZE 2)
                (hc/xform
                 ht/line-chart
                 :TITLE "Yeah plot"
                 :DATA (select-by-label data "yeah")
                 :MSIZE 2)])
     (update :vconcat #(map add-ord %))))

  plot

  (oz/view! plot)


)


