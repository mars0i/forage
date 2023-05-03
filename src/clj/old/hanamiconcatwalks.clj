(ns forage.explore.hanamiconcatwalks
  (:require [forage.viz.hanami :as h]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [forage.walks :as w]
            [forage.food :as f]
            [forage.run :as fr]
            [forage.mason.foodspot :as mf]
            [utils.math :as m]
            [utils.random :as r]))

;; Experiments with random walks using hconcat, vconcat, etc.
;; (Formerly in scratch.clj)

(def data [{"x" 100, "y" 200, "ord" 1, "label" "plot 1"}
           {"x" 500, "y" 300, "ord" 2, "label" "plot 1"}
           {"x" 100, "y" 500, "ord" 3, "label" "plot 2"}
           {"x" 800, "y" -300, "ord" 4, "label" "plot 2"}
           {"x" 200, "y" -200, "ord" 5, "label" "plot 2"}
           ])

;; Based on https://github.com/jsa-aerial/hanami/blob/master/README.md
(def cars-chart
  (hc/xform
   ht/point-chart
   :UDATA "https://vega.github.io/vega-lite/data/cars.json"
   :X "Horsepower" :Y "Miles_per_Gallon" :COLOR "Origin"))

(def concat-chart
  {:usermeta :USERDATA
   :title  :TITLE
   :height :HEIGHT
   :width :WIDTH
   :background :BACKGROUND
   :concat :CONCAT
   :columns :COLUMNS
   :resolve :RESOLVE
   :data ht/data-options
   :config ht/default-config})

(defn select-by-label
  [data label]
  (filter #(= (get % "label") label) 
          data))

(defn select-by
  [maps k v]
  (filter #(= (% k) v) maps))

(select-by data "ord" 4)

(defn add-ord
  [chart]
  (assoc-in chart [:encoding :order :field] "ord"))

(def hplot
  (->
   (hc/xform
    ht/hconcat-chart
    :TITLE "Yow, yeah!"
    :TOFFSET "10" ; space between meta-title and plots
    :HCONCAT [(hc/xform
               ht/line-chart
               :TITLE "Yow plot"
               :DATA (select-by-label data "plot 1")
               :MSIZE 2)
              (hc/xform
               ht/line-chart
               :TITLE "Yeah plot"
               :DATA (select-by-label data "plot 2")
               :MSIZE 2)])
   (update :hconcat #(map add-ord %))))

(def vplot
  (->
   (hc/xform
    ht/vconcat-chart
    :TITLE "Yow, yeah!"
    :TOFFSET "10" ; space between meta-title and plots
    :VCONCAT [(hc/xform
               ht/line-chart
               :TITLE "plot 1"
               :DATA data
               :TRANSFORM [{:filter {:field "label" :equal "plot 1"}}]
               :MSIZE 2)
              (hc/xform
               ht/line-chart
               :TITLE "plot 2"
               :DATA data
               :TRANSFORM [{:filter {:field "label" :equal "plot 2"}}]
               :MSIZE 2)])
   (update :vconcat #(map add-ord %))))


(def concat-example
  (hc/xform
   concat-chart
   :UDATA "https://vega.github.io/vega-lite/data/cars.json"
   :TITLE "MPG by horsepower for numbers of cylinders"
   :COLUMNS 3
   :CONCAT (mapv #(hc/xform
                   ht/point-chart
                   :TITLE (str "cylinders: " %)
                   :X "Horsepower"
                   :Y "Miles_per_Gallon"
                   :TRANSFORM [{:filter {:field "Cylinders" :equal 8}}])
                 [3, 4, 5, 6, 8])))

(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! concat-example)
  (oz/view! vplot)
  (oz/view! hplot)
  (oz/view! cars-chart)
)
