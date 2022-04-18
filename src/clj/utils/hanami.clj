(ns utils.hanami
  (:require
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]))

(def concat-chart
  "Template for Vega-Lite \"concat\": multi-view charts.  Lays out
  a series of plots in rows and columns from left to right and top 
  to bottom.  The value of :COLUMNS specifies the number of columns."
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

(comment
  (def concat-example
    (hc/xform
     concat-chart
     :UDATA "https://vega.github.io/vega-lite/data/cars.json"
     :TITLE "MPG by horsepower for each number of cylinders"
     :TOFFSET "10"
     :COLUMNS 3
     :CONCAT 
     (mapv #(hc/xform
              ht/point-chart
              :TITLE (str "cylinders: " %)
              :X "Horsepower"
              :Y "Miles_per_Gallon"
              :TRANSFORM [{:filter {:field "Cylinders" :equal %}}])
           [3, 4, 5, 6, 8])))

  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! concat-example)
)
