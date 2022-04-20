(ns utils.hanami
  (:require
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]))


;; For more info, see discussion at:
;; https://clojurians.zulipchat.com/#narrow/stream/210075-saite-dev/topic/concat.20template/near/279290717
(def grid-chart
  "Template for Vega-Lite \"concat\": multi-view charts.  Lays out
  a series of plots in rows and columns from left to right and top 
  to bottom.  The value of :COLUMNS specifies the number of columns."
  (-> ht/view-base
      (dissoc :encoding)
      (assoc :concat :CONCAT 
             :columns :COLUMNS 
             :resolve :RESOLVE)))

(comment
  (def grid-example
    (hc/xform
     grid-chart
     :UDATA "https://vega.github.io/vega-lite/data/cars.json"
     :TITLE "MPG by horsepower for each number of cylinders"
     :TOFFSET "10"
     :COLUMNS 3
     :CONCAT (mapv #(hc/xform
                     ht/point-layer
                     :TITLE (str "cylinders: " %)
                     :X "Horsepower"
                     :Y "Miles_per_Gallon"
                     ;; or put data here:
                     ;:UDATA "https://vega.github.io/vega-lite/data/cars.json"
                     :TRANSFORM [{:filter {:field "Cylinders" :equal %}}])
                   [3, 4, 5, 6, 8])))

  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! concat-example)
)
