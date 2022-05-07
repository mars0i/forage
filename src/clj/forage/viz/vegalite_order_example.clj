(ns forage.viz.vegalite-order-example)

;; From https://behrica.github.io/vl-galery/convert, based on
;; https://vega.github.io/vega-lite/examples/connected_scatterplot.html
(def connected-scatterplot
  {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
   :data {:url "https://vega.github.io/vega-lite/data/driving.json"}
   :encoding {:order {:field "year"}
   ;:encoding {:order {:field "year" :type "ordinal"} ; fixes warning 
              :x {:field "miles" :scale {:zero false} :type "quantitative"}
              :y {:field "gas" :scale {:zero false} :type "quantitative"}}
   :mark {:point true :type "line"}})
