;; Plots that I used as illustrations when I submitted issue 
;; https://github.com/vega/vega-lite/issues/7996
(ns forage.experims.toroidalillustration)

(def toroidal-plot
  {
  "width" 200,
  "height" 200,
  "background" "floralwhite"
  "layer" [
      {"encoding" {
        "y" {"field" "y", "type" "quantitative"},
        "color" {"field" "label", "type" "nominal"},
        "order" {"field" "ord"},
        "x" {"field" "x", "type" "quantitative"},
        "tooltip" [
          {"field" "x", "type" "quantitative"},
          {"field" "y", "type" "quantitative"}
        ]
      },
      "mark" {"type" "line", "strokeWidth" 1},
      "data" {
        "values" [{"x" 0, "y" 100, "ord" 0}
                  {"x" 100, "y" 100, "ord" 1}
                   {"x" 150, "y" 200, "ord" 2}
                  ]
               }
      }
      {"encoding" {
        "y" {"field" "y", "type" "quantitative"},
        "color" {"field" "label", "type" "nominal"},
        "order" {"field" "ord"},
        "x" {"field" "x", "type" "quantitative"},
        "tooltip" [
          {"field" "x", "type" "quantitative"},
          {"field" "y", "type" "quantitative"}
        ]
      },
      "mark" {"type" "line", "strokeWidth" 1},
      "data" {
        "values" [{"x" 150, "y" 0, "ord" 0}
                   {"x" 200, "y" 100, "ord" 1}
                   {"x" 175, "y" 125, "ord" 2}
                  ]
               }
      }
      ]
  }
)

(def nontoroidal-plot
  {
   "width" 200,
   "height" 400,
   "background" "floralwhite"
   "autosize" "pad",
   "layer" [
            {"encoding" {
                         "y" {"field" "y", "type" "quantitative"},
                         "color" {"field" "label", "type" "nominal"},
                         "order" {"field" "ord"},
                         "x" {"field" "x", "type" "quantitative"},
                         "tooltip" [
                                    {"field" "x", "type" "quantitative"},
                                    {"field" "y", "type" "quantitative"}
                                    ]
                         },
             "mark" {"type" "line", "strokeWidth" 1},
             "data" {
                     "values" [{"x" 0, "y" 100, "ord" 0}
                               {"x" 100, "y" 100, "ord" 1}
                               {"x" 150, "y" 200, "ord" 2}
                               {"x" 200, "y" 300, "ord" 3}
                               {"x" 175, "y" 325, "ord" 4}
                               ]
                     }
             }
            ]
   }
  )
