;; MWE illustrating two possible bugs, I think:
;;  - Hanami adds a misleading tooltip to heatmaps.  
;;    (How? I don't think this is intended behavior, and I don't see what causes it.)
;;  - Vega or Vega-Lite causes the tooltip to filter the data, which seems wrong.

(ns tips.hanam-vegalite-heatmap-bug
  (:require
    [aerial.hanami.common :as hc]
    [aerial.hanami.templates :as ht]
    [oz.core :as oz]))

(def data [{"x" 2 "y" 2} {"x" 2 "y" 2.5} {"x" 2.5 "y" 2} {"x" 3 "y" 2}
           {"x" 3 "y" 2.5} {"x" 3.5 "y" 2} {"x" 3.5 "y" 2.5}
           {"x" 5 "y" 2} {"x" 5 "y" 2.5} {"x" 5 "y" 2}
           {"x" 5.5 "y" 2} {"x" 5.5 "y" 2}
           {"x" 10 "y" 5} {"x" 9 "y" 5} {"x" 10.5 "y" 6} {"x" 9 "y" 5.5}
           {"x" 9.5 "y" 6}])

;; Create a Vega-Lite heatmap using Hanami.  The result is listed below.
(def hmap (hc/xform
            ht/heatmap-chart
            :DATA data
            :X "x"
            :Y "y"
            :XBIN {:maxbins 2}
            :YBIN {:maxbins 2}
            :COLOR {:aggregate "count" :type "quantitative"}
            :WIDTH  400
            :HEIGHT 400))

;; Same thing with tooltips removed.  The result is listed below.
(def hmap-notip (hc/xform
                  ht/heatmap-chart
                  :TOOLTIP ht/RMV
                  :DATA data
                  :X "x"
                  :Y "y"
                  :XBIN {:maxbins 2}
                  :YBIN {:maxbins 2}
                  :COLOR {:aggregate "count" :type "quantitative"}
                  :WIDTH  400
                  :HEIGHT 400))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hmap: Note tooltip in line 4:
{:encoding {:y {:field "y", :type "quantitative", :bin {:maxbins 2}},
            :color {:aggregate "count", :type "quantitative"},
            :x {:field "x", :type "quantitative", :bin {:maxbins 2}},
            :tooltip [{:field "x", :type "quantitative"}
                      {:field "y", :type "quantitative"}]},
 :width 400,
 :background "floralwhite",
 :layer [{:mark {:type "rect"},
          :encoding {:color {:aggregate "count", :type "quantitative"}}}
         {:mark {:type "text"}}],
 :height 400,
 :data {:values
        [{"x" 2, "y" 2}
         {"x" 2, "y" 2.5}
         {"x" 2.5, "y" 2}
         {"x" 3, "y" 2}
         {"x" 3, "y" 2.5}
         {"x" 3.5, "y" 2}
         {"x" 3.5, "y" 2.5}
         {"x" 5, "y" 2}
         {"x" 5, "y" 2.5}
         {"x" 5.5, "y" 2}
         {"x" 5, "y" 2}
         {"x" 5.5, "y" 2}
         {"x" 10, "y" 5}
         {"x" 9, "y" 5}
         {"x" 10.5, "y" 6}
         {"x" 9, "y" 5.5}
         {"x" 9.5, "y" 6}]}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hmap-notip: no tooltip:
{:encoding {:y {:field "y", :type "quantitative", :bin {:maxbins 2}},
            :color {:aggregate "count", :type "quantitative"},
            :x {:field "x", :type "quantitative", :bin {:maxbins 2}}},
 :width 400,
 :background "floralwhite",
 :layer [{:mark {:type "rect"},
          :encoding {:color {:aggregate "count", :type "quantitative"}}}
         {:mark {:type "text"}}],
 :height 400,
 :data {:values
        [{"x" 2, "y" 2}
         {"x" 2, "y" 2.5}
         {"x" 2.5, "y" 2}
         {"x" 3, "y" 2}
         {"x" 3, "y" 2.5}
         {"x" 3.5, "y" 2}
         {"x" 3.5, "y" 2.5}
         {"x" 5, "y" 2}
         {"x" 5, "y" 2.5}
         {"x" 5.5, "y" 2}
         {"x" 5, "y" 2}
         {"x" 5.5, "y" 2}
         {"x" 10, "y" 5}
         {"x" 9, "y" 5}
         {"x" 10.5, "y" 6}
         {"x" 9, "y" 5.5}
         {"x" 9.5, "y" 6}]}})


(comment
  (oz/start-server!)
  (oz/view! hmap)
  (oz/view! hmap-notip)
)
