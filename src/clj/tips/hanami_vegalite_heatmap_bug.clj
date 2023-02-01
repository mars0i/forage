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


(defn make-heatmap
  [maxbins data]
  (hc/xform ht/heatmap-chart
            :DATA data
            :X "x"
            :Y "y"
            :XBIN {:maxbins maxbins}
            :YBIN {:maxbins maxbins}
            :COLOR {:aggregate "count" :type "quantitative"}
            :WIDTH  400
            :HEIGHT 400))

(defn make-heatmap-notip
  [maxbins data]
  (hc/xform ht/heatmap-chart
            :TOOLTIP ht/RMV
            :DATA data
            :X "x"
            :Y "y"
            :XBIN {:maxbins maxbins}
            :YBIN {:maxbins maxbins}
            :COLOR {:aggregate "count" :type "quantitative"}
            :WIDTH  400
            :HEIGHT 400))


(comment
  (def hmap (make-heatmap 2 data)) 
  (def hmap-notip (make-heatmap-notip 2 data)) 

  (def vl
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


  (def vl-notip
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


  (oz/start-server!)
  (oz/view! hmap)
  (oz/view! hmap-notip)
  (oz/view! vl)
  (oz/view! vl-notip)

)
