(ns tips.oz
  (require [oz.core :as oz]))

(comment
(oz/start-server!)
(oz/view! vega-lite-spec)
)

(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i
     :item n
     :quantity
     (+ (Math/pow (* i (count n)) 0.8) (rand-int (count n)))}))

(def line-plot
  {:data {:values (play-data "monkey" "slipper" "broom")}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative"}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

;; Render the plot
; (oz/view! line-plot)

(def stacked-bar
  {:data {:values (play-data "munchkin" "witch" "dog" "lion" "tiger" "bear")}
   :mark "bar"
   :encoding {:x {:field "time"
                  :type "ordinal"}
              :y {:aggregate "sum"
                  :field "quantity"
                  :type "quantitative"}
              :color {:field "item"
                      :type "nominal"}}})

;(oz/view! stacked-bar)

(def viz
  [:div
    [:h1 "Look ye and behold"]
    [:p "A couple of small charts"]
    [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite line-plot]
      [:vega-lite stacked-bar]]
    [:p "A wider, more expansive chart"]
    [:vega line-plot]
    [:h2 "If ever, oh ever a viz there was, the vizard of oz is one because, because, because..."]
    [:p "Because of the wonderful things it does"]])

;(oz/view! viz)

; Also note that while not illustrated above, you can specify multiple
; maps in these vectors, and they will be merged into one. So for example,
; you can do 
;	[:vega-lite stacked-bar {:width 100}]
; to override the width.

;; Oz now features Figwheel-like hot code reloading for Clojure-based data
;; science workflows. To start this functionality, you specify from the
;; REPL a file you would like to watch for changes, like so:
;    (oz/live-reload! "live-reload-test.clj")

; We can also export static HTML files which use Vega-Embed to render
; interactive Vega/Vega-Lite visualizations using the oz/export! function.
;    (oz/export! spec "test.html")

