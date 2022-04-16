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

(def concat-chart
  {:usermeta :USERDATA
   :title  :TITLE
   :height :HEIGHT
   :width :WIDTH
   :background :BACKGROUND
   :concat :CONCAT    ;; FIXME For this I need to add to the Hanami globals or local globals
   :resolve :RESOLVE
   :data ht/data-options
   :config ht/default-config})

(def plot
  (->
   (hc/xform
    concat-chart
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
               :MSIZE 2)
              (hc/xform
               ht/line-chart
               :TITLE "plot 3"
               :DATA (select-by-label data "yeah")
               :MSIZE 2)
              (hc/xform
               ht/line-chart
               :TITLE "plot 4"
               :DATA (select-by-label data "yeah")
               :MSIZE 2)
              (hc/xform
               ht/line-chart
               :TITLE "plot 5"
               :DATA (select-by-label data "yeah")
               :MSIZE 2)
              (hc/xform
               ht/line-chart
               :TITLE "plot 6"
               :DATA (select-by-label data "yeah")
               :MSIZE 2)
              ])
   (update :vconcat #(map add-ord %))))

(comment
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! plot)
)
