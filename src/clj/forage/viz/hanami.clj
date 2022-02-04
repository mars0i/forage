;; Functions for plotting things in 2D spatial coordinates using Hanami
;; and Vega-Lite.
(ns forage.viz.hanami
    (:require [clojure.math.numeric-tower :as nt]
              [aerial.hanami.common :as hc]
              [aerial.hanami.templates :as ht]
              [forage.food :as f]
              [utils.math :as m]))

;; Note field names have to be strings, not keywords, in order
;; for vega-lite to make full use of them.

;; TODO let user add to the xform expression instead of Vega-Lite directly
(defn vega-walk-plot
  "Constructs a Vega-Lite random walk plot from (vega-lite-ified) data
  over the range (2*quadrant-sz x 2*quandrant-sz), with physical size 
  plot-dim x plot-dim.  If there are additional Vega-Lite specs
  to add, they can be entered in a map as an additional argument."
  [quadrant-sz plot-dim data & addl-kvs-map]
  (merge
     (-> (hc/xform ht/line-chart
                :DATA data
                :XSCALE {"domain" [(- quadrant-sz) quadrant-sz]}
                :YSCALE {"domain" [(- quadrant-sz) quadrant-sz]}
                :COLOR "label"
                :WIDTH  plot-dim
                :HEIGHT plot-dim)
         (assoc-in [:encoding :order :field] "ord") ; walk through lines in order not L-R
         (assoc-in [:mark :strokeWidth] 1)
      ) 
    (first addl-kvs-map)))

(defn add-point-labels
  "Given a sequence of pairs representing x,y coordinates, returns a
  Vega-Lite data specification in the form of a sequence of maps,
  where the coordinates are values for keys \"x\" and \"x\", and the 
  same \"label\" key and value is added to each map."
  [label xys]
  (map (fn [[x y]] {"x" x, "y" y, "label" label})
    xys))

(defn add-walk-labels
  "Given a sequence of pairs representing x,y coordinates, returns a
  Vega-Lite data specification in the form of a sequence of maps,
  where the coordinates are values for keys \"x\" and \"x\", and the 
  same \"label\" key and value is added to each map.  In addition,
  each map is given an \"ord\" key with increasing integers as values.
  This can be used with the Vega-Lite \"order\" key."
  [label xys]
  (map (fn [[x y] n] {"x" x, "y" y, "ord" n, "label" label})
       xys (range)))

(defn plot-dist
  [y-fn label xmin xmax increment]
  (let [xs (range xmin xmax increment)]
    (add-point-labels label 
                      (map vector xs (map y-fn xs)))))

(defn rotate-vega-point
  [theta point]
  (let [{x "x", y "y"} point
        [rx ry] (m/rotate theta [x y])]
    (assoc point "x" rx, "y" ry)))

(defn make-line-to-point
  [type-label end-x end-y]
  [{"x" 0,     "y" 0,     "label" type-label}
   {"x" end-x, "y" end-y, "label" type-label}])

(defn rotate-line-to-point
  "Rotate a line from the origin to an endpoint in rectangular coordinates.
  A line is a pair of two maps, each containing coordinate field names 
  \"x\" and \"y\" (*not* keywords)."
  [theta line-to-point]
  (let [endpt1 (first line-to-point)
        endpt2 (second line-to-point)
        [start finish] (if (and (= (endpt1 "x") 0)
                                (= (endpt1 "y") 0))
                         [endpt1 endpt2]
                         [endpt2 endpt1])]
    [start (rotate-vega-point theta finish)]))

(defn make-linegrid
  "Add Vega-Lite point and type labels to pairs of coordinate pairs
  representing lines."
  [label sep quadrant-width quadrant-height]
  (map (partial add-point-labels label)
       (f/linegrid sep quadrant-width quadrant-height)))

(defn make-foodspot
  "Given a pair of coordinates, returns a Vega-Lite map with an added
  label value \"food\"."
  [[x y]]
  {"x" x, "y" y, "label" "food"})

;; For the Hanami :MSIZE value:
;; Vega-Lite "mark" sizes--i.e. a circle at a point--are specified
;; by the *area of the bounding box around the circle*.  The BB's
;; edges touch the top, bottom, and sides of the circle.  So to
;; produce a circle with pixel radius p, the area of the BB is
;; diameter * diameter, since diameter is the length of a side of this
;; square.  In other words, the mark size is (* 4 radius radius).
;; 
;; We specify perceptual radius in units defined by the model.  To
;; translate those into the plot's units, calculate the proportion
;; of the width (or height) of the model world that the radius of
;; a perceptual circle takes up, and then multiply that ratio by
;; width of the plot to get the fraction of the plot width needed for
;; a mark radius.  Then use that number in (* 4 r r).
(defn foodspot-mark-size
  "Given the width or height of a square model field, the width or height 
  of a square Vega-Lite plot, and a perceptual radius in model units,
  returns the Vega-Lite mark size that will produce a perceptual radius
  circle that is visually correct in a Vega-Lite plot of that width.
  This value can be used as the value of the Hanami key :MSIZE."
  [env-sz plot-sz perc-radius]
  (let [perceptual-ratio (/ perc-radius env-sz)  ; part of env width subtended by radius
        pixel-ratio (* perceptual-ratio plot-sz)] ; part of figure width subtended by radius
    (long (* 4 pixel-ratio pixel-ratio)))) ; Vega-Lite confused by BigInt

(defn make-foodgrid
  "Make a sequence of vega-lite food records on coordinates spaced out every 
  sep integers, from -quandrant-width to quadrant-width, and from
  -quadrant-height to quadrant-height, excluding [0,0]."
  ([env-width env-height]
   (make-foodgrid 1 env-width env-height))
  ([sep env-width env-height]
   (println "make-foodgrid: sep, env-width, env-height:" sep env-width env-height) ; DEBUG
  (map make-foodspot 
       (f/centerless-rectangular-grid sep env-width env-height))))

(defn vega-foodgrid-plot
  [env-sz plot-dim food-distance perc-radius]
  (hc/xform ht/point-chart 
            :DATA (make-foodgrid food-distance env-sz env-sz) 
            :X "x"
            :Y "y"
            :COLOR "label"
            :MSIZE (foodspot-mark-size env-sz plot-dim perc-radius)
            :OPACITY 0.5  ; default is 0.7
            :WIDTH  plot-dim  ; sets dim for plot only, label area not included
            :HEIGHT plot-dim))

(defn vega-gridwalk-plot
  [perc-radius maxpathlen powerlaw-scale n-steps foodgrid-plot & walk-plots]
  (hc/xform
    ht/layer-chart
    :LAYER (cons foodgrid-plot walk-plots)
    :TITLE (str "perceptual radius = " perc-radius ";  "
                "max path len = " maxpathlen ";  "
                "scale = " powerlaw-scale ";  "
                "steps per path: " (vec n-steps))))
