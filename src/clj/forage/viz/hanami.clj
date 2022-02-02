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
  over the range (2*quadrant-size x 2*quandrant-size), with physical size 
  plot-dim x plot-dim.  If there are additional Vega-Lite specs
  to add, they can be entered in a map as an additional argument."
  [quadant-size plot-dim data & addl-kvs-map]
  (merge
     (-> (hc/xform ht/line-chart
                :DATA data
                :XSCALE {"domain" [(m/neg quadant-size) quadant-size]}
                :YSCALE {"domain" [(m/neg quadant-size) quadant-size]}
                :COLOR "label"
                :WIDTH  plot-dim
                :HEIGHT plot-dim)
         (assoc-in [:encoding :order :field] "ord") ; walk through lines in order not L-R
         ;(assoc-in [::mark :strokeWidth] 10)  ; line thickness BREAKS FN
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
  [[x y]]
  {"x" x, "y" y, "type" "food"})

(defn foodspot-mark-size2
  "Given a perceptual radius, generates the corresponding dimension for
  use as a Hanami/Vega-Lite mark size for a circle."
  [scale perc-radius]
  (println "foodspot-mark-size:" perc-radius (* m/pi perc-radius perc-radius))
  (* scale m/pi perc-radius perc-radius))

;; If I specify perc radius in units that specify quadrants (ticks),
;; and 2*quadrant is mapped to plot-dim (= width, height):
;; 
;; perc-rad/(2*quadrant-size) is the percentage of the whole width or
;; length that's the radius.
;; So multiply this by figure size to get the radius in pixels.
;; Then $\pi r^2$ is the dot area, which is what's supposed to be the
;; arg to :MSIZE.
;; 
;; i.e. mark size 
;; $= \pi r^2 = \pi$ (figure-size * (perc-rad / (2 quad-size))^2
;;
(defn foodspot-mark-size1
  [quadrant-sz figure-sz perc-radius]
  (let [env-sz (* 2 quadrant-sz)
        perceptual-ratio (/ perc-radius env-sz)
        pixel-ratio (nt/round (* perceptual-ratio figure-sz))]
    (println "foodspot-mark-size:" perc-radius quadrant-sz figure-sz perceptual-ratio pixel-ratio (* m/pi pixel-ratio pixel-ratio)) ; DEBUG
    (* m/pi pixel-ratio pixel-ratio)))

(defn foodspot-mark-size
  [quadrant-sz figure-sz perc-radius]
  (let [env-sz (* 2 quadrant-sz)
        perceptual-ratio (/ perc-radius env-sz)  ; part of env width subtended by radius
        pixel-ratio (* perceptual-ratio figure-sz)] ; part of figure width subtended by radius
    (println "foodspot-mark-size:" perc-radius quadrant-sz figure-sz
             perceptual-ratio pixel-ratio (* pixel-ratio pixel-ratio)) ; DEBUG
    (long (* 4 pixel-ratio pixel-ratio)))) ; Vega-Lite confused by BigInt

(defn foodspot-mark-size3
  [quadrant-sz plot-sz perc-radius]
  (let [env-sz (* 2 quadrant-sz)
        bbsz-model-units (* 4 perc-radius perc-radius)
        bbsz-fract-of-env-sz (/ bbsz-model-units env-sz)
        bbsz-fract-plot-sz (* bbsz-fract-of-env-sz plot-sz)]
    (println "foodspot-mark-size:" perc-radius quadrant-sz plot-sz
             bbsz-model-units bbsz-fract-of-env-sz bbsz-fract-plot-sz)
    (double bbsz-fract-plot-sz))) ; Vega-Lite confused by BigInts

(defn make-foodgrid
  "Make a sequence of vega-lite food records on coordinates spaced out every 
  sep integers, from -quandrant-width to quadrant-width, and from
  -quadrant-height to quadrant-height, excluding [0,0]."
  ([quadrant-width quadrant-height]
   (make-foodgrid 1 quadrant-width quadrant-height))
  ([sep quadrant-width quadrant-height]
  (map make-foodspot 
       (f/centerless-rectangular-grid sep quadrant-width quadrant-height))))

(defn vega-foodgrid-plot
  [quadrant-size plot-dim food-distance perc-radius]
  (hc/xform ht/point-chart 
            :DATA (make-foodgrid food-distance quadrant-size quadrant-size) 
            :X "x"
            :Y "y"
            :COLOR "type"
            :MSIZE (foodspot-mark-size quadrant-size plot-dim perc-radius)
            ;:MSIZE (foodspot-mark-size 60 perc-radius) ; FIXME number is not permanent
            :OPACITY 0.5  ; default is 0.7
            :WIDTH  plot-dim   ; dim for plot only; label area isn't included.
            :HEIGHT plot-dim))

(defn vega-gridwalk-plot
  [foodgrid-plot walk-plot perc-radius maxpathlen powerlaw-scale n-steps]
  (hc/xform
    ht/layer-chart
    :LAYER [foodgrid-plot walk-plot]
    :TITLE (str "perceptual radius = " perc-radius ";  "
                "max path len = " maxpathlen ";  "
                "scale = " powerlaw-scale ";  "
                "steps per path: " (vec n-steps))))
