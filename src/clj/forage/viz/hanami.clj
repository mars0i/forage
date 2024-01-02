;; Functions for plotting things in 2D spatial coordinates using Hanami
;; and Vega-Lite, and sometimes Oz.
(ns forage.viz.hanami
  (:require [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [oz.core :as oz]
            [utils.hanami :as uh] ; replace if grid-chart becomes non-local
            [utils.toroidal :as tor]
            [utils.math :as m]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-mason :as env]
            ;[forage.core.env-matrix :as matrixenv]
            ;[forage.core.env]
            ))
;; Code below can explicitly refer either to env-mason or env-matrix
;; using the alias above, or to whichever one is globally selected
;; in env.clj via the alias below.  (This allows legacy functions to
;; refer to env-mason, but to allow other functions to use whichever
;; environment implementation is currently selected.)

;(ns-unalias *ns* 'env) ; allow redefining without restarting Clojure
;(alias 'env forage.core.env/env)

;; Note field names have to be strings, not keywords, in order
;; for vega-lite to make full use of them.

;; TODO Rationalize parameter lists.

;; TODO Several docstrings need more detail.


;; TODO Allow other color schemes.
(defn make-heatmap
  "Create a Vega-Lite heatmap of counts in bins, with extent [bin-min
  bin-max] in both dimensions, and step bin-step.  Plot dimensions are
  plot-dim x plot-dim.  Input data should be a sequence of maps with keys
  \"x\" and \"y\"."
  [plot-dim bin-min bin-max bin-step data]
  (hc/xform ht/heatmap-chart
            :DATA data
            :X "x"
            :Y "y"
            :XBIN {:extent [bin-min bin-max] :step bin-step}
            :YBIN {:extent [bin-min bin-max] :step bin-step}
            :COLOR {:aggregate "count"
                    :type "quantitative"
                    :scale {:scheme "yelloworangered"
                            :reverse false}} ; :reverse false is the default; including it to show that true is an option.
            :TOOLTIP {:aggregate "count"}
            ;:TOOLTIP ht/RMV ; if not using tooltips, include this to remove accidental Hanami tooltip
            :WIDTH  800
            :HEIGHT 800))

(defn vega-gridwalk-plot
  "Configure a Vega-Lite plot to be filled with search path(s) for foodspots 
  represented by circles with size equal to the perceptual radius.  (Doesn't put 
  any foodspots or paths in the plot; just defines the overall plot configuration.)"
  [perc-radius maxpathlen powerlaw-scale n-steps env-plot & walk-plots]
  (hc/xform
    ht/layer-chart
    :LAYER (cons env-plot walk-plots)
    :TITLE (str "perceptual radius = " perc-radius ";  "
                "max path len = " maxpathlen ";  "
                "scale = " powerlaw-scale ";  "
                "steps per path: " (vec n-steps))))

;; TODO let user add to the xform expression instead of Vega-Lite directly
(defn vega-walk-plot
  "Constructs a Vega-Lite random walk plot from (vega-lite-ified) data
  over the range (2*quadrant-sz x 2*quandrant-sz), with physical size
  plot-dim x plot-dim.  Lines will be plotted with thickness
  stroke-width, or 1.0 if stroke-width is falsey.  clip? is a boolean
  that determines whether lines that go beyond the boundaries are
  clipped at the boundary.  If :mark-color key and value is provided,
  the value will be treated as a Vega-Lite color string to be added as
  the value of :color in the :mark specification.  If :mark-color is not
  provided, but :color-scheme is, then the value of :colors-scheme will
  be treated as a Vega color scheme to be added in the value of :color
  in :encoding.  (This changes the automatic mapping of colors to
  different plot elements.)  If neither :mark-color nor :color-scheme is
  provided, then Hanami's default color scheme is used.  If any color
  scheme is used, and :color-field is provided, its value should be a
  string that specifies a field name in the data that will be used to
  choose colors for data elements.  If :color-field is not provided, the
  field \"label\" is used as the default."
  ([plot-dim data-dim stroke-width data]
   (vega-walk-plot plot-dim 0 data-dim stroke-width true data))
  ([plot-dim data-bound-min data-bound-max stroke-width clip? data
    & {:keys [mark-color color-scheme color-field]}]
   (-> (hc/xform ht/line-chart
                 :DATA data
                 :XSCALE {"domain" [data-bound-min data-bound-max]}
                 :YSCALE {"domain" [data-bound-min data-bound-max]}
                 :MCOLOR (or mark-color hc/RMV) ; RMV removes this Hanami field
                 :COLOR  (cond mark-color hc/RMV
                               color-scheme {:field (or color-field "label")
                                             :type "nominal"
                                             :scale {:scheme color-scheme}}
                               :else (or color-field "label"))
                 :WIDTH  plot-dim
                 :HEIGHT plot-dim)
       (assoc-in [:encoding :order :field] "ord") ; walk through lines in order not L-R
       (assoc-in [:encoding :order :type] "ordinal") ; gets rid of warning on :order
       (assoc-in [:mark :clip] (if clip? "true" "false"))
       (assoc-in [:mark :strokeWidth] (or stroke-width 1.0))
       (assoc-in [:mark :strokeCap] "round"))))

(defn add-point-labels
  "Given a sequence of pairs representing x,y coordinates, returns a
  Vega-Lite data specification in the form of a sequence of maps,
  where the coordinates are values for keys \"x\" and \"x\", and the 
  same \"label\" key and value is added to each map."
  [label xys]
  (map (fn [[x y]] {"x" x, "y" y, "label" label})
    xys))

(defn order-walk-with-labels
  "Given a sequence of pairs or triples where the first two elements
  represent x and y coordinates, returns a Vega-Lite data specification in
  the form of a sequence of maps, where the coordinates are values for keys
  \"x\" and \"x\".  A \"label\" key and value base-label is added to each
  map if the elements of the input sequence are pairs.  If the elements of
  the sequence are triples, the third element of each triple will be
  concatenated onto the string base-label using the str function.
  Regardless, each map will be given an \"ord\" key with increasing
  integers as values. This can be used with the Vega-Lite \"order\" key."
  [base-label xys]
  (map (fn [[x y & [suffix]] n]
         {"x" x, "y" y,
          "ord" n,
          "label" (str base-label suffix)})
       xys (range)))

;; Backward compatibility alias
(def add-walk-labels order-walk-with-labels)


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

;; Easy to use make-foodspot with map to add to coordinates.

(defn split-foodgrid
  "For testing: If a set of coordinates is divided into two
  equally-sized subsets, one concatenated onto the other (produced, for
  example, by forage.core.food/slide-grid), this formats the coordinates for
  Vega-Lite and adds different labels two the two subsets."
  [coords]
  (let [len (count coords)
        [unshifted shifted] (split-at (/ len 2) coords)
        vun (map (fn [[x y]] {"x" x, "y" y, "label" "food1"}) unshifted) 
        vsh (map (fn [[x y]] {"x" x, "y" y, "label" "food2"}) shifted)]
    (concat vun vsh)))

;; For the Hanami :MSIZE value:
;; Vega-Lite "mark" sizes--i.e. a circle at a point--are specified
;; by the *area of the bounding box around the circle*.  The BB's
;; edges touch the top, bottom, and sides of the circle.  So to
;; produce a circle with pixel radius p, the area of the BB is
;; diameter * diameter, since diameter is the length of a side of this
;; square.  In other words, the mark size is (* 4 radius radius).
;; 
;; We have specified perceptual radius in units defined by the model. 
;; To translate those into the plot's units, calculate the proportion
;; of the width (or height) of the model world that the radius of
;; a perceptual circle takes up.  Or rather calculate the proportion
;; of the distance from zero to the edge of the figure; otherwise 
;; it's a proportion of double the units in terms of which the radius
;; is defined.  Thus `perceptual-ratio` = `(/ perc-radius (/ env-sz 2))`.
;; Once perceptual ratio is calculated, that should then be multiplied
;; by width of the plot, to get the fraction of the plot width needed for
;; a mark radius.  Use that number in (* 4 r r).
(defn foodspot-mark-size
  "Given the width or height of a square model field, the width or height 
  of a square Vega-Lite plot, and a perceptual radius in model units,
  returns the Vega-Lite mark size that will produce a perceptual radius
  circle that is visually correct in a Vega-Lite plot of that width.
  This value can be used as the value of the Hanami key :MSIZE."
  [env-sz plot-sz perc-radius]
  (let [perceptual-ratio (/ perc-radius env-sz 2) ; part of env width subtended by radius
        pixel-ratio (* perceptual-ratio plot-sz)] ; part of figure width subtended by radius
    (long (* 4 pixel-ratio pixel-ratio)))) ; Vega-Lite confused by BigInts

(defn make-foodgrid
  "Make a sequence of vega-lite food records on coordinates spaced out every 
  sep integers, from -quadrant-width to quadrant-width, and from
  -quadrant-height to quadrant-height, excluding [0,0]."
  ([env-width env-height]
   (make-foodgrid 1 env-width env-height))
  ([sep env-width env-height]
  (map make-foodspot 
       (f/centerless-rectangular-grid sep env-width env-height))))

;; TODO Generalize the color specification as in vega-walk-plot
(defn vega-food-plot
  "Plot foodspot display radii where foodspots are.  If a fifth argument
  is passed, it will be interpreted as a colorscheme name string from 
  https://vega.github.io/vega/docs/schemes ."
  [foodspots env-sz plot-dim display-radius & colorscheme-seq]
  (hc/xform ht/point-chart 
            :DATA foodspots
            :X "x"
            :Y "y"
            :XSCALE {"domain" [0 env-sz]} ; Forces internal dimensions to this
            :YSCALE {"domain" [0 env-sz]} ; size, rather than min needed for data.
            :COLOR (if colorscheme-seq
                     {:field "label" :type "nominal"
                      :scale {:scheme (first colorscheme-seq)}}
                     "label")
            :MSIZE (foodspot-mark-size env-sz plot-dim display-radius)
            :OPACITY 0.5  ; default is 0.7
            :WIDTH  plot-dim  ; sets dim for plot only, label area not included
            :HEIGHT plot-dim))

(defn vega-foodgrid-plot
  "Plot foodspot display radii on a rectangular grid using make-foodgrid."
  [env-sz plot-dim food-distance display-radius]
  (vega-food-plot (make-foodgrid food-distance env-sz env-sz) 
                  env-sz plot-dim display-radius))

;; TODO: Do this right.  It's a kludge based on ignorance of Vega-Light.
(defn vega-linegrid-plot
  "Plots a grid of lines without foodspots."
  [env-sz plot-dim food-distance]
  (vega-food-plot (make-linegrid "" food-distance env-sz env-sz) 
                  env-sz plot-dim 0))


(defn vega-env-plot
  "Plot foodspot display radii on where foodspots from env are.
  plot-dim is the Vega-Lite dimension.  display-radius is the
  Vega-Lite size for foodspots in perc-radius units: a display-radius
  of 1 is the perc-radius of a forager."
  [env plot-dim display-radius & {:keys [extra-pts]}]
  (let [food (add-point-labels "food" (env/env-foodspot-coords env))
        food (if extra-pts (concat food extra-pts))]
    (vega-food-plot food
                    (env/env-size env)
                    plot-dim
                    display-radius)))

;; TODO add a nice header
(defn vega-envwalk-plot
  "Simple plot that plots whatever foodspots are in env and then plots
  foodwalks and their hypothetical extensions. plot-dim is the Vega-Lite
  dimension.  stroke-width is the Vega-Lite thickness of the foraging path.
  display-radius is the Vega-Lite size for foodspots in perc-radius units:
  a display-radius of 1 is the perc-radius of a forager. walk-stops is a
  sequence of 2D coordinates representing the foraging path.  If
  ':foodspots-on-top? truthy-val' is added, foodspots will be plotted on
  top of the walk paths; otherwise walk paths will be appear on top of
  foodspots. (Note that stroke-width uses Vega-Lite units, whereas
  display-radius uses units based on simulation values.)"
  [env plot-dim stroke-width display-radius walk-stops & {:keys [foodspots-on-top? env-plot]}]
  (let [env-plot (or env-plot (vega-env-plot env plot-dim display-radius))
        data-dim (env/env-size env)
        toroidal-walk (tor/toroidal-to-vega-lite "piece" (tor/wrap-path 0 data-dim walk-stops))
        walk-plot (vega-walk-plot plot-dim data-dim stroke-width toroidal-walk)
        plots (if foodspots-on-top?
                (list walk-plot env-plot)
                (list env-plot walk-plot))]
    (hc/xform
      ht/layer-chart
      :LAYER plots)))

(defn did-couldve-walk-plot
  "Given a single foodwalk consisting of a sequence of found foodspots,
  a food walk (a sequence of coordinates until the point at which food was 
  found), and a sequence of coordinates for the entire possible walk,
  creates a pair of vega-lite plots of size plot-dim x plot-dim for the
  foodwalk and its extension walk beyond the found food."
  [plot-dim data-dim stroke-width foodwalk]
  (let [[food did couldve] foodwalk]
    [(vega-walk-plot plot-dim data-dim stroke-width (add-walk-labels "could've" couldve)) ; may be empty if didn't find food
     (vega-walk-plot plot-dim data-dim stroke-width (add-walk-labels "did" did))]))

;; TODO add a nice header
(defn vega-didcould-envwalk-plot
  "Simple plot that plots whatever foodspots are in env and then plots
  foodwalks and their hypothetical extensions. (Note that
  stroke-width uses Vega-Lite units, whereas display-radius uses units
  based on simulation values.)"
  [env plot-dim stroke-width display-radius foodwalks]
  (let [env-plot (vega-env-plot env plot-dim display-radius)
        data-dim (env/env-size env)
        did-couldve-plots (mapcat
                            (partial did-couldve-walk-plot plot-dim data-dim stroke-width)
                           foodwalks)]
    (hc/xform
     ht/layer-chart
     :LAYER (cons env-plot did-couldve-plots))))

;; FIXME IS THERE A LEAK IN THIS?  I don't think there should be,
;; but seems like there might be.
;; 
;; SEE ALSO forage.core.run/write-found-coords.
;; 
;; Maybe ought to be merged with other looping functions above.
;; But maybe not: levy-experiments and straight-experiments are designed to
;; automate generation of a lot of data in a systematic way, while
;; write-foodwalk-plots is designed for exploratory work to understand how
;; things would or did work when produced systematically, and should allow
;; a variety of ways of generating the walk data (e.g. by using the same
;; walk in different envs, which is not usually what I'd want when generating
;; a lot of data).
(defn write-foodwalk-plots
  "Given a sequence of foodwalk triples (foodwalks), uses Hanami,
  Vega-Lite, and Oz to generate a series of graphic files containing
  grids/lattices of plots, one plot for each foodwalk.  Filenames are
  composed of stubname, the seed (could be an arbitrary string), and
  info about which walks (runs) from foodwalks are included in a
  particular graphics file.  params is a map of the kind used to
  specify parameters for walks (illustrated in forage/run.clj and many
  files in forage/experiment). mu and params are used only for labels
  or filename info. env is only needed to generate foodspot
  representations. total-runs specifies how many runs to plot.
  Currently uses oz/export! to plot, and file-type should be :svg (or
  :png, but that's currently broken in oz/export!). (Note that
  stroke-width uses Vega-Lite units, whereas display-radius uses units
  based on simulation values.)"
  [stubname file-type seed                                 ; filename parameters
   env plot-size runs-per-grid grid-columns stroke-width display-radius ; plot display parameters
   mu params                                               ; plot header label info
   foodwalks]                                              ; data 
  (let [total-runs (count foodwalks)
        basename (str stubname "seed" seed)
        basetitle (str "mu=" mu ", seed=" seed ", maxpathlen=trunclen=" (params :maxpathlen))]
    (doseq [plot-index (range 0 total-runs runs-per-grid)]
      (let [first-run-id (inc plot-index)
            last-run-id  (+ plot-index runs-per-grid)
            suffix (name file-type)
            filename (str basename "runs" first-run-id "thru" last-run-id "." suffix)
            title (str basetitle ", runs " first-run-id " through " last-run-id)]
        (print "Constructing" filename "... ") (flush)
        (-> (hc/xform
             uh/grid-chart
             :TITLE title
             :TOFFSET 10
             :COLUMNS grid-columns
             :CONCAT (mapv (partial vega-envwalk-plot env plot-size stroke-width display-radius)
                           (map vector 
                                (take runs-per-grid
                                      (drop plot-index foodwalks)))))
            (oz/export! filename))
        (println "written.") (flush)))))
