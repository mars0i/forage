;; Functions for placing foodspots (resources, "targets") in 2D 
;; (rectangular) spatial coordinates.
;; (Code s/b independent of MASON and plot libs (e.g. Hanami, Vega-Lite).)
(ns forage.core.food
  (:require [utils.math :as um]
            [utils.fractal :as uf]
            [utils.misc :as misc]))


(declare x-zero? y-zero? either-zero? both-zero?)

(defn shift-point
  "Shifts the point [x y] to the right by inc-x and up by inc-y (where
  these last two values may be negative)."
  [inc-x inc-y [x y]]
  [(+ x inc-x) (+ y inc-y)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRIDS

(defn linegrid
  "Make a sequence of pairs of coordinate pairs representing horizontal and
  vertical lines from neg to positive quandrant-width and quadrant-height,
  spaced every sep units."
  [sep env-width env-height]
  (let [xmax (inc (/ env-width 2))
        ymax (inc (/ env-height 2))
        neg-xmax (- xmax)
        neg-ymax (- ymax)
        neg-sep (- sep)
        horiz-lines (for [y (range neg-ymax ymax sep)] [[neg-xmax y] [xmax y]])
        vert-lines  (for [x (range neg-xmax xmax sep)] [[x neg-ymax] [x ymax]])]
    (concat horiz-lines vert-lines)))


(defn rectangular-grid
  "Make a sequence of coordinate pairs spaced out every sep integers of
  width env-width and height env-height, starting from left-offset and
  bottom-offset.  By default, does not include rightmost column of
  points, or topmost row of points (if set divides env-width and
  env-height without remainder).  If include-max-edges? is true, these
  extra points are included.  (The former case is appropriate for
  toroidal environments; the latter may be useful for non-toroidal
  environments or for an environment composed of multiple grids."
  ([sep env-width env-height]
   (rectangular-grid sep 0 0 env-width env-height)) ; origin at lower left
  ([sep left-offset bottom-offset env-width env-height]
   (rectangular-grid sep left-offset bottom-offset env-width env-height false))
  ([sep left-offset bottom-offset env-width env-height include-max-edges?]
   (let [rang (if include-max-edges? misc/irange range)]
     (for [x (rang left-offset   (+ left-offset env-width)    sep)
           y (rang bottom-offset (+ bottom-offset env-height) sep)]
       [x y]))))


;; DEPRECATED--old version used until 2/10/2023
;; It works correctly when the offsets are = 0, but does strange things
;; otherwise.  (What was I thinking?)
(defn rectangular-grid-old
  "Make a sequence of coordinate pairs spaced out every sep integers,
  from -quadrant-width to quadrant-width, and from -quadrant-height
  to quadrant-height, including [0,0]."
  ([env-width env-height]
   (rectangular-grid 1 env-width env-height))
  ([sep env-width env-height]
   (rectangular-grid sep 0 0 env-width env-height)) ; origin at lower left
  ([sep left-offset bottom-offset env-width env-height]
   (let [xmax (- env-width left-offset)    ; range doesn't go to env-width, env-height
         ymax (- env-height bottom-offset) ; so toroidal wrap won't double foodspots
         neg-xmax (- left-offset)
         neg-ymax (- bottom-offset)
         neg-sep (- sep)
         ne-pairs (for [x (range 0 xmax sep)  ; rest excludes 0,0 but
                        y (range 0 ymax sep)] ;  includes 0,1 and 1,0
                       [x y])
         se-pairs (remove y-zero?  ; remove top row of quadrant
                          (for [x (range 0 xmax sep)  ; 1, -1: axes in ne-pairs, sw-pairs
                                y (range 0 neg-ymax neg-sep)]
                               [x y]))
         nw-pairs (remove x-zero?  ; remove right column of quadrant
                          (for [x (range 0 neg-xmax neg-sep)
                                y (range 0 ymax sep)]  ; 1, -1: axes in ne-pairs, sw-pairs
                               [x y]))
         sw-pairs (remove either-zero? ; remove top and right cols of quadrant
                          (for [x (range 0 neg-xmax neg-sep)
                                y (range 0 neg-ymax neg-sep)]
                               [x y]))]
     (concat ne-pairs sw-pairs se-pairs nw-pairs))))

(comment
  (def grid1 (rectangular-grid-old  10 -20 -10 40 30))
  (def grid2 (rectangular-grid      10 -20 -10 40 30))
  (println (count grid1) (count grid2))
  (= (sort grid1) (sort grid2))

  (def grid1a (rectangular-grid-old  10 0 0 40 30))
  (def grid2a (rectangular-grid      10 0 0 40 30))
  (println (count grid1a) (count grid2a))
  (= (sort grid1a) (sort grid2a))

  (def grid1b (rectangular-grid-old  10 -10 -10 40 30))
  (def grid2b (rectangular-grid      10 -10 -10 40 30))
  (println (count grid1b) (count grid2b))
  (= (sort grid1b) (sort grid2b))

  (def grid1c (rectangular-grid-old  5 -100 500 400 30000))
  (def grid2c (rectangular-grid      5 -100 500 400 30000))
  (println (count grid1c) (count grid2c))
  (def grid2t (rectangular-grid      5 -100 500 400 30000 true))
  (println (count grid2c) (count grid2t))
  (+ (/ 400 5) (/ 30000 5))

  (def grid2b (rectangular-grid 10 -10 -10 40 30))
  (def grid2bt (rectangular-grid 10 -10 -10 40 30 true))
  (println (count grid2b) (count grid2bt))
  (+ (/ 40 5) (/ 30 5))
  (/ (+ 40 30) 5)
)


(comment
  ;; DELETE IF UNUSED:

  (defn divisible-by?
    [n d]
    (= n (* d (quot n d))))

  (defn filtered-pairs
    [include-pair?
     start-x end-x step-x
     start-y end-y step-y]
    (for [x (range start-x end-x step-x)
          y (range start-y end-y step-y)
          :when (include-pair? x y)]
      [x y]))

)

;; TODO: Add option to include right margins.  This is harder than it sounds.  See
;; https://stackoverflow.com/a/68476365/1455243 and definition of irange in utils/misc.clj.
(defn slide-grid
  "A slide-grid is the composition of two rectangular grids with spacing
  2*sep, offset from each other by shift-x and shift-y.  i.e. those parameters
  allow you to \"slide\" some of the points toward/away from others.  See
  rectangular-grid for other parameters.  Tips: It's probably best if
  shift-x and shift-y are nonnegative and <= sep; otherwise foodspots may
  go beyond the intended boundaries of env.  You can use
  viz.hanami/split-foodgrid to distinguish between shifted and unshifted
  foodspots.  If shift-x = shift-y = sep, the result is a rectangular
  grid with spacing sep."
  ([sep shift-x shift-y env-width env-height]
   (slide-grid sep shift-x shift-y 0 0 env-width env-height))
  ([sep shift-x shift-y left-offset bottom-offset env-width env-height]
   (slide-grid sep shift-x shift-y left-offset bottom-offset env-width env-height false))
  ([sep shift-x shift-y left-offset bottom-offset env-width env-height include-max-edges?]
   (when include-max-edges?
     (println "slide-grid: include-max-edges? is NOT YET IMPLEMENTED.")) ; because irange doesn't handle :when
   (let [unshifted-pairs (for [x (range 0 (/ env-width sep))
                               y (range 0 (/ env-height sep))
                               :when (even? (+ x y))]
                           [(+ left-offset   (* x sep))
                            (+ bottom-offset (* y sep))])
         shifted-pairs   (for [x (range 0 (/ env-width sep))
                               y (range 0 (/ env-height sep))
                               :when (odd? (+ x y))]
                           [(+ left-offset   (* x sep) shift-x)
                            (+ bottom-offset (* y sep) shift-y)])]
     (concat unshifted-pairs shifted-pairs))))


(comment
  ;; Small envs for direct viewing of coordinates:
  (def gridold  (rectangular-grid-old 10     0 0 40 60))
  (def gridnew  (rectangular-grid     10     0 0 40 60))
  (def sgridnew (slide-grid           10 0 0 0 0 40 60))
  
  (def grid0  (rectangular-grid-old 10 -10 -10 40 60))
  (def grid1  (rectangular-grid 10     -30 -20 40 60))
  (def sgrid1 (slide-grid       10 0 0 -30 -20 40 60))
  (def sgrid2 (slide-grid       10 10 10 -30 -20 40 60))
  (def sgrid3 (slide-grid       10 10 10 -30 -20 40 60 true))
 
  (def grid5 (rectangular-grid 10     0 0 30 40))
  (def sgrid5 (slide-grid      10 0 0 0 0 30 40))
  (def sgrid6 (slide-grid      10 -7 0 0 0 30 40))

  ;; Large envs that can be observed in plots:
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def grid1 (f/slide-grid 10 0 0 0 0 100 100))
  (def vgrid1 (map h/make-foodspot grid1))
  (def food1 (h/vega-food-plot vgrid1 100 400 1))
  (oz/view! food1)
  
  (def grid2 (slide-grid 10 8 0 0 0 100 100))
  (def vgrid2 (map h/make-foodspot grid2))
  (def food2 (h/vega-food-plot vgrid2 100 400 1))
  (oz/view! food2)
  (def vgrids2 (h/split-foodgrid grid2)) ; display shifted points with different color
  (def foods2 (h/vega-food-plot vgrids2 100 600 1))
  (oz/view! foods2)
)
                    
(defn remove-center
  "Remove the center point of a grid of size env-width x env-height.
  Dimensions must be even numbers."
  [env-width env-height grid]
  (remove (fn [[x y]] (and (= x (/ env-width 2))    ; assumes width, height 
                           (= y (/ env-height 2)))) ; are even
          grid))

(defn centerless-rectangular-grid
  "Make a sequence of coordinate pairs spaced out every sep integers,
  from -quandrant-width to quadrant-width, and from -quadrant-height
  to quadrant-height, excluding [0,0]."
  ([env-width env-height]
   (centerless-rectangular-grid 1 env-width env-height))
  ([sep env-width env-height]
   (remove-center env-width
                  env-height
                  (rectangular-grid sep env-width env-height))))

(defn perc-foodspot-coords-in-coll
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  with possible additional ones, or nil if there are none.  Performs a 
  linear search through all foodspots in collection coll."
  [foodspot-coords perc-radius coords]
  (some (fn [foodspot-coord]
            (if (<= (um/distance-2D coords foodspot-coord) perc-radius)
              foodspot-coord))
        foodspot-coords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RADIALLY ARRANGED FOODSPOTS

(defn radial-target-coords
  "Create coordinates for num-targets that are evenly spaced radially
  around the center of env (i.e. around [half-size, half-size]) at a
  distance nomin/denom X half-size."
  [env-size num-targets denom nomin]
  (let [half-size (double (/ env-size 2))
        distance-from-zero (* (/ nomin denom) half-size)
        first-zero-tgt [distance-from-zero 0] ; east from origin--will be shifted later
        directions (map (fn [n] (* um/pi2 (/ n num-targets)))
                        (rest (range num-targets))) ; don't divide by zero
        zero-targets (cons first-zero-tgt
                           (map (fn [dir] (um/rotate dir first-zero-tgt))
                                directions))
        targets (map (partial shift-point half-size half-size)
                     zero-targets)]
    targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC UTILITY FUNCTIONS

(defn x-zero?
  "Given a pair as argument, returns true if the first element of the pair
  is equal to 0."
  [[x y]]
  (= x 0))

(defn y-zero?
  "Given a pair as argument, returns true if the second element of the pair
  is equal to 0."
  [[x y]]
  (= y 0))

(defn either-zero?
  "Given a pair as argument, returns true if either element of the pair
  is equal to 0."
  [[x y]]
  (or (= x 0) (= y 0)))

(defn both-zero?
  "Given a pair as argument, returns true if both elements of the pair
  are equal to 0."
  [pair]
  (= pair [0 0]))

