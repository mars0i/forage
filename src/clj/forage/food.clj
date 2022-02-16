;; Functions for placing foodspots (resources, "targets") in 2D 
;; (rectangular) spatial coordinates.
;; (Code s/b independent of MASON and plot libs (e.g. Hanami, Vega-Lite).)
(ns forage.food
    (:require [utils.math :as m]
              [utils.random :as r]
              [clojure.math.numeric-tower :as nt]))


(declare x-zero? y-zero? either-zero? both-zero?)

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

;; FIXME Rewrite for origin-in-corner rather than in center
(defn rectangular-grid
  "Make a sequence of coordinate pairs spaced out every sep integers,
  from -quandrant-width to quadrant-width, and from -quadrant-height
  to quadrant-height, including [0,0]."
  ([env-width env-height]
   (rectangular-grid 1 env-width env-height))
  ([sep env-width env-height]
   (let [xmax (inc env-width)  ; inc: range should all the way to env-width 
         ymax (inc env-height) ;  below
         neg-xmax (- xmax)
         neg-ymax (- ymax)
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

(defn centerless-rectangular-grid
  "Make a sequence of coordinate pairs spaced out every sep integers,
  from -quandrant-width to quadrant-width, and from -quadrant-height
  to quadrant-height, excluding [0,0]."
  ([env-width env-height]
   (centerless-rectangular-grid 1 env-width env-height))
  ([sep env-width env-height]
   (remove (fn [[x y]] (and (= x (/ env-width 2))    ; assumes width, height 
                            (= y (/ env-height 2)))) ; are even
           (rectangular-grid sep env-width env-height))))

(defn perc-foodspot-coords-in-coll
  "Returns a sequence of foodspot coordinates within perc-radius of (x,y),
  with possible additional ones, or nil if there are none.  Performs a linear 
  search through all foodspots in collection coll."
  [foodspot-coords perc-radius coords]
  (some (fn [foodspot-coord]
            (if (<= (m/distance2D coords foodspot-coord) perc-radius)
              foodspot-coord))
        foodspot-coords))

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

