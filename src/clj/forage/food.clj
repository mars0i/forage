;; Functions for placing foodspots (resources, "targets") in 
;; 2D (rectangular) spatial coordinates.  
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
  [sep quadrant-width quadrant-height]
  (let [xmax (inc quadrant-width)
        ymax (inc quadrant-height)
        neg-xmax (m/neg xmax)
        neg-ymax (m/neg ymax)
        neg-sep (m/neg sep)
        horiz-lines (for [y (range neg-ymax ymax sep)] [[neg-xmax y] [xmax y]])
        vert-lines  (for [x (range neg-xmax xmax sep)] [[x neg-ymax] [x ymax]])]
    (concat horiz-lines vert-lines)))

(defn rectangular-grid
  "Make a sequence of coordinate pairs spaced out every sep integers,
  from -quandrant-width to quadrant-width, and from -quadrant-height
  to quadrant-height, including [0,0]."
  ([quadrant-width quadrant-height]
   (rectangular-grid 1 quadrant-width quadrant-height))
  ([sep quadrant-width quadrant-height]
   (let [xmax (inc quadrant-width)
         ymax (inc quadrant-height)
         neg-xmax (m/neg xmax)
         neg-ymax (m/neg ymax)
         neg-sep (m/neg sep)
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
  ([quadrant-width quadrant-height]
   (centerless-rectangular-grid 1 quadrant-width quadrant-height))
  ([sep quadrant-width quadrant-height]
   (remove both-zero? 
           (rectangular-grid sep quadrant-width quadrant-height))))


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

