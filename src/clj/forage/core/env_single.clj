;; Functions for minimal, fast environments containing a 
;; single foodspot, without toroidal lookup.
;; The search process runs globally for each segment, and doesn't
;; step through the segment as in env-mason.
(ns forage.core.env-single
  (:require [ham-fisted.api :as hf]
            [ham-fisted.hlet :as hfl]
            [ham-fisted.primitive-invoke :as hfp]
            [utils.math :as um]
            [clojure.math :as math :refer [sqrt]]
            [fastmath.core :as fm]
            [forage.core.food :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(fm/use-primitive-operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT THAT CONSISTS OF A BARE COORDINATE PAIR

;; TODO consider replacing the Clojure vector with a Java array
;; if this would improve performance.

;; NOTE Since no size, doesn't support toroidal lookup.
(defn make-env
  "Make an environment that consists of a single foodspog, and only a
  single point.  The environment returned is is simply a pair [x y] of
  coordinates for that foodspot."
  [x y]
  [x y])

(defn perc-single-foodspot
  "Returns a collection containing the sole foodspot if it's within
  perc-radius of forager-coords (x, y), or nil if not.
  This is designed to work *only* with environments that are nothing more
  than a pair representing a single foodspot, as created by
  make-single-food-spot-env. If the env has only a single foodspot,
  but it's in a collection, use perc-mulitple-foodspots instead.
  Example usage:
     (def look-fn (partial env (params :perc-radius)))"
  [env ^double perc-radius ^double x ^double y] 
  (if (<= (um/distance-2D* x y (double (env 0)) (double (env 1))) perc-radius)
  ;(if (<= (um/distance-2D* x y (env 0) (env 1)) perc-radius)
    [env]
    nil))

(def foodspot-coords 
  "Extracts the coordinates from a foodspot. (Since in this env, foodspots
  are coordinate pairs, this is simply the identity function.)"
  identity)

(defn env-foodspots
  "Returns a collection containing the sole foodspot in environment env, or
  nil if there are none."
  [env]
  [env])

(defn env-foodspot-coords
  "Returns a collection containing the sole coordinate pair of a foodspot
  in environment env, or nil if there are none."
  [env]
  [env])

;(defn- lt [^double l ^double r] (< l r))
;(defn- gt [^double l ^double r] (> l r))

(defn within-interval
  "Returns true iff x is in the closed interval [a, b]. a must be <= b. All
  scalars should be doubles."
  [a b x]
  (<= ^double a ^double x ^double b))

;; Basic idea: near-pt-on-seg and distance-2D* are a little bit expensive.
;; But a segment can't possibly be near a foodspot if:
;;   - no endpoint is within perc radius of the foodspot AND
;;   - the foodspot is not inside [x0,x1] or [y0,1y].
;; Those are cheaper to test, so test them first.
;; As a heuristic, just checks that the foodspot is not inside [x0-radius, x1+radius]
;; or [y0-radius, y1+radius], assuming x0<x1 and y0<y1.  When the segment
;; isn't horizontal or vertical, there will be a small region that should
;; not get the expensive tests but will anyway--i.e. the region in the
;; square of size radiusXradius that is beyond radius from the endpoint.
(defn new-make-look-fn
  "Returns a function that accepts x, y coordinates from two points
  representing a line segment.  The returned function will checks to see
  whether env's sole foodspot is within perc-radius of the line at any
  point.  If so, returns a pair containing the coordinates of the foodspot
  as a pair, and the coordinates of point on the line segment that is
  closest to the foodspot, also as a pair.  (Note that the point
  returned--the one that is closest to the foodspot--is not, in general,
  the first point at which the foodspot could have been perceived; it's not
  where line segment crosses within perc-radius of the foodspot.  If
  perc-radius is large, the returned point might be some distance away from
  the point at which perception would have been possible.  It's as if the
  forager only has narrowly focused eyes on the side of its head, and only
  sees perpendicularly, unless it steps on a foodspot.)"
  [env ^double perc-radius]
  (fn [^double x0 ^double y0 ^double x1 ^double y1]
    (hfl/let [[x-low x-high] (if (< x0 x1)
                               [(- x0 perc-radius) (+ x1 perc-radius)]
                               [(- x1 perc-radius) (+ x0 perc-radius)])
              [y-low y-high] (if (< y0 y1)
                               [(- y0 perc-radius) (+ y1 perc-radius)]
                               [(- y1 perc-radius) (+ y0 perc-radius)])
              [p q] (dbls env)]
      (if (or (within-interval x-low x-high p)
              (within-interval y-low y-high q))
        (hfl/let [[near-x near-y] (dbls (um/near-pt-on-seg x0 y0 x1 y1 p q))
                  distance (um/distance-2D* near-x near-y p q)]
          (if (<= distance perc-radius)
            [[[p q]] [near-x near-y]]
            nil))
        nil))))

(defn make-look-fn
  "Returns a function that accepts x, y coordinates from two points
  representing a line segment.  The returned function will checks to see
  whether env's sole foodspot is within perc-radius of t:qa
  he line at any
  point.  If so, returns a pair containing the coordinates of the foodspot
  as a pair, and the coordinates of point on the line segment that is
  closest to the foodspot, also as a pair.  (Note that the point
  returned--the one that is closest to the foodspot--is not, in general,
  the first point at which the foodspot could have been perceived; it's not
  where line segment crosses within perc-radius of the foodspot.  If
  perc-radius is large, the returned point might be some distance away from
  the point at which perception would have been possible.  It's as if the
  forager only has narrowly focused eyes on the side of its head, and only
  sees perpendicularly, unless it steps on a foodspot.)"
  [env ^double perc-radius]
  (fn [x0 y0 x1 y1]
    (hfl/let [[p q] (dbls env)
              [near-x near-y] (dbls (um/near-pt-on-seg x0 y0 x1 y1 p q))
              distance (um/distance-2D* near-x near-y p q)]
      (if (<= distance perc-radius)
        [[[p q]] [near-x near-y]]
        nil))))

;; Use ham-fisted's primitive invoke?  No, can't because look-fn's
;; return value is too complex.
(defn find-in-seg
  "Applies look-fn to x0 y0 x1 y1, ignoring the first argument."
  [look-fn _ x0 y0 x1 y1]
  (look-fn x0 y0 x1 y1))

