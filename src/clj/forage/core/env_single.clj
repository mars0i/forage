;; Functions for minimal, fast environments containing a 
;; single foodspot, without toroidal lookup.
(ns forage.core.env-single
  (:require [ham-fisted.api :as hf]
            [ham-fisted.hlet :as hfl]
            [ham-fisted.primitive-invoke :as hfpi]
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
(defn make-single-foodspot-env
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


;; TODO Should I instead build the lookup into look-fn (as with env-mason)?
;; Then look-fn doesn't have to return perc-radius; it just returns a found
;; foodspot or nil.

;; Version with primitive invoke is slower?
#_
(defn make-look-fn
  [env ^double perc-radius]
  (fn [x0 y0 x1 y1]
    (hfl/let [[p q] (dbls env)
              [near-x near-y] (dbls (um/near-pt-on-seg x0 y0 x1 y1 p q))  ; _ (println "near-x:" near-x " near-y:" near-y) ; DEBUG
              ;; Has to be local?
              distance-2D-prim (hfpi/->ddddd (fn ^double [^double x0 ^double y0 ^double x1 ^double y1]
                                               (let [xdiff (- x0 x1)
                                                     ydiff (- y0 y1)]
                                                 (sqrt (+ (* xdiff xdiff) (* ydiff ydiff))))))
              distance (hfpi/ddddd distance-2D-prim near-x near-y p q)]  ; (println "distance:" distance) ; DEBUG
      (if (<= distance perc-radius)
        [[[p q]] [near-x near-y]]
        nil))))

(defn make-look-fn
  [env ^double perc-radius]
  (fn [x0 y0 x1 y1]
    (hfl/let [[p q] (dbls env)
              [near-x near-y] (dbls (um/near-pt-on-seg x0 y0 x1 y1 p q))  ; _ (println "near-x:" near-x " near-y:" near-y) ; DEBUG
              distance (um/distance-2D* near-x near-y p q)]  ; (println "distance:" distance) ; DEBUG
      (if (<= distance perc-radius)
        [[[p q]] [near-x near-y]]
        nil))))

(defn find-in-seg
  [look-fn _ x0 y0 x1 y1]
  (look-fn x0 y0 x1 y1))

;; env-mason look-fns take a pair of coordinates representing the current
;; location, so that Continuous2D can look for any targets in the nearest bucket.
;; In env_single, there is only one target no matter where you are, so
;; there's no need to pass in coordinates.  So make-look-fn returns a
;; function of no arguments that always returns the same target.
#_
(defn make-look-fn
  [env ^double perc-radius]
  (constantly (hf/double-array [perc-radius (env 0) (env 1)])))

;; Note that look-fn plays a different role here than in walks/find-in-seg, as it must.
;; Version with new ham-fisted destructuring let:
#_
(defn find-in-seg
  [look-fn _ x0 y0 x1 y1]
  (hfl/let [[perc-radius p q] (dbls (look-fn))  ; _ (println "\ntarget:" p q ", radius:" perc-radius) ; DEBUG
        [near-x near-y] (dbls (um/near-pt-on-seg x0 y0 x1 y1 p q))  ; _ (println "near-x:" near-x " near-y:" near-y) ; DEBUG
        ;; TODO: Wrap this in ham-fisted.primitive-invoke/->ddddd?
        distance (um/distance-2D* near-x near-y p q)]  ; (println "distance:" distance) ; DEBUG
    (if (<= distance perc-radius)
      [[[p q]] [near-x near-y]]
      nil)))


;; Note that look-fn plays a different role here than in walks/find-in-seg, as it must.
;; OLD VERSION without new ham-fisted destructuring let:
#_
(defn find-in-seg
  [look-fn _ x0 y0 x1 y1]
  (let [info (look-fn)
        perc-radius (hf/dnth info 0)
        p (hf/dnth info 1)
        q (hf/dnth info 2)
        ;_ (println "\ntarget:" p q ", radius:" perc-radius) ; DEBUG
        near-pt (um/near-pt-on-seg x0 y0 x1 y1 p q)
        near-x (hf/dnth near-pt 0)
        near-y (hf/dnth near-pt 1)
        ;_ (println "near-x:" near-x " near-y:" near-y) ; DEBUG
        distance (um/distance-2D* near-x near-y p q)]
    ; (println "distance:" distance) ; DEBUG
    (if (<= distance perc-radius)
      [[[p q]] [near-x near-y]]
      nil)))

