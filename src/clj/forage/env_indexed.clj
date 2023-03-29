;; Future home of replacement for env-mason.
(ns forage.env-indexed
  (:require [clojure.core.matrix :as mx]))

;; Strategy:
;; Foodspots exist in a 2D grid made with core.matrix using one of 
;; the pure Clojure matrix implementations.
;; A foodspot is represented by a special marker (maybe true, or a number).
;; The points around a foodspot, up to a radius in some integer-rounded 
;; coordinates, are marked with a collection containing points to the
;; foodspot it's a radius around.  This is the size of the perceptual
;; radius, but it's around the foodspots.

;; When a walk reaches a point, mod its coords it to get the indexes into 
;; the matrix and check wehther it's in the radius of the foodspot.


;(mx/set-current-implementation :persistent-vector)
(mx/set-current-implementation :ndarray)
;(mx/set-current-implementation :vectorz)

(defn make-env
  ([size] (mx/matrix 'fixme))
  ([size locs] 'fixme))
