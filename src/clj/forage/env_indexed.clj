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
  "Creates a new environment in the form of a matrix.  If locs is
  provided, it should be a collection of triples consisting of two
  integers representing coordinates of a location in the environment,
  and a value to be placed at that location."
  ([size] (mx/new-matrix size size))
  ([size locs]
   (let [e (make-env size)]
     (doseq [[x y v] locs]
       (mx/mset! e x y v))
     e)))


(comment
  (mx/pm
    (make-env 5 [[1 1 1] [1 2 "Yow!"] [4 3 27]])
  )
)
