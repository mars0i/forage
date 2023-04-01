;; Future home of replacement for env-mason.
(ns forage.env-indexed
  (:require [clojure.core.matrix :as mx]
            [utils.misc :as um]))

;; Strategy:
;; Foodspots exist in a 2D grid made with core.matrix using one of 
;; the pure Clojure matrix implementations.
;; A foodspot is represented by a special marker (maybe true, or a number).
;; The points around a foodspot, up to a radius in some integer-rounded 
;; coordinates, are marked with a collection containing points to the
;; foodspot it's a radius around.  This is the size of the perceptual
;; radius, but it's around the foodspots.

;; These should perhaps be marked with the coordinates of the foodspoot
;; that that can be recorded.

;; Alternatively, if all paths are continuous in 2D, one might only fill in
;; the slots on the perimeter.

;; When a walk reaches a point, mod or better yet round its coords it to
;; get the indexes into the matrix and check whether it's in the radius
;; of the foodspot.

;; The representation should have a scale, so that e.g. we represent a
;; distance of 1 as 100 cells.  Otherwise, if a distance of 1 was the cell
;; dimension, then perceptual radius 1 would be represented by a Moore or
;; von Neumann neighborhood, which is not a good approximation of circle.

;; PROBLEM: What is perc-radii for two foodspots overlap?


; (mx/set-current-implementation :persistent-vector)  ; doesn't allow mset!
(mx/set-current-implementation :ndarray) 
;(mx/set-current-implementation :vectorz)

;(def scale 1000)

(defn make-env
  "Creates a new environment in the form of a matrix with cells initialized
  with nils."
  [size] 
  (let [env (mx/new-matrix size size)]
    (doseq [x (range size)
            y (range size)]
      (mx/mset! env x y nil))
    e))


(defn add-foodspot!
  "Add foodspot, with its perceptual radius perc-radius, at scale scale, at
  location (x y). All cells within perc-radius of (x y) will have the value
  [x y]."
  [env scale x y perc-radius]
  (let [radius-squared (* perc-radius perc-radius)]
    (doseq [off-x (um/irange (- scale) scale)
            off-y (um/irange (- scale) scale)
            :when (<= (+ (* off-x off-x) (* off-y off-y))
                      radius-squared)]
      (mx/mset! env (+ x off-x) (+ y off-y) [x y])))) ; Every within radius contains spot coords, including itself


(comment
  (def e (make-env 10))
  (class e)
  (mx/pm e)
  (mx/mset! e 1 2 15)
  (add-foodspot! e 1 4 4 3)


)
