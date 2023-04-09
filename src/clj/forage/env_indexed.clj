;; Future home of replacement for env-mason.
(ns forage.env-indexed
  (:require [clojure.core.matrix :as mx]
            [utils.misc :as um]))

;; Strategy:
;; Foodspots exist in a 2D grid made with core.matrix (or some other
;; arbitrary-content 2D structure) using one of the pure Clojure matrix implementations.
;; A foodspot is represented by a special marker (maybe true, or a number
;; representing nutritional value or an id).
;; The points around a foodspot, up to a radius in some integer-rounded 
;; coordinates, are marked with a collection containing points to the
;; foodspot it's a radius around.  This is the size of the perceptual
;; radius, but it's around the foodspots.

;; These should perhaps be marked with the coordinates of the foodspoot
;; that that can be recorded.

;; Alternatively, if all search paths are continuous in 2D, one might only fill in
;; the slots on the perimeter.

;; When a walk reaches a check point, mod, or better yet round its coords it to
;; get the indexes into the matrix and check whether it's in the radius
;; of the foodspot.  Since indexes are integers 0, 1, 2, ..., this could be done 
;; every sqrt(2) along the path, since that's the farthest distance between two
;; cells.  NO, it should be every 1, since that's the shortest distance
;; between two cells.
;; (Whereas with the MASON Continous2D system, I worried that one could
;; miss targets on the edge of the perc radius if you had check intervals
;; that were two large, here were are just letting the grid representation
;; decide what is close enough.  So in addition to it probably being faster
;; to look up targets this way, I don't need to check for them as often.


;; The representation should have a scale, so that e.g. we represent a
;; distance of 1 as 100 cells.  Otherwise, if a distance of 1 was the cell
;; dimension, then perceptual radius 1 would be represented by a Moore or
;; von Neumann neighborhood, which is not a good approximation of circle.

;; PROBLEM: What if perc-radii for two foodspots overlap?

;; NOTE there if I use regular Clojure vectors, I can use Clojure indexing:
;;   (def foo (mx/new-matrix :persistent-vector 4 4))
;;   ((foo 0) 0) ;=> 0.0
;; But I can't mset! the values, which is inconvenient: it means that I
;; have to create the entire environment at once.  If I want
;; persistent-vectors, though, I could create it as an ndarray and then
;; convert it.

(mx/set-current-implementation :ndarray) 
;(mx/set-current-implementation :vectorz)
;; Strategy is to use :ndarray internally so that it's easy to update
;; matrices, but then return the result in whatever implementation is
;; the default specified above.

(def default-center :food)

;; FIXME I think scale is being ignored.
;;
;; Giving the foodspot itself a distinguishing value is better for debugging.
;; Also for list of foodspots.
(defn add-foodspot
  "Add foodspot, with its perceptual radius perc-radius, at scale scale, at
  location (x y). All cells within perc-radius of (x y) will have the value
  [x y] to indicate that they are in the perceptual radius of foodspot."
  ([scale perc-radius env x y]
   (add-foodspot scale perc-radius env x y default-center))
  ([scale perc-radius env x y center-val]
  (let [radius-squared (* perc-radius perc-radius)]
    (doseq [off-x (um/irange (- scale) scale)
            off-y (um/irange (- scale) scale)
            :when (<= (+ (* off-x off-x) (* off-y off-y))
                      radius-squared)]
      (mx/mset! env (+ x off-x) (+ y off-y) 
                [x y]))) ; Every within radius contains spot coords
  (mx/mset! env x y center-val))) ; replace foodspot location itself with distinguishing value

(defn add-foodspots
  [scale perc-radius env locs]
  ;; FIXME
  )

;; By default new-matrix will initialize with zero doubles, I don't want that 
;; because it could be confusing.
(defn make-env
  "Creates a new environment in the form of a matrix with cells initialized
  with nils."
  ([size] 
   (let [env (mx/new-matrix :ndarray size size)] ;; Use ndarray internally so mset! works
     (doseq [x (range size)
             y (range size)]
       (mx/mset! env x y nil)) ; initialize with non-confusing default
     (mx/matrix env))) ; return as default implementation
  ([size scale perc-radius locs]
   (let [env (make-env size)]
     (add-foodspots scale perc-radius env locs)
     (mx/matrix env)))) ; return as default implementation


(def m (mx/new-matrix 4 4))


(comment
  (def e (make-env 100))
  (add-foodspot e 30 20 60 50)
  (mx/pm e)
)
