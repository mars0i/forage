;; Future home of replacement for env-mason.
(ns forage.env-indexed
  (:require [clojure.math :as math]
            [clojure.core.matrix :as mx]
            [utils.random :as r]
            [utils.misc :as um]))

;; Note perc-foodspot fns below have different signatures from similar functions in 
;; env_mason.clj because of differences in the approach.  In env-mason,
;; perceptual radius is associated with the forager, whereas here it's 
;; built into the env, and each foodspot.

;; TODO TODO:
;; SHOULD POINTER COORDS BE THE INTERNAL, SCALED COORDS, OR
;; THE EXTERNAL, UNSCALED COORDS?
;; ANSWER: The external unscaled ones.  And then always do the conversion
;; internally (as if it were OO).
;; So ... FIXME below.

;; Maybe make the env into a pair or map that contains the scale and maybe size
;; as well as the matrix.  But in that case, do not destructure the map
;; or whatever it is in the functions that are used to look for foodspots.
;; Or don't create the working versions of those using partial.  Make it
;; so that any destructuring only happens once during setup--not every
;; every time the system stops to look for foodspots.

;; TODO: Rename choose from mulitple foodspots function as "random".
;; An alternative function might assess which foodspot is closer and
;; choose that one.


;; Overall strategy:
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

;; NOTE there if I use regular Clojure vectors, I can use Clojure indexing:
;;   (def foo (mx/new-matrix :persistent-vector 4 4))
;;   ((foo 0) 0) ;=> 0.0
;; But I can't mset! the values, which is inconvenient: it means that I
;; have to create the entire environment at once.  If I want
;; persistent-vectors, though, I could create it as an ndarray and then
;; convert it.

(mx/set-current-implementation :ndarray) 
;; don't use this: (mx/set-current-implementation :persistent-vector)

;; core.matrix helper function
(defn mset-conj!
  "Sets element [x y] of core.matrix matrix m to a new value which is v
  conj'ed onto the old value of m at [x y]. The old value must be a
  collection."
  [m x y v]
  (mx/mset! m x y (conj 
                    (mx/mget m x y)
                    v)))


(def default-foodspot-val :food)

;; Note: Vega-lite and other coordinates I've used increase from lower
;; left to upper right.  core.matrix uses matrix-style coords from
;; upper left to lower right.  But that's OK.
;; I didn't need to think about that for the MASON envs in env_mason.clj.
;; It turns out that MASON in fact use the matrix-style rep.  From 
;; section 5.1.1 of the version 20 MASON manual:
;;      It’s also probably best to think of grids in traditional matrix
;;      format, that is, as having their origin in the top let corner,
;;      with the positive Y axis pointing down. This is because of how
;;      MASON displays them with its portrayals, which in turn is because
;;      Java’s graphics coordinate system is flipped in the Y axis and has
;;      <0, 0> at the top left corner.


;; TODO:
;; What if foodspot is near edge of env?
;; If non-toroidal, make sure there's not an index out of bounds error.
;; If toroidal, then need to mod the pointers over to the other side.

;; TODO ??  Maybe only place coordinates on the actual radius of the circle?

;; Values are conj'ed onto the existing value since there can be two
;; foodspots at one location, or more likely, foodspots with
;; overlapping perceptual radii.
;;
;; Algorithm for scale:
;; If scale is 1, then fill in every point s.t. distance from foodspot
;; is <= perc-radius.  i.e. sqrt(x^2 + y^2) <= r, or x^2 + y^2 <= r^2.
;; But suppose I want at least radius of 500.  Then if perc-radius=1,
;; r s/b 500.  If perc-radius=2, make r=1000.  So make r = perc-radius * scale.
;;
;; I put scale and perc-radius before env in the parameters below to make it
;; easier to define custom version of these functions for specific types
;; of models using partial.
(defn add-foodspot!
  "Add foodspot to env, with its perceptual radius perc-radius, at
  scale scale, around location [x y]. All cells within
  scale*perc-radius of (x y) will have the value [x y] conj'ed to
  indicate that they are within the perceptual radius of foodspot, and
  the center--the actual foodspot--will have the value passed as
  foodspot-val, or env-indexed/default-foodspot-val."
  ([scale perc-radius env x y]
   (add-foodspot! scale perc-radius env x y default-foodspot-val))
  ([scale perc-radius env x y foodspot-val]
   (mset-conj! env x y foodspot-val) ; mark foodspot location itself with a distinguishing value
   (let [actual-radius (* scale perc-radius)  ; add pointers to foodspot in surrounding cells
         radius-squared (* actual-radius actual-radius)]
     (doseq [off-x (um/irange (- actual-radius) actual-radius)
             off-y (um/irange (- actual-radius) actual-radius)
             :when (and 
                     (or (not= 0 off-x) (not= 0 off-y)) ; skip foodspot location itself
                     (> radius-squared (+ (* off-x off-x) (* off-y off-y))))] ; Every other point within radius needs foodspot coords
       (mset-conj! env (+ x off-x) (+ y off-y) [x y]))))) ; add the foodspot coords


(defn add-foodspots!
  [scale perc-radius env locs]
  (doseq [[x y] locs]
    (add-foodspot! scale perc-radius env x y)))


;; NOTE Because of the use of scale, this has to have a different
;; signature from the version in env_mason.clj.
;; 
;; By default new-matrix will initialize with zero doubles, I don't want that 
;; because it could be confusing.  Instead initialize with nils, which can
;; be conj'ed onto later.  Don't use [] for this purpose: It can confuse
;; core.matrix because it thinks the inner vectors are part of the matrix structure.
(defn make-env
  "Creates a new environment in the form of a matrix with cells initialized
  with nils.  The size of th4e matrix will be size X scale."
  ([scale size] 
   (let [size* (* scale size)
         env (mx/new-matrix :ndarray size* size*)] ;; Use ndarray internally so mset! works
     (doseq [x (range size*)
             y (range size*)]
       (mx/mset! env x y nil)) ; this default is assumed by other functions here.
     env))
  ([scale size perc-radius locs]
   (let [env (make-env scale size)]
     (add-foodspots! scale perc-radius env locs)
     env)))
     


;; Method used below doesn't try to find the foodspots themselves.  Their
;; coordinates are referenced many times, so we extract them into a set.
;; Perhaps this method will be too slow given the size of envs in models.
;; A more bespoke method might be needed (perhaps using mx/ereduce).
(defn env-foodspot-coords
  "Returns a sequence of the coordinates off all foodspots in environment
  env."
  [env]
  (seq (into #{}
             (comp cat (filter coll?)) ; Each elt is coll of pairs or single values--we want pairs.
             (mx/eseq env))))


;; Note that although a forager who moves continuously and always looks for
;; food can never reach a foodspot without first getting within perceptual
;; radius, the following functions look for that possibility, for the sake
;; of (a) random initial states, (b) foragers who don't look while in
;; motion, and (c) possible models of e.g. raptors who don't move across
;; the ground.

(defn perc-foodspots
  "Examines location [x y] in env. Returns a falsey value if no foodspot is
  within the perceptual radius of that position, or the coordinates of a
  foodspot that's perceptible that location.  These coordinates will be [x
  y] if there's a foodspot on that very location.  Otherwise a collection
  of all coordinates of all foodspots perceptible from that location will
  be returned."
  [env x y]
  (let [x-int (math/round x)
        y-int (math/round y)
        found (mx/mget env x-int y-int)] ; note mget accepts floats but floors them
    (and found   ; if nil, just return that
         (if (some (complement coll?) found)
           [[x y]]   ; This is a foodspot itself
           found)))) ; A sequence of one or more coordinate pairs


;; TODO add similar random selection in env_mason
(defn perc-foodspot-choose-randomly
  "Examines location [x y] in env. Returns a falsey value if no foodspot is
  within the perceptual radius of that position, or the coordinates of a
  foodspot that's perceptible that location.  These coordinates will be [x
  y] if there's a foodspot on that very location.  Otherwise there are one
  more or more foodspots perceptible from that location; a randomly chosen
  one of them will be returned."
  [rng env x y]
  (let [x-int (math/round x)
        y-int (math/round y)
        found (mx/mget env x-int y-int)] ; note mget accepts floats but floors them
    (and found   ; if nil, just return that
         (cond (some (complement coll?) found) [x y]  ; This is a foodspot itself
               (= 1 (count found))             (first found)  ; Within radius of a single foodspot
               :else                           (r/sample-from-coll rng found 1))))) ; Within radius of more, so randomly choose.


;; TODO: Maybe add version of perc-foodspot that chooses the closest one, and
;; only chooses randomly if there are mulitple equally close foodspots.


(comment

  mx/pm

  (def e (make-env 2 10))
  (add-foodspot! 2 3 e 5 5)
  (mx/pm e)

  (def e (make-env 5 50))
  (add-foodspots! 5 2 e [[30 30] [15 15]]) ; no overlap
  (mx/pm e)

  (def e (make-env 5 50))
  (add-foodspots! 5 2 e [[30 30] [30 20]]) ; a little bit of overlap
  (mx/pm e)

  (env-foodspot-coords e)

  (perc-foodspots e 30 40)
  (def rng (r/make-well19937))
  (perc-foodspot-choose-randomly rng e 12 12)

  (mx/mget e 4 5)
  (mx/mget e 10 10)
  (mx/mget e 10 11)


)
