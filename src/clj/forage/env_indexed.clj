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
  "Sets element [rows-down columns-right] of core.matrix matrix mat to a
  new value which is value conj'ed onto the old value of mat at [rows-down
  columns-right]. The old value must be a collection.  Note that
  core.matrix uses tradition"
  [mat rows-down columns-right value]
  (mx/mset! mat rows-down columns-right
            (conj (mx/mget mat rows-down columns-right) value)))

(def default-foodspot-val :food)

;; TODO:
;; What if foodspot is near edge of env?
;; If non-toroidal, make sure there's not an index-out-of-bounds error.
;; If toroidal, then need to mod the pointers over to the other side.

;; TODO ??  Maybe only place coordinates on the actual radius of the circle?



;; TODO Currently, toroidal? has no effect.  Should it?  Or should
;; this distinction just be built into functions that work on the env
;; (as it is at present)?
;;
;; NOTE Because of the use of scale, this has to have a different
;; signature from the version in env_mason.clj.
;; 
;; By default new-matrix will initialize with zero doubles, I don't want that 
;; because it could be confusing.  Instead initialize with nils, which can
;; be conj'ed onto later.  Don't use [] for this purpose: It can confuse
;; core.matrix because it thinks the inner vectors are part of the matrix structure.
(defn make-env
  "Returns a new environment as a map containing a value of :size which is
  dimension of the external representation of a size X size field, a value
  of :scale, which specifies how much finer-grained the internal
  representation of the field is, and :locations, whose value is a (size *
  scale) X (size * scale) matrix which is the internal representation of
  the field.  There will also be a key and value for internal-size.  Note
  that coordinates in env range from [0,0] to [size,size].  All of the cells
  of the matrix will be initialized with nil."
  [size scale toroidal?] 
  (let [size* (* scale size)
        locations (mx/new-matrix :ndarray size* size*)] ;; Use ndarray internally so mset! works
    (doseq [row (range size*)
            col (range size*)]
      (mx/mset! locations row col nil)) ; this default is assumed by other functions here.
    {:size size, :scale scale, :toroidal? toroidal?, :locations locations}))

(defn scale-coord
  "Scale value x by the :scale in env.  (Not for inner loops.)"
  [env x]
  (* x (:scale env)))

(defn scale-xy
  "Scale pair [x y] by the :scale in env.  (Not for inner loops.)"
  [env [x y]]
  [(* x (:scale env)) (* y (:scale env))])

(defn toroidize-coord
  "If necessary map a coordinate to the other size of the field, on the
  assumption that coordinates in env range from [0,0] to [size,size]."
  [size coord]
  (cond (> coord size) (- coord size) ; if too far right/up, add diff to 0
        (neg? coord)   (+ size coord) ; if below left/bottom edge, subtract from high edge
        :else coord)) ; just return as is if already within boundaries

(comment
  (toroidize-coord 100 105)
  (toroidize-coord 100 25)
  (toroidize-coord 100 -20)
)

(defn toroidize-pair
  "If necessary, map a pair of coordinates to the other size of the field,
  on the assumption that coordinates in env range from [0,0] to
  [size,size]."
  [size [x y]]
  [(toroidize-coord size x) (toroidize-coord size y)])

(comment
  (toroidize-pair 100 [110 105])
  (toroidize-pair 100 [110 25])
  (toroidize-pair 100 [25 125])
  (toroidize-pair 100 [-20 -10])
)

(defn toroidize-pairs
  "For each coordinate pair in coord-pairs, if necessary, map a pair of
  coordinates to the other size of the field, on the assumption that
  coordinates in env range from [0,0] to [size,size]."
  [size coord-pairs]
  (map (partial toroidize-pair size) coord-pairs))


(defn within-bounds?
  [size [x y]]
  (and (>= x 0) (>= y 0) (< x size) (< y size)))

(defn remove-outside-pairs
  [size coord-pairs]
  (filter (partial within-bounds? size) coord-pairs))

(comment
  (remove-outside-pairs 100 [[-2 50] [40 -30] [200 50] [65 45] [0 100] [100 0]
                                   [125 0] [-50 150] [0 15] [25 75]])
  (remove-outside-pairs 100 [[-2 -50] [-40 -30] [200 150] [165 -45] [-10 101] [101 -1]
                                   [125 2003] [-50 150] [-17 215] [125 175]])
)


(defn circle-range
  "Returns a sequence of coordinates representing a filled circle, around
  [x y], of external size perc-radius, but scaled according to env's scale."
  [env perc-radius x y]
  (let [scale-it (partial * (:scale env))
        x* (scale-it x)
        y* (scale-it y)
        radius* (scale-it perc-radius)  ; add pointers to foodspot in surrounding cells
        radius*-squared (* radius* radius*)]
    (for [off-x (um/irange (- radius*) radius*)
          off-y (um/irange (- radius*) radius*)
          :when (>= radius*-squared (+ (* off-x off-x) (* off-y off-y)))] ; ignore outside circle
      [(+ x* off-x) (+ y* off-y)])))


(defn make-toroidal-circle-range 
  "Returns a sequence of coordinates representing a filled circle, around
  [x y], of external size perc-radius, scaled according to env's scale, and
  wrapped toroidally if necessary."
  [env perc-radius x y]
  (toroidize-pairs (* (:size env) (:scale env))
                   (circle-range env perc-radius x y)))

(defn make-toroidal-donut
  "Does the same thing as make-toroidal-circle-range, but removes the
  coordinates for the center point."
  [env perc-radius x y]
  (let [x* (scale-coord env x)
        y* (scale-coord env y)]
    (remove (partial = [x* y*])  ; will fail if numbers are not the same type
            (make-toroidal-circle-range env perc-radius x y))))

(defn make-trimmed-circle-range 
  "Returns a sequence of coordinates representing a filled circle, around
  [x y], of external size perc-radius, scaled according to env's scale, and
  trimmed to the dimensions of env if necessary: points not falling within
  the environment are not included."
  [env perc-radius x y]
  (remove-outside-pairs (* (:size env) (:scale env))
                        (circle-range env perc-radius x y)))

(defn make-trimmed-donut
  "Does the same thing as make-trimmed-circle-range, but removes the
  coordinates for the center point."
  [env perc-radius x y]
  (let [x* (scale-coord env x)
        y* (scale-coord env y)]
    (remove (partial = [x* y*])  ; will fail if numbers are not the same type
            (make-trimmed-circle-range env perc-radius x y))))

(comment
  (remove (partial = [4 5]) [[1 2] [3 4] [4 5] [6 7]])
  (def e (make-env 5 1 false))
  (mx/pm (:locations e))
  (mx/shape (:locations e))
  (circle-range e 1 2 3)
  (make-trimmed-circle-range e 1 2 3)
  (make-trimmed-donut e 1 2 3)
)

(defn add-toroidal-foodspot!
  "Add foodspot to env, with its perceptual radius perc-radius, at scale
  scale, around location [x y], but wrapped toroidally if necessary. All
  cells within scale*perc-radius of (x y) will have the value [x y] conj'ed
  to indicate that they are within the perceptual radius of foodspot, and
  the center--the actual foodspot--will have the value passed as
  foodspot-val, or env-indexed/default-foodspot-val."
  ([env perc-radius x y]
   (add-toroidal-foodspot! env perc-radius x y default-foodspot-val))
  ([env perc-radius x y foodspot-val]
   (let [locs (:locations env)]
     ;; NOTE reversed order of x and y because the internal
     ;; representation uses traditional matrix indexing:
     (doseq [[x* y*] (make-toroidal-donut env perc-radius x y)] ; donut, i.e. leave out center
       (mset-conj! locs y* x* [x y])) ; i.e. we set each cell to pair that points to the center
     (mset-conj! locs (scale-coord env y) (scale-coord env x) foodspot-val)))) ; center gets special value

(defn add-toroidal-foodspots!
  "Add multiple foodspots at coordinate pairs in locs, to env with
  perceptual radius perc-radius. See add-toroidal-foodspot! for further
  details."
  [env perc-radius locs]
  (doseq [[x y] locs]
    (add-toroidal-foodspot! env perc-radius x y)))

(defn add-trimmed-foodspot!
  "Add foodspot to env, with its perceptual radius perc-radius, at scale
  scale, around location [x y], but trimmed to fit within the environment:
  points within the radius that are outside the boundaries of the envirment
  won't be included. All cells within scale*perc-radius of (x y) and within
  the environment boundaries will have the value [x y] conj'ed to indicate
  that they are within the perceptual radius of foodspot, and the
  center--the actual foodspot--will have the value passed as foodspot-val,
  or env-indexed/default-foodspot-val."
  ([env perc-radius x y]
   (add-trimmed-foodspot! env perc-radius x y default-foodspot-val))
  ([env perc-radius x y foodspot-val]
   (let [locs (:locations env)]
     ;; NOTE reversed order of x and y because the internal
     ;; representation uses traditional matrix indexing:
     (doseq [[x* y*] (make-trimmed-donut env perc-radius x y)] ; donut, i.e. leave out center
       (mset-conj! locs y* x* [x y])) ; i.e. we set each cell to pair that points to the center
     (mset-conj! locs (scale-coord env y) (scale-coord env x) foodspot-val)))) ; center gets special value

(defn add-trimmed-foodspots!
  "Add multiple foodspots at coordinate pairs in locs, to env with
  perceptual radius perc-radius. See add-trimmed-foodspot! for further
  details."
  [env perc-radius locs]
  (doseq [[x y] locs]
    (add-trimmed-foodspot! env perc-radius x y)))


(defn older-add-foodspot!
  ([env perc-radius x y]
   (older-add-foodspot! env perc-radius x y default-foodspot-val))
  ([env perc-radius x y foodspot-val]
   (let [raw-scaled (partial * (:scale env))
         scaled (if (:toroidal? env)
                  (fn [x] (raw-scaled x)) ; FIXME add mod or rem
                  raw-scaled)
         size* (scaled (:size env))
         radius* (scaled perc-radius)  ; add pointers to foodspot in surrounding cells
         radius*-squared (* radius* radius*)
         locs (:locations env)]
     ;; NOTE reversed order of x and y because the internal
     ;; representation uses traditional matrix indexing:
     (mset-conj! locs (scaled y) (scaled x) foodspot-val) ; mark foodspot location itself with a distinguishing value
     (doseq [off-x (um/irange (- radius*) radius*)
             off-y (um/irange (- radius*) radius*)
             :when (and 
                     (or (not= 0 off-x) (not= 0 off-y)) ; skip foodspot location itself
                     (> radius*-squared (+ (* off-x off-x) (* off-y off-y))))] ; Every other point within radius needs foodspot coords
       (mset-conj! locs                 ; add the foodspot coords
                   (+ off-x (scaled y))
                   (+ off-y (scaled x))
                   [x y])))))



;; TODO: Add clipping for toroidal? = falsey, wrapping for toroidal? = truthy
(defn get-xy
  "Returns whatever is at (external, integer) coordinates x, y in
  environment e. Notes: Can't be used to look at intervening
  micro-locations. AVOID in inner loops, to avoid repeated destructuring."
  [env x y]
  (let [scale (:scale env)]
    ;; NOTE reversed order of x and y because the internal
    ;; representation uses traditional matrix indexing:
    (mx/mget (:locations env) (* y scale) (* x scale)))) 


(comment
  (use 'clojure.repl) ; for pst

  (def scale 4)

  (def e1 (make-env 5 scale false))
  (mx/shape (:locations e1))
  (mx/pm (:locations e1))
  (older-add-foodspot! e1 2 2 3)
  (get-xy e1 2 3)

  (def e3 (make-env 5 scale false))
  (mx/shape (:locations e3))
  (mx/pm (:locations e3))
  (add-trimmed-foodspot! e3 2 2 3)
  (circle-range e3 3 2 3)
  (make-trimmed-circle-range e3 3 2 3)
  (make-trimmed-donut e3 3 2 3)
  (get-xy e3 2 3)

  (pst)

  (make-trimmed-donut e2 1 3 5) 
  (make-trimmed-circle-range e2 1 3 5) 
  (circle-range e2 1 3 5) 
  (make-toroidal-donut e2 1 3 5) 

  (def e2 (make-env 6 scale false))
  (mx/pm (:locations e2))
  ;; broken:
  (old-add-foodspot! e2 1 3 5)



  (= (mx/matrix :persistent-vector e1) (mx/matrix :persistent-vector e2))

  (mx/pm (:locations e1) {:formatter (fn [x] (if x (str x) "(-----)"))})
  (mx/mget (:locations e) (* scale 3) (* scale 3))

  (def yo (mx/matrix :persistent-vector e))

  (env-foodspot-coords e)
)


;; FIXME needs to be un-scaled
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

