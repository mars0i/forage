;; Replacement for env-mason.
(ns forage.core.env-matrix
  (:require [clojure.math :as math]
            [clojure.set :as sets]
	    [clojure.core.matrix :as mx]
            [clojure.core.matrix.selection :as mxsel] ; name of namespace changed between 0.44 and 0.63
	    [utils.random :as r]
            [utils.misc :as um]))

;; NOTE ENV-MASON treats perceptual radius as a property of the
;; forager, so it's not represented in the env.  To get the perceptual
;; radius into the look-fn, it has to be supplied from an independent
;; source from the env.
;; By contrast, ENV-MATRIX treats perceptual radii as properties of
;; foodspots, so they are specified in the env, and look-fns have
;; to get them from the env (in effect).


;; NOTE When this was env_indexed.clj, the goal was to provide a (kind of)
;; drop-in replacement for env_mason.clj.  That meant converting between
;; external coordinates corresponding to the ones used by env-mason, and
;; internal coordinates in matrices that gave a more fined-grained resolution.
;; I decided that keeping track of the conversion between these two scales
;; was more trouble than it was worth.  So I'm instead going to just change
;; the scale in new experiments that use this namespace.  Those experiments
;; will be in forage/matruns rather than forage/experiment, to distinguish
;; them.

;; TODO Experiment using fastmath to speed this up.


(mx/set-current-implementation :ndarray) 
;; don't use this: (mx/set-current-implementation :persistent-vector)

;; core.matrix helper function
(defn mset-conj!
  "Sets element [rows-down columns-right] of core.matrix matrix mat to a
  new value which is value conj'ed onto the old value of mat at [rows-down
  columns-right]. The old value must be a collection.  (Note core.matrix
  uses tradition matrix indexing with 0,0 indicating the upper left
  corner.)"
  [mat rows-down columns-right value]
  (mx/mset! mat rows-down columns-right
            (conj (mx/mget mat rows-down columns-right) value)))

;; TODO should order of results be swapped?
(defn filter-coords
  "Returns the coordinate pairs for elements in matrix mat for which
  pred returns truthy."
  [mat pred]
  (let [[h w] (mx/shape mat)]
    (for [x (range w)
          y (range h)
          :when (pred (mx/mget mat x y))]
      [x y])))


(def default-foodspot-val :food)

; Must work with conj and possibly other things:
;(def env-loc-initializer nil)
(def env-loc-initializer #{})  ; NOTE Some functions below may assume this, e.g. env-foodspot-coords
;; nil was confusing core.matrix/pm because it does a conversion to Clojure
;; vectors, and then does a test that uses count to figure out how many
;; dimensions the matrix/array has.  This causes pm to fail iff there's 
;; non-nil sequence in the first column.  Using sets prevents this.
;; See issue I submitted https://github.com/mikera/core.matrix/issues/361
;; But I could go back to nils if I stop using pm.


(defrecord MatEnv [size locs])

(comment
  (def me1 (->MatEnv 2 [[#{} #{}][#{} #{}]]))
  (def me2 (MatEnv. 2 [[#{} #{}][#{} #{}]]))
  (class me1)
  (class me2)
)

;; By default mx/new-matrix initializes with zero doubles.  I don't want that 
;; because it could be confusing.  Instead initialize with empty sets,
;; which can be conj'ed onto later.  (Don't use nil or [] for this purpose: It can 
;; confuse core.matrix/pm when there is added data in the first column of the matrix.)
(defn make-env
  "Returns a new environment as a map containing a value of :size which is
  dimension size X size matrix and :locs, whose value is the matrix.
  that coordinates in env range from [0,0] to [size-1,size-1].  All of the
  cells of the matrix will be initialized with env-loc-initializer."
  [size]
  (let [locations (mx/new-matrix :ndarray size size)] ;; Use ndarray internally so mset! works
    (doseq [row (range size)  ; note zero-based
            col (range size)]
      (mx/mset! locations row col env-loc-initializer)) ; this default is assumed by other functions here.
    (->MatEnv size locations)))

(defn env-size
  "Return the width (= height) of env."
  [env]
  (:size env))

(comment
  (def e (make-env 5))
  (mx/pm (:locs e))
  (mx/shape (:locs e))
  (mx/mget (:locs e) 4 4)

)

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

(defn trim-outside-pairs
  [size coord-pairs]
  (filter (partial within-bounds? size) coord-pairs))

(comment
  (trim-outside-pairs 100 [[-2 50] [40 -30] [200 50] [65 45] [0 100] [100 0]
                                   [125 0] [-50 150] [0 15] [25 75]])
  (trim-outside-pairs 100 [[-2 -50] [-40 -30] [200 150] [165 -45] [-10 101] [101 -1]
                                   [125 2003] [-50 150] [-17 215] [125 175]])
)

(defn circle-range
  "Returns a sequence of coordinates representing a filled circle around
  [x y] of size radius.  (radius is rounded to the nearest integer before
  being squared.)"
  [radius x y]
  (let [round-x (math/round x) ; round returns longs
        round-y (math/round y)
        ceil-radius  (long (math/ceil radius))  ; will be clipped by radius-squared anyway
        radius-squared (* radius radius)] ; We use the precise, maybe double vals for clipping.
    (for [off-x (um/irange (- ceil-radius) ceil-radius)
          off-y (um/irange (- ceil-radius) ceil-radius)
          :when (>= radius-squared (+ (* off-x off-x) (* off-y off-y)))] ; ignore outside circle
      [(+ round-x off-x) (+ round-y off-y)])))

(comment 
  (= (int 25) (long 25))
  (= (int 25) (float 25))
  (= (float 25) (double 25))
  (== (int 25) (long 25))
  (== (int 25) (float 25))
  (== (float 25) (double 25))
)

(defn make-toroidal-circle-range 
  "Returns a sequence of coordinates representing a filled circle, around
  [x y], of external size perc-radius, wrapped toroidally if necessary."
  [env perc-radius x y]
  (toroidize-pairs (:size env) (circle-range perc-radius x y)))

(defn make-toroidal-donut
  "Does the same thing as make-toroidal-circle-range, but removes the
  coordinates for the center point."
  [env radius x y]
  (let [round-x (math/round x)
        round-y (math/round y)]
    (remove (partial = [round-x round-y]) ; circle-range will have rounded.
            (make-toroidal-circle-range env radius x y)))) ; use raw x, y for radius calculation

(defn make-trimmed-circle-range 
  "Returns a sequence of coordinates representing a filled circle, around
  [x y], of external size perc-radius, trimmed to the dimensions of env if
  necessary: points not falling within the environment are not included."
  [env radius x y]
  (trim-outside-pairs (:size env) (circle-range env radius x y)))

(defn make-trimmed-donut
  "Does the same thing as make-trimmed-circle-range, but removes the
  coordinates for the center point."
  [env radius x y]
  (remove (partial = [x y])  ; will fail if numbers are not the same type
          (make-trimmed-circle-range env radius x y)))

(comment
  (remove (partial = [4 5]) [[1 2] [3 4] [4 5] [6 7]])
  (def e (make-env 5 1 false))
  (mx/pm (:locs e))
  (mx/shape (:locs e))
  (circle-range e 1 2 3)
  (make-trimmed-circle-range e 1 2 3)
  (make-trimmed-donut e 1 2 3)
)

;; FIXME BUG: For all of the foodspot adders, I think:
;; It ought to be possible to specific non-integer coordinates
;; for foodspots, and have them translated into internal integer
;; coordinates (after rounding).  However, this just slaps the provided
;; coordinates into the cells as is, and in addition, ends up sticking
;; a coordinate pair in with the foodspot itself.
;; Also make sure that non-integer perc-radii work.

(defn add-toroidal-foodspot!
  "Add foodspot to env, with its perceptual radius perc-radius around
  location [x y], but wrapped toroidally if necessary. All cells within
  perc-radius of (x y) will have the value [x y] conj'ed to indicate that
  they are within the perceptual radius of foodspot, and the center--the
  actual foodspot--will have the value passed as foodspot-val, or
  env-indexed/default-foodspot-val."
  ([env perc-radius x y]
   (add-toroidal-foodspot! env perc-radius x y default-foodspot-val))
  ([env perc-radius x y foodspot-val]
   (let [locs (:locs env)
         round-x (math/round x)
         round-y (math/round y)]
     ;; NOTE reversed order of x and y because the internal
     ;; representation uses traditional matrix indexing:
     (doseq [[x* y*] (make-toroidal-donut env perc-radius x y)] ; donut, i.e. leave out center
       (mset-conj! locs y* x* [round-x round-y])) ; i.e. we set each cell to pair that points to the center
     (mset-conj! locs y x foodspot-val)))) ; center gets special value

(defn add-toroidal-foodspots!
  "Add multiple foodspots at coordinate pairs in locs, to env with
  perceptual radius perc-radius. See add-toroidal-foodspot! for further
  details."
  [env perc-radius locs]
  (doseq [[x y] locs]
    (add-toroidal-foodspot! env perc-radius x y)))

(defn add-trimmed-foodspot!
  "Add foodspot to env, with its perceptual radius perc-radius around
  location [x y], but trimmed to fit within the environment: points within
  the radius that are outside the boundaries of the envirment won't be
  included. All cells within perc-radius of (x y) and within the
  environment boundaries will have the value [x y] conj'ed to indicate that
  they are within the perceptual radius of foodspot, and the center--the
  actual foodspot--will have the value passed as foodspot-val, or
  env-indexed/default-foodspot-val."
  ([env perc-radius x y]
   (add-trimmed-foodspot! env perc-radius x y default-foodspot-val))
  ([env perc-radius x y foodspot-val]
   (let [locs (:locs env)]
     ;; NOTE reversed order of x and y because the internal
     ;; representation uses traditional matrix indexing:
     (doseq [[x* y*] (make-trimmed-donut env perc-radius x y)] ; donut, i.e. leave out center
       (mset-conj! locs y* x* [x y])) ; i.e. we set each cell to pair that points to the center
     (mset-conj! locs y x foodspot-val)))) ; center gets special value

(defn add-trimmed-foodspots!
  "Add multiple foodspots at coordinate pairs in locs, to env with
  perceptual radius perc-radius. See add-trimmed-foodspot! for further
  details."
  [env perc-radius locs]
  (doseq [[x y] locs]
    (add-trimmed-foodspot! env perc-radius x y)))


(defn raw-env-locs-getxy
  "Returns whatever is at (external) coordinates x, y in locations matrix
  locs. Non-integer coordinates will NOT be run through clojure.math/round.
  Coordinates must be legal for locs. They should be rounded first.
  (Non-rounded coords will succeed, but will be floored by core.matrix.
  Rounding is better.)"
  [locs x y]
  (mx/mget locs y x)) ; Note x, y reversed since core.matrix uses trad matrix indexing

; Notes: Possibly avoid in inner loops; instead use env-mat-getxy.
(defn raw-env-getxy
  "Returns whatever is at (external) coordinates x, y in environment e.
  Coordinates must be legal for the :locs matrix in env."
  [env x y]
  (raw-env-locs-getxy (:locs env) x y))

;; Returns false rather than nil because under some schemes, I initialize
;; cells with nil.  If so, and if this functionr returned nil to indicate out
;; out of bounds, there would be no way of finding this out from the return value.
(defn trimmed-env-locs-getxy
  "Returns the contents of matrix locs at (x,y), or false if coordinates
  are outside the boundaries of locs."
  [locs size x y]
  (if (or (neg? x) (neg? y)
          (>= x size) (>= y size))
    false
    (raw-env-locs-getxy locs x y)))

(defn trimmed-env-getxy
  "Returns the contents of :locs locs at (x,y), or false if
  coordinates are outside the boundaries of locs."
  [env x y]
  (trimmed-env-locs-getxy (:locs env) (:size env) x y))

(defn toroidal-env-locs-getxy
  "After possible toroidal wrapping using size, returns the contents of
  matrix locs at (x,y)."
  [locs size x y]
  (raw-env-locs-getxy locs (rem x size) (rem y size)))

(defn toroidal-env-getxy
  "After possible toroidal wrapping using :size, returns the contents of
  the :locs matrix at (x,y)."
  [env x y]
  (toroidal-env-locs-getxy (:locs env) (:size env) x y))

;; Currently, a foodspot is simply a pair of coordinates in this
;; environment implementation.  Perhaps later it might be valuable
;; to add additional information, but add it as additional elements
;; to the sequence of two coordinates.  This version of foodspot-coords
;; is designed to work with that case (but would be slow if repeatedly
;; called in an inner loop).
(defn foodspot-coords 
  [[x y]]
  "Returns the coordinates of a foodspot."
  [x y])

;; Method used below doesn't try to find the foodspots themselves.  Their
;; coordinates are referenced many times, so we extract them into a set.
;; A more bespoke method might be faster (perhaps using mx/ereduce).
(defn locs-foodspot-coords
  "Returns a collection of coordinate pairs of all foodspots in matrix
  locs, or nil if there are none."
  [locs]
  (-> (into #{} cat (mx/eseq locs))
      (disj default-foodspot-val)
      (seq)))

(defn env-foodspot-coords
  "Returns a collection of coordinate pairs of all foodspots in environment
  env, or nil if there are none."
  [env]
  (locs-foodspot-coords (:locs env)))

(comment
  (use 'clojure.repl) ; for pst

  (def size 50)
  (mx/shape (:locs e))
  (mx/pm (:locs e))
  (def e (make-env size))
  (add-toroidal-foodspot! e 4 2 3)
  (add-toroidal-foodspot! e 4.5 2.75 3.3)
  (add-toroidal-foodspot! e 2 6 4)
  (add-toroidal-foodspot! e 2 44.17 29.8)
  (rawenv-getxy e 6 4) ; doesn't use matrix indexing
  (raw-env-getxy e 2 4) ; doesn't use matrix indexing

  (locs-foodspot-coords (:locs e))
  (env-foodspot-coords e)
  (filter-coords (:locs e) seq)

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def foodspots (h/vega-env-plot e 400 0.75))
  (oz/view! foodspots)
  (def in-radii (h/vega-food-plot
                  (h/add-point-labels "food" 
                                      (map reverse ; because matrix coords
                                           (filter-coords (:locs e) seq)))
                  (env-size e)
                  400
                  0.75))
  (oz/view! in-radii)
)


(defn matrix-perc-foodspots
  "Examines location [(round x) (round y)] in env. Returns a falsey value
  if no foodspot is within the perceptual radius of that position, or a
  sequence (not merely a collection) of coordinates of all foodspots within
  perceptual radii. The function env-getxy should be a function such as
  trimmed-env-getxy (which returns the contents of a location or false if
  It's outside the env), or toroidal-env-getxy (which wraps locations). the
  function order-found (probably from forage.run) decides which foodspot to
  list first.  It is passed (a) the coordinates of the foodspot that the
  forager is precisely on (after coordinate rounding), if it is precisely
  on foodspot, or nil; and (b) a sequence of coordinates of foodspots that
  within perceptual radius, but not precisely on, or nil."
  [env-getxy order-found env x y]
  (let [x-int (math/round x) ; mget accepts floats but floors them
        y-int (math/round y) ; we want round, not floor
        whats-here (env-getxy env x-int y-int)
        foodspot-here (if (default-foodspot-val whats-here) [x-int y-int] nil)
        foodspots-near (sets/select vector? whats-here)] ; seq nilifies empty set
    (order-found foodspot-here foodspots-near)))

;; Partial has a perf hit only (maybe) when more than four args are
;; partialed up into the new function.  In fact it's better than
;; (fn [...] ...) since the partialed args are baked in.
(def perc-foodspots-trimmed
  "([order-found env x y])
  Runs matrix-perc-foodspots ignoring coordinates that fall outside the
  boundaries of env.  See matrix-perc-foodspots for further information
  about parameters."
  (partial matrix-perc-foodspots trimmed-env-getxy))

(def perc-foodspots-toroidally
  "([order-found env x y])
  Runs matrix-perc-foodspots with toroidal wrapping of coordinates that
  fall outside the boundaries of env.  See matrix-perc-foodspots for
  further information about parameters."
  (partial matrix-perc-foodspots toroidal-env-getxy))

;; TODO: Maybe add version of perc-foodspot that chooses the closest one, and
;; only chooses randomly if there are mulitple equally close foodspots.
