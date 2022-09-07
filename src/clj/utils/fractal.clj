;; Mathematical operations to create or manipulate fractal structures
(ns utils.fractal
  (:require [clojure.math.numeric-tower :as nt :refer [floor]]
            [clojure.set :as s]
            [flatland.ordered.set :as os]
            [fastmath.complex :as c]
            [fastmath.vector :as v]
            [utils.math :as m]))

;; Switched my earlier naming convention for atoms (final "$") to Peter Taoussanis's
;; better known and equally good convention from taoensso.encore (final "_"):
;; https://github.com/ptaoussanis/encore/blob/f2450b7a12712b7553bb61603e6e98ac75d4a34d/src/taoensso/encore.cljc#L19

;; From the ordered-set docstring:
;;
;;    Note that clojure.set functions like union, intersection, and
;;    difference can change the order of their input sets for efficiency
;;    purposes, so may not return the order you expect given ordered sets
;;    as input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL-PURPOSE ITERATIVE FUNCTION SYSTEMS

;; Note it's not enough to recursively apply the functions to the points,
;; because the functions themselves must be recursively transformed.
;; e.g. additions as well as multiplications have to be scaled recursively.
;; (In theory the recursion could be moved to a macro body that simply
;; contructed a for expression with the appropriate number of variables.
;; Though maybe having a for comprehension that big would be problematic?
;; Or I bet it's handled in a similar way internally.)
(defn ifs-iterate
  "Given a set of points, recursively (n times) applies each transformation
  function found in fns to all of the points together (not to each 
  individually).  So the functions should be appropriate for whatever is
  in points, be numbers, pairs, triples, etc. (Be aware that the number of 
  functions that are internally applied internally grows exponentially.)"
  [n fns points]
  (if (zero? n)
    points
    (let [newfns (loop [k n, fs fns] ; recursively construct the lattice of functions
                   (if (> k 1)
                     (recur (dec k)
                            (for [f fs, g fs] (comp f g)))
                     fs))]
      ((apply juxt newfns) points)))) ; now apply them to points


;; Illustration of use of ifs-iterate with numbers replaced by intervals
(defn middle-third-cantor-tran1
  [endpts]
  (map #(/ % 3) endpts))

(defn middle-third-cantor-tran2
  [endpts]
  (map #(+ 2/3 (/ % 3)) endpts))

(defn middle-third-cantor
  "Given a pair of endpoints, returns a sequence of endpoints representing
  alternating endpoints of the corresponding middle-third Cantor set.
  Does not indicate which are left or right endpoints."
  [n endpoints]
  (ifs-iterate n 
               [middle-third-cantor-tran1 middle-third-cantor-tran2]
               endpoints))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOURNIER UNIVERSES (see Mandelbrot's books)

(defn fournier-children
  "Given a coordinate pair, return four coordinate pairs that are
  shifted by offset up, down, left, and right from the original point."
  [offset [x y]]
  [[(+ x offset) y] [x (+ y offset)]
   [(- x offset) y] [x (- y offset)]])

(defn eightnier-children
  "Given a coordinate pair, return eight coordinate pairs that are
  shifted by offset up, down, left, and right from the original point, 
  along with points that are rotated 45 degrees from those points."
  [offset [x y]]
  (let [four-centered-children (fournier-children offset [0 0])
        rotated-children (map (partial m/rotate (/ m/pi 4)) four-centered-children)
        all-centered-children (concat four-centered-children rotated-children)]
    (map (fn [[child-x child-y]] [(+ child-x x) (+ child-y y)])
         all-centered-children)))

(defn fournierizer
  "Returns a fournierize function using children-fn.  See doc for
  fournierize for case using fournier-children."
  [children-fn]
  (fn [points sep multiplier levels]
    (loop [pts points, offset sep, iters levels]
      (if (<= iters 0)
        pts
        (let [new-offset (* offset multiplier)
              new-pts (mapcat (partial children-fn new-offset) pts)]
          (recur (into pts new-pts) new-offset (dec iters)))))))

;; Inspired by Mandelbrot's description of Fournier d'Albe's model universe.
;; See Mandelbrot's _The Fractal Geometry of Nature_ pp. 86-87 and 95-96,
;; or one of its predecessor books.
(def fournierize
  "([points sep multiplier levels])
  Given a sequence of nate pairs (points), returns a sequence
  containing those points and \"fournier children\", i.e. points that
  are (* sep multiplier) up, down, left, and to the right of each
  original point.  Then the function iterates, performing the same 
  operation on all of the points at a smaller scale, levels times.  
  multiplier should be < 1.  (Note that the number of points is increased
  exponentially, multiplying by 5 each time.)"
  (fournierizer fournier-children))

(def eightnierize
  "Given a sequence of coordinate pairs (points), returns a sequence
  containing those points and \"fournier children\", i.e. points that
  are (* sep multiplier) up, down, left, and to the right of each
  original point, as well as points rotated 45 degreess from those.
  Then iterates, performing the same operation on all of the points at a
  smaller scale, levels times.  multiplier should be < 1.  (Note that
  the number of points is increased exponentially, multiplying by 9 each
  time.)"
  (fournierizer eightnier-children))


(comment
  ;; Attempt to use IFS to implement fournierization.
  ;; FIXME this is a mess.  not right.
  (defn fournierize-points
    "Applies fournier-children with offset as first parameter to each point,
    the results in a sequence."
    [offset-mult offset points]
    (mapcat (partial fournier-children offset) points))
  ;; FIXME not right that offset is fixed.  It should be a multiplier that
  ;; changes at each level.  Or maybe the change should be in fournier-children.
  (defn fournierize2d
    "Given a set of points, recursively applies fournierize-points--with
    offset as first parameter--iters times to points, starting with 
    initial-points."
    [iters offset-mult init-offset initial-points]
    (ifs-iterate iters 
                 [(partial fournierize-points offset-mult)]
                 initial-points))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUADRATIC JULIA FUNCTIONS: SIMPLE GENERATION AND SUPPORT FUNCTIONS

(defn quad-fn
  "Returns an implementation of the quadratic function f(z) = z^2 + c,
  where z and c are instances of fastmath.complex numbers, i.e. Vec2's."
  [c]
  (fn [z] (c/add (c/sq z) c)))

(defn inv-quad-fn
  "Returns an implementation of the inverse of the quadratic function
  f(z) = z^2 + c, i.e. returns f^{-1}(z) = sqrt(z - c), where z and c
  are instances of fastmath.complex numbers, i.e. Vec2's.  Returns a pair
  containg the positive and negative values of the square root."
  [c]
  (fn [z] 
    (let [posval (c/sqrt (c/sub z c))]
      [posval (c/neg posval)])))

(defn doesnt-escape?
  [max-iters esc-bound f init-z]
  (loop [i 1, z init-z]
    (cond (> (c/abs z) esc-bound) false
          (> i max-iters) true
          :else (recur (inc i) (f z)))))

;; This is fine for connected Julia sets, although you get a filled
;; Julia set, which might not be what you want.  For disconnected
;; Julia sets, sometimes you get a solid blob, as if it was a connected 
;; Julia set IF max-iters IS TOO SMALL.  This is because by that point,
;; they points have not necessarily escaped the bound.  These parameters
;; may need to be tuned for each quadratic function to avoid blobs or 
;; night-sky style images.  (e.g. for 0.0+0.68i, they should be set e.g.
;; to max-iters=60, esc-bound=10 will give you an OK representation of the
;; actual Julia set with minimal blobs.)
(defn filled-julia
  "Find all points in the box from [x-min, c-min] to [x-max, x-max] at
  increments of size step that approximate the filled Julia set of f
  Method: Find points such that results of iterating f on its output up
  to max-iters times does not exceed esc-bound.  Returns a collection of
  fastmath.complex Vec2 pairs."
  [x-min x-max c-min c-max step max-iters esc-bound f]
  (doall
   (for [x (range x-min x-max step)
         c (range c-min c-max step)
         :let [z (c/complex x c)]
         :when (doesnt-escape? max-iters esc-bound f z)]
     z)))

(defn c-floor
  "Floor function for complex numbers based on Wolframe's Floor function:
  Floors the real and imaginary parts separately.  See 
  https://mathworld.wolfram.com/FloorFunction.html,
  https://math.stackexchange.com/a/2095679/73467, 
  \"Inverse iteration algorithms for Julia sets\", by Mark McClure, in
  _Mathematica in Education and Research_, v.7 (1998), no. 2, pp 22-28,
  https://marksmath.org/scholarship/Julia.pdf"
  [z] (c/complex (nt/floor (c/re z))
                 (nt/floor (c/im z))))

(defn c-round
  "Round function for complex numbers based on Wolframe's Floor function:
  Floors the real and imaginary parts separately.  See 
  https://mathworld.wolfram.com/FloorFunction.html,
  https://math.stackexchange.com/a/2095679/73467, 
  \"Inverse iteration algorithms for Julia sets\", by Mark McClure, in
  _Mathematica in Education and Research_, v.7 (1998), no. 2, pp 22-28,
  https://marksmath.org/scholarship/Julia.pdf"
  [z] (c/complex (nt/round (c/re z))
                 (nt/round (c/im z))))

(defn c-round-to
  "Like round (for complex numbers), but rounds to the nearest multiple
  of a real number resolution, rather than the nearest integer.  If z is
  not provided, returns a function of one argument."
  ([resolution z]
   (let [cres (c/complex resolution 0.0)]
     (-> z
         (c/div cres) ; multiply by the reciprocal of cres
         c-round
         (c/mult cres)))) ; divide by the reciprocal
  ([resolution] (fn [z] (c-round-to resolution z)))) ; for use with into

;; DEPRECATED
(defn clip-into-set
  "Clips (floors) elements in zs to multiples of resolution, and returns the
  modified values as a set.  Inspired by \"Inverse iteration algorithms
  for Julia sets\", by Mark McClure,in _Mathematica in Education and
  Research_, v.7 (1998), no. 2, pp 22-28,
  https://marksmath.org/scholarship/Julia.pdf"
  [resolution zs]
  (into #{} (map (c-round-to resolution)) zs))
  ;; or: (set (map (c-round-to resolution) zs))


;; The simple function that julia-inverse below improves on.
(defn julia-inverse-simple
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set.  See e.g. Falconer's _Fractal Geometry_, 3d ed,
  p. 255, or Mark McClure's \"Inverse Iteration Algorithms for Julia Sets\".
  depth must be >=1.  (Computes sum_i^n 2^i values.  julia-inverse is more
  efficient!)"
  [inverse-f depth z]
  (let [pair (inverse-f z)]
    (if (== depth 1)
      pair
      (doall ; periodically make sure won't be tripped up by concat's laziness
             (concat pair
                     (mapcat (partial julia-inverse-simple inverse-f (dec depth))
                             pair))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFICIENT JULIA INVERSE ITERATION GENERATORS
;; Based on an algorithms in Mark McClure's \"Inverse Iteration Algorithms for 
;; Julia Sets\" and Peitgen and Richter's _The Beauty of Fractals_.
;; NOTE: The parameter 'resolution' below is the reciprocal of McClure's 
;; misnamed 'resolution' variable.

;; julia-inverse below commits the sin of using an imperative algorithm
;; when a purely functional algorithm might be reasonable.  Despite what
;; some functional programming devotees say, there are cases in which an
;; imperative algorithm is easier to understand.  This is one of them.
;; However, the imperative bit is kept contained within the function itself.
;; From the outside, julia-inverse can be treated as purely functional.

;; It might be interesting to write this using clojure.core/tree-seq.

;; Based on McClure's algorithm.
;; THIS VERSION RECURSES ON THE UN-CLIPPED VALUES, RETURNING A MAP WITH 
;; CLIPPED VALUES AS KEYS AND UNCLIPPED ONES AS  VALUES.  Note that the
;; unclipped values are just the ones that got there first, so in a sense
;; the clipped values have a better claim to being the *real* values to
;; be returned.  But by retaining the values actually used for the recursion,
;; we keep around info about what the iteration was actually based on rather
;; than discarding that information.  This may use up a little bit more memory,
;; but it's useful, and it's not slower.  (It may even be a little faster.)
;; Note there's no simple way to use loop/recur here; we need to recurse down 
;; two parallel paths.  But the number of iterations needed is unlikely to
;; blow the stack, and it runs quickly.
(defn julia-inverse-debug-store-full-vals
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within resolution distance
  from points already collected.  More precisely, points are kept if they
  are still new after being rounded to a multiple of resolution.  init-z is the 
  initial value to iterate from, a  fastmath.complex (Vec2) number.  iter-depth
  must be >=0.  Returns a Clojure map in which each key is one of the 
  clipped points, whose value is the first, non-clipped value that caused
  the key/value pair to be added to the map.  NOTE: To avoid spurious
  isolated points that are not part of the Julia set, re-run with init-z
  very near some part of the Julia set."
  [resolution iter-depth inverse-f init-z]
  (let [zs-map_ (atom {})]  ; a recursive imperative algorithm
    (letfn [(inv-recur [curr-depth curr-z]
              (when (> curr-depth 0) ; otherwise nil--which won't be used
                (let [[z+ z-] (inverse-f curr-z)
                      cz+ (c-round-to resolution z+)
                      cz- (c-round-to resolution z-)]
                  (when-not (@zs-map_ cz+)           ; If z+ is not near a previous point
                    (swap! zs-map_ assoc cz+ z+)     ; add clipped version and z+ to the map
                    (inv-recur (dec curr-depth) z+)) ; and iterate on z+ .
                  (when-not (@zs-map_ cz-)           ; Ditto for z- .
                    (swap! zs-map_ assoc cz- z-)
                    (inv-recur (dec curr-depth) z-)))))]
      (inv-recur iter-depth init-z))
    @zs-map_))

(defn inc-or-one
  "Return (inc x) unless x is falsey, in which case return 1, as if nil
  was zero and was incremented."
  [x]
  (if x (inc x) 1))

;; TODO WHY DO FUNCTIONS ABOVE AND BELOW RETURN SLIGHTLY DIFFERENT SETS?

;; Based on an algorithm in Peitgen and Richter.
(defn julia-inverse
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, but without iterating further when too many
  previous points are within resolution distance of the new point.
  init-z is the initial value to iterate from [a fastmath.complex (Vec2)
  number].  iter-depth must be >=0.  Returns a Clojure map in which each key
  is one of the clipped points, and the values of the counts of nearby points
  that have been iterated on."
  [resolution max-revisits iter-depth inverse-f init-z]
  (let [zsmap_ (atom {})]  ; a recursive imperative algorithm
    (letfn [(inv-recur [curr-depth curr-z]
              (when (> curr-depth 0) ; otherwise nil--which won't be used
                (let [[z+ z-] (inverse-f curr-z)
                      cz+ (c-round-to resolution z+) ; canonical value of micro-region
                      cz- (c-round-to resolution z-) ; ditto
                      z+count (@zsmap_ cz+)  ; number of prev visits, or nil if none
                      z-count (@zsmap_ cz-)] ; ditto
                  ;; recurse on branches for which micro-region is not overpopulated:
                  (when (or (not z+count) (< z+count max-revisits))
                      (swap! zsmap_ update cz+ inc-or-one)
                      (inv-recur (dec curr-depth) z+))
                  (when (or (not z-count) (< z-count max-revisits))
                      (swap! zsmap_ update cz- inc-or-one)
                      (inv-recur (dec curr-depth) z-)))))]
      (inv-recur iter-depth init-z))
    @zsmap_))

;; THIS VERSION RECURSES ON THE CLIPPED VALUES, RETURNING THEM AS A SET.
(defn old-julia-inverse-recurse-on-clipped
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within resolution distance
  from points already collected.  More precisely, points are kept if they
  are still new after being floored to a multiple of resolution.  This is
  based on the second algorithm in Mark McClure's \"Inverse Iteration 
  Algorithms for Julia Sets\".  (resolution is the reciprocal of McClure's
  resolution.) depth must be >=1.  Returns a Clojure set of fastmath.complex
  (Vec2) points."
  [resolution inverse-f depth z]
  (let [zs-set_ (atom #{})]
    (letfn [(inv-recur [curr-depth curr-z]
              (let [new-vals (-> (clip-into-set resolution (inverse-f curr-z))
                                 (s/difference @zs-set_))]
                (swap! zs-set_ s/union new-vals)
                (when (> curr-depth 1)
                  (run! (partial inv-recur (dec curr-depth)) new-vals))))] ; depth first
      (inv-recur depth z))
    @zs-set_))

;; THIS VERSION RECURSES ON THE UN-CLIPPED VALUES, BUT RETURNS THE CLIPPED
;; VALUES, NOT THE UNCLIPPED ONES, AS A SET.
(defn old-julia-inverse-recurse-on-unclipped
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within resolution distance
  from points already collected.  More precisely, points are kept if they
  are still new after being floored to a multiple of resolution.  This is
  based on the second algorithm in Mark McClure's
  \"Inverse Iteration Algorithms for Julia Sets\".  (resolution is the reciprocal
  of McClure's resolution.) depth must be >=1.  Returns a Clojure set of 
  fastmath.complex (Vec2) clipped (i.e. floored) points."
  [resolution inverse-f depth z]
  (let [zs-set_ (atom #{})]
    (letfn [(inv-recur [curr-depth curr-z]
              (when (> curr-depth 0) ; otherwise nil--which won't be used
                (let [zs-set @zs-set_
                      [v1 v2] (inverse-f curr-z)
                      cv1 (c-round-to resolution v1)
                      cv2 (c-round-to resolution v2)
                      v1-isnt-near (not (contains? zs-set cv1))
                      v2-isnt-near (not (contains? zs-set cv2))]
                  ;not exactly same as below--not sure why: (swap! zs-set_ s/union #{cv1 cv2})
                  (when v1-isnt-near 
                    (swap! zs-set_ conj cv1) 
                    (inv-recur (dec curr-depth) v1)) ; unlikely to blow stack
                  (when v2-isnt-near
                    (swap! zs-set_ conj cv2) 
                    (inv-recur (dec curr-depth) v2)))))]
      (inv-recur depth z))
    @zs-set_))

(defn complex-to-vecs
  "Convenience function convert a collection of fastmath.complex numbers
  to a sequence of Clojure vector pairs by calling vec->Vec.  (Not always
  needed. Some contexts will treat complex numbers, i.e. Vec2's, as 
  sequences.)"
  [cs]
  (map v/vec->Vec cs))

(def c2v complex-to-vecs)

(defn filled-julia-vecs
  "Calls filled-julia and then converts its output to a collection
  of Clojure vectors."
  [x-min x-max c-min c-max step max-iters esc-bound f]
  (println "DEPRECATED")
  (map v/vec->Vec
       (filled-julia x-min x-max c-min c-max step max-iters esc-bound f)))

;; Useful with ordered-set, because clojure.set/union may reorder elements,
;; and concat always returns a sequence, not a set.  ordered-set knows about
;; conj, though.
(defn multi-conj
  "Successively conj each element of ys onto xs."
  [xs ys]
  (reduce (fn [newxs y] (conj newxs y))
          xs ys))

(comment
  (multi-conj [4 5 6] [3 2 4])
  (multi-conj (os/ordered-set 4 5 6) [3 4 8])
  (multi-conj (os/ordered-set 4 5 6) (os/ordered-set 4 3))
)


(comment
  ;; Informal tests/illustrations of filled-julia-vecs:

  (def f1 (quad-fn c/ZERO))
  (def zs (filled-julia -2 1 -1 1 0.01 100 2 f1))
  (count zs)
  (take 10 zs)
  (map v/vec->Vec (take 10 zs))

  ;; Should be a disconnected Julia set; c is to the right of the middle-sized
  ;; lower "ear" of the Mandelbrot set.
  (def f1 (quad-fn (c/complex 0.06310618062296447 -0.7250300283183553)))
  (def zs (filled-julia-vecs -2 2 -2 2 0.0005 100 4 f1)) ; hard to grid size right
  (count zs)
  ;; c is near center of left "head" of Mandelbrot set:
  (def f1 (quad-fn (c/complex -1.025871775288859 -0.0007815313243673128)))
  (def zs (filled-julia-vecs -2 2 -2 2 0.01 100 4 f1))
  (count zs)

  ;; c is outside the Mandelbrot set, nestled in the crevice on the right.
  ;; The plot is indeed disconnected.
  (def f1 (quad-fn (c/complex 0.18815628082336522 -1.2981763209035255)))
  (def zs (filled-julia-vecs -2 2 -2 2 0.001 10 2 f1))

  ;; This is supposed to be disconnected, but my algorithm is creating a
  ;; connected, filled image, even with a dots of dimension 1.
  (def f1 (quad-fn (c/complex -0.025470973685652876 -0.6729258199015218)))
  (def zs (filled-julia-vecs -2 2 -2 2 0.001 10 2 f1))
  (count zs)

  (def f1 (quad-fn (c/complex 0.0 0.68)))
  ;; Disconnected Julia set from pp. 253, 254 of Falconer's _Fractal Geometry_
  (def f1 (quad-fn (c/complex 0.0 0.66)))
  (def zs (time (doall (-> (filled-julia -2 2 -1.5 1.5 0.001 10 5 f1) (complex-to-vecs)))))
  (def zs (time (doall (-> (p-filled-julia 8 -2 2 -1.5 1.5 0.001 10 5 f1) (complex-to-vecs)))))
  (count zs)


  (def f-1 (inv-quad-fn (c/complex 0.0 0.68)))
  (def zs (time (julia-inverse 0.001 5 100000 f-1 c/ZERO)))
  (count zs)

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)
  (oz/view! (time (h/vega-food-plot (h/add-point-labels "Julia set" (keys zs)) 500 800 0.7)))

  ;; This didn't work--Firefox displayed blank:
  (def julia68s (time (into {} (map (fn [z] [z (julia-inverse 0.001 f-1 100000 z)]) [c/ZERO (c/complex 1 0) (c/complex 0 1) (c/complex 1 1) (c/complex 5 0) (c/complex 0 5) (c/complex 5 5)]))))
  (def grid68s (hc/xform uh/grid-chart :COLUMNS 3 :CONCAT (mapv (fn [k] (h/vega-food-plot (h/add-point-labels (str k) (keys (julia68s k))) 500 800 2)) (keys julia68s))))

)




;; TODO UNIMPLEMENTED
(defn julia-corners
  "Algorithm for approximating a (non-filled) Julia set, sketched in 
  Falconer's _Fractal Geometry_, 3d ed, pp. 255f."
  [x-min x-max c-min c-max max-iters f]
  (println "UNIMPLEMENTED"))
