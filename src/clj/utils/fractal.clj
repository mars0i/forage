;; Mathematical operations to create or manipulate fractal structures
(ns utils.fractal
  (:require [clojure.math.numeric-tower :as nt :refer [floor]]
            [clojure.set :as s]
            [flatland.ordered.set :as os]
            [fastmath.complex :as c]
            [fastmath.vector :as v]))

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

(defn fournierize-points
  [offset points]
  (map (partial fournier-children offset) points))

(defn fournierize2d
  [n offset initial-points]
  (ifs-iterate n [fournierize-points] initial-points))
                      

;; FIXME ?  I'm using huge values for sep.  Maybe this is the wrong
;; approach.  cf. Mandelbrot's way of constructing Cantor dusts, including
;; the infinitely large ones.
;;
;; Inspired by Mandelbrot's description of Fournier d'Albe's model universe.
;; See Mandelbrot's _The Fractal Geometry of Nature_ pp. 86-87 and 95-96,
;; or one of its predecessor books.
(defn fournierize
  "DEPRECATED: Use fournierize2d.
  Given a sequence of coordinate pairs (points), returns a sequence containing
  those points and \"fournier children\", i.e. points that are (* sep multiplier)
  up, down, left, and to the right of each original point.  Then iterates,
  performing the same operation on all of the points at a smaller scale, levels
  times.  multiplier should be < 1.  (Note that the number of points is increased
  exponentially, multiplying by 5 each time.)"
  [points sep multiplier levels]
  (loop [pts points, offset sep, iters levels]
    (if (<= iters 0)
      pts
      (let [new-offset (* offset multiplier)
            new-pts (mapcat (partial fournier-children new-offset) pts)]
        (recur (into pts new-pts) new-offset (dec iters))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUADRATIC JULIA FUNCTIONS

(defn quad-fn
  "Returns an implementation of the quadratic function f(z) = z^2 + c,
  where z and c are instances of fastmath.complex numbers, i.e. Vec2's."
  [c]
  (fn [z] (c/add (c/sq z) c)))

(defn doesnt-escape?
  [max-iters esc-bound f init-z]
  (loop [i 1, z init-z]
    (cond (> (c/abs z) esc-bound) false
          (> i max-iters) true
          :else (recur (inc i) (f z)))))


;; This is fine for connected Julia sets, although you get a filled
;; Julia set, which might not be what you want.  For disconnected
;; Julia sets, it sometimes works, but sometimes you get a solid 
;; blob, as if it was a connected Julia set.
(defn filled-julia
  "Find all points in the box from [x-min, c-min] to [x-max, x-max] at
  increments of size step that approximate the filled Julia set of f, by
  finding those points such that results of iterating f on its output up
  to max-iters times doesn't exceed esc-bound.  Returns a collection of
  fastmath.complex Vec2 pairs."
  [x-min x-max c-min c-max step max-iters esc-bound f]
  (doall
   (for [x (range x-min x-max step)
         c (range c-min c-max step)
         :let [z (c/complex x c)]
         :when (doesnt-escape? max-iters esc-bound f z)]
     z)))

(defn inv-quad-fn
  "Returns an implementation of the inverse of the quadratic function
  f(z) = z^2 + c, i.e. returns f^{-1}(z) = sqrt(z - c), where z and c
  are instances of fastmath.complex numbers, i.e. Vec2's.  Returns a pair
  containg the positive and negative values of the square root."
  [c]
  (fn [z] 
    (let [posval (c/sqrt (c/sub z c))]
      [posval (c/neg posval)])))

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

;; Possibly rewrite using all reals, and recreate a complex at the end.
(defn c-clip
  "Like floor (for complex numbers), but floors to the nearest multiple
  of a real number gap, rather than the nearest integer.  If z is
  not provided, returns a function of one argument."
  ([gap z]
   (let [cres (c/complex gap 0.0)]
     (-> z
         (c/div cres) ; multiply by the reciprocal of cres
         c-floor
         (c/mult cres)))) ; divide by the reciprocal
  ([gap] (fn [z] (c-clip gap z)))) ; for use with into

(comment
  (c-floor (c/complex 1.5 -3.6))
  (c-clip 0.25 (c/complex 21.571 -3.6))
  ((c-clip 0.25) (c/complex 21.571 -3.6))
 )

(defn clip-into-set
  "Clips (floors) elements in zs to multiples of gap, and returns the
  modified values as a set.  Inspired by \"Inverse iteration algorithms
  for Julia sets\", by Mark McClure,in _Mathematica in Education and
  Research_, v.7 (1998), no. 2, pp 22-28,
  https://marksmath.org/scholarship/Julia.pdf"
  [gap zs]
  (into #{} (map (c-clip gap)) zs))
  ;; or: (set (map (c-clip gap) zs))

(comment
  (clip-into-set 1/3 [(c/complex 1.5 1.6) (c/complex -1.5 0.8) (c/complex 0.2 -27)])
)

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

;; It might be interesting to write this using clojure.core/tree-seq.
(defn old-julia-inverse
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within gap distance
  from points already collected.  More precisely, points are kept if they
  are still new after being floored to a multiple of gap.  This is
  based on the second algorithm in Mark McClure's
  \"Inverse Iteration Algorithms for Julia Sets\".  (gap is the reciprocal
  of McClure's resolution.) depth must be >=1.  Returns a Clojure set of 
  fastmath.complex (Vec2) points (which often will automatically be
  converted to Clojure seq pairs)."
  [gap inverse-f depth z]
  (let [zs-set_ (atom #{})]
    (letfn [(inv-recur [curr-depth curr-z]
              (let [new-vals (-> (clip-into-set gap (inverse-f curr-z))
                                 (s/difference @zs-set_))]
                (swap! zs-set_ s/union new-vals)
                (when (> curr-depth 1)
                  (run! (partial inv-recur (dec curr-depth)) new-vals))))] ; depth first
      (inv-recur depth z))
    @zs-set_))

;; Should I collect and return the precise values, not clipped?  Well, 
;; I'm already throwing out a lot of precise values; why is the one that is
;; retained special?  The clipped value is the average--the effective meaning.
;; (It might be interesting to write this using clojure.core/tree-seq.)
(defn julia-inverse
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within gap distance
  from points already collected.  More precisely, points are kept if they
  are still new after being floored to a multiple of gap.  This is
  based on the second algorithm in Mark McClure's
  \"Inverse Iteration Algorithms for Julia Sets\".  (gap is the reciprocal
  of McClure's resolution.) depth must be >=1.  Returns a Clojure set of 
  fastmath.complex (Vec2) clipped (i.e. floored) points."
  [gap inverse-f depth z]
  (let [zs-set_ (atom #{})]
    (letfn [(inv-recur [curr-depth curr-z]
              (let [zs-set @zs-set_
                    [v1 v2] (inverse-f curr-z)
                    cv1 (c-clip gap v1)
                    cv2 (c-clip gap v2)
                    v1-isnt-near (not (contains? zs-set cv1))
                    v2-isnt-near (not (contains? zs-set cv2))]
                (swap! zs-set_ s/union #{cv1 cv2}) ; partly redundant; conj individually iff v?-isnt-near ?
                (when (> curr-depth 1)
                  (when v1-isnt-near 
                    (inv-recur (dec curr-depth) v1)) ; not worth avoiding recursion;
                  (when v2-isnt-near                      ; unlikely to blow stack
                    (inv-recur (dec curr-depth) v2)))))]
      (inv-recur depth z))
    @zs-set_))

(defn julia-inverse-debug
  "Version of julia-inverse that stores additional information.  julia-inverse
  only stores recorded points in a set; this function also stores them in a
  sequence, with new points conj'ed onto the end, and in a tree that reflects
  the way that points are found by the recursive calls.  
  NOTE: Make sure algorithm is up to date with julia-inverse before using!"
  [gap inverse-f depth z]
  (let [zs-set_ (atom #{})
        zs-seq_ (atom [])  ; records complex numbers in order found
        zs-set-seq_ (atom [])] ; records pairs/singletons/empty sets in order found
    (letfn [(inv-recur [curr-depth curr-z]
              (let [new-vals (-> (clip-into-set gap (inverse-f curr-z))
                                 (s/difference @zs-set_))]
                (swap! zs-set_ s/union new-vals)
                (swap! zs-seq_ multi-conj new-vals)
                (swap! zs-set-seq_ conj new-vals) ; #{a b} or #{a} or #{b} or #{}
                (when (> curr-depth 1)
                  (run! (partial inv-recur (dec curr-depth)) new-vals))))]
      (inv-recur depth z))
    [@zs-set_ @zs-seq_ @zs-set-seq_]))

(defn yo
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within gap distance
  from points already collected.  More precisely, points are kept if they
  are still new after being floored to a multiple of gap.  This is
  based on the second algorithm in Mark McClure's
  \"Inverse Iteration Algorithms for Julia Sets\".  (gap is the reciprocal
  of McClure's resolution.) depth must be >=1.  Returns a Clojure set of 
  fastmath.complex (Vec2) points (which often will automatically be
  converted to Clojure seq pairs)."
  [gap inverse-f depth z]
  (let [curr-zs_ (atom #{})
        yonotyet_ (atom true)]
    (letfn [(inv-recur [curr-depth curr-z]
              (let [new-vals (-> (clip-into-set gap (inverse-f curr-z))
                                 (s/difference @curr-zs_))]
                (swap! curr-zs_ s/union new-vals)
                (when @yonotyet_
                  (println new-vals)
                  (let [nvs (seq new-vals)]
                    (when (or (= c/ZERO (first nvs))
                              (= c/ZERO (second nvs)))
                      (reset! yonotyet_ false))))
                (when (> curr-depth 1)
                  (run! (partial inv-recur (dec curr-depth)) new-vals))))]
      (inv-recur depth z))
    @curr-zs_))

(comment
  (def f-1 (inv-quad-fn (c/complex 0.0 0.68)))
  (def zs (time (julia-inverse 0.01 f-1 2 c/ZERO)))
  (def zs (time (julia-inverse 0.001 f-1 1000 c/ZERO)))
  (count zs)
)

(defn julia-inverse-simple
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set.  See e.g. Falconer's _Fractal Geometry_, 3d ed,
  p. 255, or Mark McClure's \"Inverse Iteration Algorithms for Julia Sets\".
  depth must be >=1.  (Computes sum_i^n 2^i values.  julia-inverse is more
  efficient.)"
  [inverse-f depth z]
  (let [pair (inverse-f z)]
    (if (== depth 1)
      pair
      (doall ; periodically make sure won't be tripped up by concat's laziness
             (concat pair
                     (mapcat (partial julia-inverse-simple inverse-f (dec depth))
                             pair))))))

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

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (def vl-zs (doall (h/add-point-labels "Julia set" zs)))
  (oz/start-server!)
  (oz/view! (time (h/vega-food-plot (h/add-point-labels "Julia set" zs) 500 800 1)))
  (oz/view! (time (h/vega-food-plot (h/add-point-labels "Julia set" zs) 1000 1000 1)))

  (let [[x c] (c/complex 5 4)] [x c])

)




;; TODO UNIMPLEMENTED
(defn julia-corners
  "Algorithm for approximating a (non-filled) Julia set, sketched in 
  Falconer's _Fractal Geometry_, 3d ed, pp. 255f."
  [x-min x-max c-min c-max max-iters f]
  (println "UNIMPLEMENTED"))
