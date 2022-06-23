;; Mathematical operations to create or manipulate fractal structures
(ns utils.fractal
  (:require [clojure.math.numeric-tower :as nt :refer [floor]]
            [clojure.set :as s]
            [fastmath.complex :as c]
            [fastmath.vector :as v]))


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


(defn filled-julia
  "Find all points in the box from [x-min, c-min] to [x-max, x-max] at
  increments of size step that approximate the filled Julia set of f, by
  finding those points such that results of iterating f on its output up
  to max-iters times doesn't exceed esc-bound.  Returns a collection of
  fastmath.complex Vec2 pairs."
  [x-min x-max c-min c-max step max-iters esc-bound f]
  (for [x (range x-min x-max step)
        c (range c-min c-max step)
        :let [z (c/complex x c)]
        :when (doesnt-escape? max-iters esc-bound f z)]
    z))


;; DOESN'T SEEM TO IMPROVE SPEED
(defn p-filled-julia
  "Perform calculation similar to filled-julia (q.v.), but try to calculate
  in parallel in n-threads threads."
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  (let [x-subrange (/ (- x-max x-min) n-threads)
        x-mins (range x-min x-max x-subrange)
        x-maxs (conj (vec (rest x-mins)) x-max)]
    (into [] cat
          (pmap (fn [xmn xmx]
                  (filled-julia xmn xmx c-min c-max step max-iters esc-bound f))
                x-mins x-maxs))))


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
  "Clips elements in zs to multiples of gap, and returns the
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

;; This simple algorithm using partial is a little faster than an earlier
;; version, julia-inverse-simple-alt, that recursed using an internal function
;; without partial.
(defn julia-inverse-simple
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set.  See e.g. Falconer's _Fractal Geometry_, 3d ed,
  p. 255, or Mark McClure's \"Inverse Iteration Algorithms for Julia Sets\".
  depth must be >=1."
  [inverse-f depth z]
  (let [pair (inverse-f z)]
    (if (== depth 1)
      pair
      (doall ; periodically make sure won't be tripped up by concat's laziness
             (concat pair
                     (mapcat (partial julia-inverse-simple inverse-f (dec depth))
                             pair))))))

(comment
  (def circle (inv-quad-fn (c/complex 0.2 -0.35)))
  (def f-1 (inv-quad-fn (c/complex 0.7 0.25)))
  (def ps (julia-inverse-simple circle 1 (c/complex 0.0 1.0)))
  (def ps (julia-inverse-simple circle 2 (c/complex 0.0 1.0)))
  (def ps (julia-inverse-simple circle 3 (c/complex 0.0 1.0)))
  (def ps (julia-inverse-simple circle 4 (c/complex 0.0 1.0)))
  (def ps (julia-inverse-simple circle 5 (c/complex 0.0 1.0)))
  (def ps (time (julia-inverse-simple f-1 23 (c/complex 0.0 0.0))))
  (def psalt (time (julia-inverse-simple-alt f-1 23 (c/complex 0.0 0.0))))
  (= psalt ps)
)

;; TODO Is the the logic optimal? Shouldn't recursing on the second element
;; check the set returned from the first element?  Otherwise, there
;; can be duplication between them that's not removed until they
;; both return.
;; TODO Should I use the actual values or the floored values
;; for the recursion?  (Using the latter initially.) How much does it matter?
(defn julia-inverse
  "Use iterations of the inverse of a quadratic function f to identify
  points in f's Julia set, skipping points that within gap distance
  from points already collected.  More precisely, points are kept if they
  are still new after being floored to a multiple of gap.  This is
  based on the second algorithm in Mark McClure's
  \"Inverse Iteration Algorithms for Julia Sets\".  (gap is the reciprocal
  of McClure's resolution.) depth must be >=1."
  [gap inverse-f depth z]
  (letfn [(inv-recur [curr-zs curr-depth curr-z]
            (let [poss-new-vals (clip-into-set gap (inverse-f curr-z))
                  new-vals (s/difference poss-new-vals curr-zs)
                  zs (s/union new-vals curr-zs)]
              ;(print [curr-depth (count zs)])(flush) ; DEBUG
              (if (== curr-depth 1)
                (set new-vals)  ; i.e. make a set out of it (this doesn't set anything)
                (apply s/union new-vals
                       (doall (map  ; replacing with pmap quickly fails ("unable to create new native thread")
                                (partial inv-recur zs (dec curr-depth))
                                new-vals))))))]
    (seq (inv-recur #{} depth z))))

(comment
  (def f-1 (inv-quad-fn (c/complex 0.0 0.68)))
  (def zs (time (julia-inverse 0.01 f-1 2 c/ZERO)))
  (def zs (time (julia-inverse 0.01 f-1 24 c/ZERO)))
  (count zs)
  (f-1 (c/complex 0.58 -0.59))
  (f-1 (c/complex 0.99 -0.64))
  (f-1 (c/complex -1.0 0.63))
)


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
  (def zs ps)
  (def vl-zs (doall (h/add-point-labels "Julia set" zs)))
  (def julia-plot (time (h/vega-food-plot vl-zs 500 800 1)))
  (def julia-plot (time (h/vega-food-plot (h/add-point-labels "Julia set" zs) 1000 1000 1)))
  (oz/view! julia-plot)
  (time (oz/view! (h/vega-food-plot (h/add-point-labels "Julia set" zs) 500 800 1)))
  (time (oz/view! (h/vega-food-plot (h/add-point-labels "Julia set" zs) 1000 1000 1)))

  (let [[x c] (c/complex 5 4)] [x c])

)




;; TODO UNIMPLEMENTED
(defn julia-corners
  "Algorithm for approximating a (non-filled) Julia set, sketched in 
  Falconer's _Fractal Geometry_, 3d ed, pp. 255f."
  [x-min x-max c-min c-max max-iters f]
  (println "UNIMPLEMENTED"))
