;; Mathematical operations to create or manipulate fractal structures
(ns utils.fractal
  (:require [fastmath.complex :as c]
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

(defn inv-quad-fn
  "Returns an implementation of the inverse of the quadratic function
  f(z) = z^2 + c, i.e. returns f^{-1}(z) = sqrt(z - c), where z and c
  are instances of fastmath.complex numbers, i.e. Vec2's.  Returns a pair
  containg the positive and negative values of the square root."
  [c]
  (fn [z] 
    (let [posroot (c/sqrt (c/sub z c))]
      [posroot (c/neg posroot)])))

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


;; FIXME Draft.  probably wrong.
(defmacro p-filled-julia-v1
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  (let [x-subrange (/ (- x-max x-min) n-threads)
        x-mins (range x-min x-max x-subrange)
        x-maxs (conj (vec (rest x-mins)) x-max)
        filled-julia-forms# (map (fn [xmn xmx]
                                   ;(list 'filled-julia xmn xmx c-min c-max step max-iters esc-bound f))
                                   `(filled-julia xmn xmx c-min c-max step max-iters esc-bound f))
                                 x-mins x-maxs)]
    `(into [] cat (pvalues @filled-julia-forms#)))) ; cat concatenates.  This realizes the for's.


;; FIXME Draft.  probably wrong.
(defmacro p-filled-julia-v2
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  `(let [x-subrange# (/ (- x-max x-min) n-threads)
        x-mins# (range x-min x-max x-subrange#)
        x-maxs# (conj (vec (rest x-mins#)) x-max)
        filled-julia-forms# (map (fn [xmn# xmx#]
                                   (list 'filled-julia xmn# xmx# c-min c-max step max-iters esc-bound f))
                                   ;(filled-julia xmn# xmx# c-min c-max step max-iters esc-bound f))
                                 x-mins# x-maxs#)]
    (into [] cat (pvalues @'filled-julia-forms#))
    ;(into [] cat (pvalues @(construct-filled-julia-forms 
    ;                          n-threads x-min x-max c-min c-max step max-iters esc-bound f)))
    ;nil
    )) ; cat concatenates.  This realizes the for's.


(defn construct-filled-julia-forms
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  (let [x-subrange (/ (- x-max x-min) n-threads)
        x-mins (range x-min x-max x-subrange)
        x-maxs (conj (vec (rest x-mins)) x-max)]
    (map (fn [xmn# xmx#]
           `(filled-julia xmn# xmx# c-min c-max step max-iters esc-bound f))
         x-mins x-maxs)))

;; FIXME Draft.  probably wrong.
(defmacro p-filled-julia-v3
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  `(into [] cat (pvalues @(construct-filled-julia-forms 
                             n-threads x-min x-max c-min c-max step max-iters esc-bound f))))


;; FIXME Draft.  probably wrong.
(defmacro p-filled-julia-v4
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  (concat '[into [] cat]
          (cons 'pvalues (construct-filled-julia-forms 
                           n-threads x-min x-max c-min c-max step max-iters esc-bound f))))

;; FIXME Draft.  probably wrong.
(defmacro p-filled-julia-v5
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  `(into [] cat (pvalues @(construct-filled-julia-forms 
                             'n-threads 'x-min 'x-max 'c-min 'c-max 'step 'max-iters 'esc-bound 'f))))

;; FIXME Draft.  probably wrong.
(defn p-filled-julia-v6
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  (let [x-subrange (/ (- x-max x-min) n-threads)
        x-mins (range x-min x-max x-subrange)
        x-maxs (conj (vec (rest x-mins)) x-max)]
    (into [] cat
          (apply pcalls
                 (map (fn [xmn xmx]
                        #(filled-julia xmn xmx c-min c-max step max-iters esc-bound f))
                      x-mins x-maxs)))))

(defn p-filled-julia
  [n-threads x-min x-max c-min c-max step max-iters esc-bound f]
  (let [x-subrange (/ (- x-max x-min) n-threads)
        x-mins (range x-min x-max x-subrange)
        x-maxs (conj (vec (rest x-mins)) x-max)]
    (into [] cat
          (pmap (fn [xmn xmx]
                  (filled-julia xmn xmx c-min c-max step max-iters esc-bound f))
                x-mins x-maxs))))


(comment
  (def a -2) (def b 2) (def c -1) (def d 1)
  (def f1 (quad-fn (c/complex 0.0 0.066)))

  (clojure.pprint/pprint
   (construct-filled-julia-forms 4
                                 a
                                 b
                                 c
                                 d
                                 0.01 10 2 f1))

  (clojure.pprint/pprint (macroexpand-1 
                          '(p-filled-julia 4
                                           -2
                                           2
                                           -1
                                           1
                                           0.01 10 2 f1)))

  (clojure.pprint/pprint (macroexpand-1 
                          '(p-filled-julia 4
                                           a
                                           b
                                           c
                                           d
                                           0.01 10 2 f1)))
)

(comment
  (def yo (p-filled-julia 2 -2 2 -1 1 0.01 10 2 f1))
  (macroexpand-1 '(p-filled-julia 4 -2 2 -1 1 0.01 10 2 f))
  (macroexpand-1 '(defn yo [x] (* x x)))
)

(defn complex-to-vecs
  "Convenience function convert a collection of fastmath.complex numbers
  to a sequence of Clojure vector pairs by calling vec->Vec."
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

  ;; Disconnected Julia set from pp. 253, 254 of Falconer's _Fractal Geometry_
  (def f1 (quad-fn (c/complex 0.0 0.66)))
  (def zs (time (doall (-> (p-filled-julia 8 -2 2 -1.5 1.5 0.001 10 5 f1) (complex-to-vecs)))))
  (count zs)

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (def vl-zs (doall (h/add-point-labels "Julia set" zs)))
  (def julia-plot (time (h/vega-food-plot vl-zs 500 800 1)))
  (def julia-plot (time (h/vega-food-plot (h/add-point-labels "Julia set" zs) 1000 1000 1)))
  (oz/view! julia-plot)
  (time (oz/view! (h/vega-food-plot (h/add-point-labels "Julia set" zs) 1000 1000 1)))

)


;; TODO UNIMPLEMENTED
(defn julia-inverse
  "Use iterations of the inverse of a quadratic function f to identify points
  in f's Julia set.  See e.g. Falconer's _Fractal Geometry_, 3d ed, p. 255."
  [initial-val inverse-f]
  (println "UNIMPLEMENTED"))

;; view-source:https://marksmath.org/visualization/julia_sets/JuliaSetApp.js:
;;
;; // Draw the Julia set using an inverse iteration algorithm
;; function draw_inverse_iterates(c, draw_canvas) {
;; 	"use strict";
;; 	var bailout = 10;
;; 	var draw_context = draw_canvas.getContext("2d");
;; 	var plot_record = new Array(draw_canvas.height);
;; 	for(var cnt=0; cnt<draw_canvas.height; cnt++) {
;; 		plot_record[cnt] = new Array(draw_canvas.width);
;; 	}
;; 	for(var row = 0; row < draw_canvas.height; row++) {
;; 		for(var col = 0; col < draw_canvas.width; col++) { 
;; 			plot_record[row][col] = 0;
;; 		}
;; 	}
;; 	var z0 = math.complex(0.12,0.34);
;; 	for(var cnt = 0; cnt<5; cnt++) {
;; 		z0 = math.sqrt(math.add(z0,math.multiply(c,-1)));
;; 		z0 = math.multiply(math.sqrt(math.add(z0,math.multiply(c,-1))),-1);
;; 	}
;; 	var zNode = new JuliaTreeNode(z0);
;; 	var queue = [zNode];
;; 	var cnt = 0;
;; 	while(queue.length > 0) {
;; 		zNode = queue.pop();
;; 		var z = zNode.z;
;; 		var ij = complex_to_canvas(z, -2,2, -2,2, draw_canvas);
;; 		var i = ij[0];
;; 		var j = ij[1];
;; 		if(plot_record[i][j] < bailout) {
;; 			draw_context.fillRect(i,j,1,1);
;; 			var z1 = math.sqrt(math.add(z,math.multiply(c,-1)));
;; 			var z2 = math.multiply(z1,-1);
;; 			var left = new JuliaTreeNode(z1);
;; 			var right = new JuliaTreeNode(z2);
;; 			zNode.left = left;
;; 			zNode.right = right;
;; 			queue.push(left);
;; 			queue.push(right);
;; 			plot_record[i][j] = plot_record[i][j]+1;
;; 		}
;; 	}
;; }
;; 
;; // The inverse iteration algorithm constructs the Julia set as a
;; // binary tree with these types of nodes.
;; function JuliaTreeNode(z /* , left, right */) {
;; 	this.z = z || 0;
;; 	this.left = "empty";
;; 	this.right = "empty";
;; }



;; TODO UNIMPLEMENTED
(defn julia-corners
  "Algorithm for approximating a (non-filled Julia set sketched in Falconer's
  _Fractal Geometry_, 3d ed, pp. 255f."
  [x-min x-max c-min c-max max-iters f]
  (println "UNIMPLEMENTED"))
