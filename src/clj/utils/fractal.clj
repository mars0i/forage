;; Mathematical operations to create or manipulate fractal structures
(ns utils.fractal)


;; note it's not enough to recursively apply the functions to the points,
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
(defn old-fournierize
  "Given a sequence of coordinate pairs (points), returns a sequence containing
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



(comment
  (middle-third-cantor 3 [0 1])
  (middle-third-cantor 2 [0.0 1.0])
)
