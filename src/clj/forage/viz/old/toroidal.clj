;; Toroidal ("periodic boundary conditions") wrapping of coordinates for viz
(ns forage.viz.toroidal
  (:require [utils.math :as ma]
            [utils.misc :as mi]
            [clojure.math.numeric-tower :as nt]))

(println "DON'T USE forage.viz.toroidal!")
(println "It's not worth it.  Just make your worlds large, or don't
         bother to plot lines toroidally.  Or maybe use MASON for
         that purpose (but that will require further work).")

;; These are oriented toward plotting with Vega-Lite/Hanami, and they
;; shouldn't be needed for data analysis; maybe they will be useful
;; for some other future visualization methods as well.

;; NOTE: *Finding* foodspots in a toroidal world should be handled in a 
;; look-fn (e.g. defined in forage.mason.foodspot) tha is passed to one of
;; the functions in forage.walks, thereby preserving the original coordinates.

;; For example data to experiment with to understand these functions,
;; see one of the files in the test/forage directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; GENERAL IDEA OF THE NEXT FEW FUNCTIONS:
;; What we want is to divide a sequence of points representing a walk
;; into segments that will wrap the points toroidally in a displayed
;; environment.  The following might not be the simplest algorithm, but
;; it has the advantage that the distinct functions might at some point
;; be separately useful.
;;
;; Step 1: Split the sequence into subsequences where points exceed boundaries
;;  (toroidal-partition).
;;
;; Step 2: Mod them so that all points fall within the display boundaries
;;  (wrap-stops-toroidally).
;;
;; At this point, the sub-walks will stop before they reach the boundaries.
;; That is, there's a gap between the last point before the boundary-
;; crossing, and the next point.  We need to do something about that.
;;
;; Step 3: Modify adjacent subsequences so that the earlier one includes
;;  the first element of the later one, and vice versa (overlap-ends).
;; 
;; At this point if we plot the subwalks, some lines will go off the edge of 
;; designated the plot region.  (In Vega-Lite these will actually be plotted
;; out over the edge unless you set "autosize" "type" to "none"; that might
;; possibly be useful in some context.)  However, normally, the lines should
;; stop and start at the edge of the plot.
;; 
;; Step 4: Overwrite the endpoints of the subwalks so that they stop
;; at the edge of the plot area.


(defn toroidal-partition
  "Maps a sequence of coordinates representing stops on a walk path into
  a sequence of shorter sequences that map large coordinates back to the
  original dimensions toroidally (aka: with periodic boundary
  conditions).  Specifically, splits a sequence of coordinates when they
  exceed boundaries of an environment of width env-width and height
  env-height, or when they wrap around and exceed the boundaries a
  second time, etc."
  [maxx maxy stops]
  (let [x-split-paths (partition-by #(quot (first %) maxx) stops) ; when beyond border again, split
        xy-split-paths (mapcat
                         (fn [subpath] 
                             (partition-by #(quot (second %) maxy) subpath))
                         x-split-paths)]
    xy-split-paths))

(defn overlap-ends
  "Given a sequence of paths (each a sequence of coordinate pairs), adds
  the first element of each path to the end of the preceding path, and
  the last element of each path to the beginning of the next path
  (except where this is impossible for the paths on the end of the
  sequence).  The result will be that the last two elements of each path
  will be identical to the first to elements of the next path." [paths]
  [paths]
  (let [paths (vec paths)
        ;; For each path that's not first or last, add last and first elements
        ;; of adjacent paths.  (First and last paths need special handling.) 
        middle-paths (map (fn [[before middle after]]
                              (cons (last (vec before))
                                    (conj (vec middle) (first
                                                         after))))
                          (partition 3 1 paths))
        ;; These were left out of middle-paths:
        end-path (cons (last                        ; Add last element of
                         (mi/second-to-last paths)) ; of second to last seq
                       (last paths))                ; to last seq.
        beg-path (conj (vec (first paths))       ; Add to end of first seq
                       (first (second paths)))]  ; the first element in second.
    (cons beg-path
          (conj (vec middle-paths)
                end-path))))


;; This one is good, but doesn't implement the above.  It implements rem
;; but maps x to m or -m when it's an odd-numbered multiple of m.
;; (Even-numbered multiples should map to zero.)
;(defn rem*
;  [x m]
;  (let [remx (rem x m)]
;    (if (zero? remx)
;      (if (even? (quot x m))
;        0
;        (if (neg? x) (- m) m))
;      remx)))

(defn unshift-fn
  [x]
  (if (neg? x) + -))

;; Seems to work right for positive numbers ...
;(defn rem-
;  [x env-size]
;  (let [env-max (/ env-size 2)
;        shifted-x (+ x env-max)
;        shifted-x-rem (rem shifted-x env-size)]
;    ((unshift-fn x) shifted-x-rem env-max)))

;; The purpose of this function is to make numbers that are exactly on the 
;; edge of an environment, or on an "edge" that is some multiple of it, 
;; from being mapped into zero.
;; Desired output for env-width 10, i.e. maxx = 5:
;     0 -> 0
;     1 -> 1
;     2 -> 2
;     3 -> 3
;     4 -> 4
;     5 -> 5  [-5 might work, too]
;     6 -> -4
;     7 -> -3
;     8 -> -2
;     9 -> -1
;     10 -> 0
;     11 -> 1
;     12 -> 2
;     13 -> 3
;     14 -> 4
;     15 -> 5  [-5 might work, too]
;     16 -> -1
;     ...
;     -1 -> -1
;     -2 -> -2
;     -3 -> -3
;     -4 -> -4
;     -5 -> -5  [5 might work, too]
;     -6 -> 4
;     -7 -> 3
;     -7 -> 3
;; i.e. when a number is wrapped, if it's a little bit over, it
;; should wrap to the other side--negative if it was positive, positive
;; if it was negative.
;; (MASON's Continuous2D simplifies this by putting the origin in the corner.)

;(defn rem+old
;  [x env-size] ; env-size = width or height
;  (let [env-max (/ env-size 2) ; since env symmetric from neg to pos
;        abs-x (nt/abs x) ; to avoid confusion, just work with pos nums
;        shifted-x (+ abs-x env-max) ; result is outside bounds for all x
;        shifted-x-rem (rem shifted-x env-size) ; now rem without confusion
;        unshifted (- shifted-x-rem env-max)] ; but we have to go back
;    (if (neg? x)      ; and we have to put neg nums back to neg
;      (- unshifted)
;      unshifted)))

;(defn rem+not
;  [x env-size] ; env-size = width or height
;  (let [env-max (/ env-size 2) ; since env symmetric from neg to pos
;        shifted-x (+ x env-max) ; result is outside bounds for all x
;        shifted-x-rem (rem shifted-x env-size) ; now rem without confusion
;        unshifted (- shifted-x-rem env-max)] ; but we have to go back
;    unshifted))

;; Based on tx() in MASON's Continuous2D.java
;(defn mason-toroidal
;  [x size]
;  (if (and (>= x 0) (< x size))
;    x
;    (let [modx (mod x size)]
;      (if (neg? modx)
;        (+ modx size)
;        modx))))

;(defn rem+
;  [x env-size] ; env-size = width or height
;  (let [env-max (/ env-size 2) ; since env symmetric from neg to pos
;        shifted-x (+ x env-max) ; result is outside bounds for all x
;        shifted-x-rem (rem shifted-x env-size) 
;        unshifted-x (- shifted-x-rem env-max)] ; but we have to go back
;    unshifted-x))

(defn rem+
  [x m]
  (let [remx (rem x m)]
    (if (zero? remx) ; if x on border, leave it there; don't replace with zero
      m
      remx)))

;; BUG: only handles a single env-width.  What if a point is beyond twice the width?
(defn wrap-stops-toroidally
  "Map coordinates in a sequence of points to their values mod maxx and maxy."
  [env-width env-height stops]
  (map (fn [[x y]] [(rem+ x env-width) (rem+ y env-height)])
       stops))

(defn clip-to-env
  "Returns [x y] if x and y lie within [-maxx,maxx] and [-maxy,maxy],
  respectively; otherwise replaces x or y with the nearest of the
  extremes, and adjusts the other coordinate so that the returned
  point is on the same line."
  [minx maxx miny maxy nextto-pt end-pt] ; end-point is at end of subwalk, before-point is one next to it
  (let [[x y] end-pt
        slope (ma/slope-from-coords nextto-pt end-pt)
        newx (cond (> x maxx)  maxx
                   (< x minx) minx
                   :else x)
        newy (cond (> y maxy)  maxy
                   (< y miny) miny
                   :else y)]
    (cond (and (not= x newx)
               (not= y newy)) [newx newy] ; line runs through corner
          (not= x newx) [newx  ; double-wrapped: Vega-Lite doesn't like Clojure's funny numbers
                         (double (+ (* slope newx) ; slope can't be nil since line crossed left or right vertical border
                                    (ma/intercept-from-slope slope [x y])))]
          (not= y newy) (if (not (ma/pos-inf? slope)) ; if not vertical
                          (let [yslope (/ slope)]
                            [(double
                               (+ (* yslope newy)
                                  (ma/intercept-from-slope yslope [y x])))
                             newy])
                          [x newy]) ; line is vertical, so x doesn't change
          :else [x y])))


;; One could instead simply map clip-to-env over all of the points in ech
;; subwalk, since only the endpoints will exceed extremes, but it's not
;; hard to just do the endpoints.
(defn clip-ends-to-env
  "Returns path modified by applying (clip-to-end maxx maxy ...) to its 
  first and last points."
  [minx maxx miny maxy path]
  (let [path (vec path)
        left (clip-to-env minx maxx miny maxy (second path) (first path))
        right (clip-to-env minx maxx miny maxy
                           (mi/second-to-last path)
                           (last path))
        middle (vec (rest (butlast path)))]
    (cons left (conj middle right))))


;; FIXME? behaves oddly if env-width, env-height are not even?
;; See forage/core_test.clj for a test of this function.
(defn toroidal-wrapped-partition
  "Maps a sequence of coordinates representing stops on a walk path into
  a sequence of shorter sequences that map large coordinates back to the
  original dimensions toroidally (aka: with periodic boundary conditions).
  Specifically, splits a sequence of coordinates when they exceed boundaries
  of an environment of width env-width and height env-height, or when they
  wrap around and exceed the boundaries a second time, etc.  Then after
  splitting the original sequence of coordinates into a sequence of sequences
  in this way, adds the first element of each path after the first to the end
  of the preceding path, and returns the resulting modified sequence of paths.
  Finally, goes through the inner sequences and replaces coordinates by
  coordinates mod env-width/2 and env-height/2 [since (0,0) is in the center
  of the environment], and reduces values on the ends so that they stop at
  the environment boundaries."
  [env-width env-height stops]
  (let [maxx (/ env-width  2)  ; rem will preserve neg signs using pos divisor
        maxy (/ env-height 2)]
    ;; could be more efficient with comp or transducer, but this is just for prep'ing data for display
    ;; FIXME Is this right??? :
    (->> stops
         (toroidal-partition env-width env-height)
         (overlap-ends)
         ;(map (partial clip-ends-to-env 0 env-width 0 env-height))
         ;(map (partial wrap-stops-toroidally env-width env-height))
         )))
