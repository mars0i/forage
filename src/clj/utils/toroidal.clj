;; Functions to manipulate search paths so that they "wrap" as if 
;; in a toroidal environment, i.e. one with periodic boundary conditions.
(ns utils.toroidal
  (:require [utils.math :as m]))


;; SEE doc/toroidal/Toroidal.md for usage, vocabulary, notation, and
;; explanation of less obvious algorithms below.

;; It might also be helpful to look at
;; doc/exceedingboundaries1.pdf, although that only illustrates certain cases.

;; I recommend that one begin reading below at either wrap-segs or wrap-paths.
;; The second is a wrapper (different sense) for the first.

(declare add-walk-labels choose-shifts full-shift nil-delimited-to-ptseqs 
         outside-dir points-to-segs segs-to-points shift-seg 
         toroidal-to-cljplot toroidal-to-vega-lite wrap-path wrap-segs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-PATH
;; Takes a sequence of points, turns it into a sequence of segments,
;; passes that to wrap-segs, and takes the output and turns it into a
;; sequence of points with delimiters between shifted sequences.

(defn wrap-path
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [bound-min bound-max points]
  (->> (points-to-segs points)
       (wrap-segs bound-min bound-max)
       (segs-to-points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-SEGS AND ITS SUPPORT FUNCTIONS
;; Called by wrap-path.

;; SEE doc/Toroidal.md for explanation
;;
;; This algorithm and the one in outside-dirs are derived from generateme's
;; correct-path function at 
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;; https://github.com/generateme/cljplot/blob/f272932c0228273f293a834e6c19c50d0374d3da/sketches/examples.clj#L572
(defn wrap-segs
  "Given a sequence of line segments (pairs of pairs of numbers)
  representing a path connected by line segments, returns a transformed
  sequence in which segments whose second point would go beyond the
  boundaries are \"duplicated\" with a new segment that is like the
  previous version, but shifted so that, if it's short enough, the
  duplicate ends within boundaries.  If it doesn't end within the
  boundaries, another \"duplicate\" will be created that's further
  shifted, and so on, until there is a duplicate that ends within the
  boundaries.  \"Duplicate\" segments are separated by nils."
  [bound-min bound-max segments]
  (let [width (- bound-max bound-min)
        [first-coord-x first-coord-y] (first (first segments))    ; If start point of first seg
        init-sh-x (full-shift bound-min bound-max first-coord-x)  ;  is outside boundaries, shift
        init-sh-y (full-shift bound-min bound-max first-coord-y)] ;  it in (and entire sequence).
    (loop [new-segs [], sh-x init-sh-x, sh-y init-sh-y, segs segments]
      (if-not segs
        new-segs
        (let [new-seg (shift-seg sh-x sh-y (first segs))
              [x-sh-dir y-sh-dir] (choose-shifts bound-min bound-max new-seg)
              [new-sh-x new-sh-y] [(+ sh-x (* x-sh-dir width))
                                   (+ sh-y (* y-sh-dir width))]]
          (if (and (== new-sh-x sh-x)
                   (== new-sh-y sh-y))
            (recur (conj new-segs new-seg)
                   new-sh-x new-sh-y
                   (next segs))
            (recur (conj new-segs new-seg nil)
                   new-sh-x new-sh-y
                   segs))))))) ; This will result in the next iteration adding 
                               ; the same seg after nil, but with new shifts.
                               ; We'll keep doing that until the forward end
                               ; (new-x2, new-y2) no longer goes beyond boundary.


;; Note this can't be replaced by clojure.core/compare in any simple
;; way, because the comparison should return 0 for any values in the range
;; [bound-min bound-max].
(defn outside-dir
  "Returns -1, 0, or 1, indicating whether v strictly below bound-min (-1),
  strictly above bound-max (1), or between them, inclusive (0).  For example,
  if v is a coordinate in the end point of a segment, the value indicates
  which boundary, if any, the segment crosses in the given dimension."
  [bound-min bound-max v]
  (cond (< v bound-min) -1
        (> v bound-max)  1
        :else 0))


;; Note there's a sort of assymetry: 
;; Does not return nonzero shift if coord is equal to either boundary (a consequence 
;; of the def of outside-dir).
;; So the first set of outside shifts is smaller than the rest.
;; Is that right?  Should I be using non-strict inequality somewhere?
;; Note that the two boundaries are kind of logically equal.  Should one
;; be favored over the other?
;;
;; Note: This differs from the numbers returned by choose-shifts because
;; the return value of full-shift will immediately shift into the region,
;; while choose-shifts returns numbers for incrementally shifting, one
;; segment at a time.
(defn full-shift
  "coord is a number representing one coordinate of a point.  Returns a
  number that when added to coord, shifts it into a correct relative
  location within [bound-min bound-max].  (This differs from the numbers
  returned by choose-shifts because the return value of full-shift can be
  used to immediately shift into the region, while choose-shifts returns 
  numbers for incrementally shifting, one \"duplicated\" segment at a time.)"
  [bound-min bound-max coord]
  (let [dir (outside-dir bound-min bound-max coord)]
    (if (zero? dir)
      0
      (let [width (- bound-max bound-min)
            bound (if (pos? dir) bound-max bound-min)
            dist-from-region (if (pos? dir)  ; n-widths will be < 0 if dividend is
                               (- coord bound-max) ; how far outside bound is coord?
                               (- bound-min coord)) ; ditto
            n-widths (quot dist-from-region width)
            shift-dir (- dir)]
        (* shift-dir (inc n-widths) width)))))

(comment
  (map (partial full-shift 3 7) (range -9 20 0.5))
  (map (partial full-shift -7 -3) (range -20 10 0.5))
  (map (partial full-shift -2 2) (range -10 11 0.5))
)


;; SEE doc/Toroidal.md for explanation
;;
;; Note: The numbers returned by this function differ from the number
;; returned by full-shift, which immediately shifts into the region,
;; while choose-shifts returns numbers for incrementally shifting, one
;; segment at a time.
(defn choose-shifts
  "Returns a pair in which each element is -1, 0, or 1, If no boundary
  is exceeded, no shift is needed, so [0 0] is returned.  If only one
  boundary is exceeded, -1 is returned if it's bound-min, or 1 if it's
  bound-max; the other element of the pair will be zero.  If both
  boundaries are exceeded, and the line segment went exactly through a
  corner of the region, both elements of the returned pair will be
  nonzero.  Otherwise, the end point of the segment exceeds one boundary
  at a location that is beyond both boundaries in the other dimension.
  Only one element of the returned pair will be nonzero: the one
  corresponding to the boundary that was crossed between the boundaries
  in the other dimension.  See doc/exceedingboundaries1.pdf.  (The numbers
  returned by this function differ from the number returned by full-shift,
  which can be used to immediately shift into the region, while 
  choose-shifts returns numbers for incrementally shifting, one
  \"duplicated\" segment at a time.)"
  [bound-min bound-max seg]
  (let [[pt1 pt2] seg
        [x1 y1] pt1
        [x2 y2] pt2
        x-dir (outside-dir bound-min bound-max x2)
        y-dir (outside-dir bound-min bound-max y2)]
    (if (or (zero? x-dir) (zero? y-dir)) ; if <= one boundary exceeded (includes vertical, horoizontal)
      [(- x-dir) (- y-dir)] ; simple shift or no shift; at least one of those = 0
      ;; Now forward point must exceed bounds in both dims (cf. doc/exceedingboundaries1.pdf):
      (let [x-bound (if (pos? x-dir) bound-max bound-min)
            y-bound (if (pos? y-dir) bound-max bound-min)
            slope (m/slope-from-coords pt1 pt2)              ; prepare to calculate line
            intercept (m/intercept-from-slope slope [x1 y1]) ;  function along seg
            y-at-x-bound (+ (* slope x-bound) intercept)  ; y coord of line at x-bound
            x-at-y-bound (/ (- y-bound intercept) slope)] ; x coord of line at y-bound
        (cond (and (> y-at-x-bound bound-min)         ; if seg goes through x-bound edge
                   (< y-at-x-bound bound-max)) [(- x-dir) 0] ; then shift horizontally back
              (and (> x-at-y-bound bound-min)         ; if seg goes through y-bound
                   (< x-at-y-bound bound-max)) [0 (- y-dir)] ; shift vertically back
              :else [(- x-dir) (- y-dir)]))))) ; else seg runs through corner, so shift both


(defn shift-seg
  "Given a line segment seg (represented by its 2D endpoints as a pair
  of pairs of numbers), returns a version of the line segment in which
  the x-coordinates have been shifted by the addition of sh-x, and
  the y-coordinates have been shifted by the addition of sh-y."
  [sh-x sh-y seg]
  (let [[[x1 y1] [x2 y2]] seg]
    [[(+ x1 sh-x) (+ y1 sh-y)]
     [(+ x2 sh-x) (+ y2 sh-y)]]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-PATH SUPPORT FUNCTIONS (except for wrap-segs)

;; SEE doc/Toroidal.md for explanation
(defn segs-to-points
  "segs is a sequence of line segments (pairs of pairs representing 2D
  coordinates), possibly separated by nils, where each second coordinate
  is identical to  the next first coordinate, except across nils. Extracts
  the sequence of coordinate pairs (without duplicating common second and
  first endpoints), separated by nils where the source line segments were
  separated."
  [segs]
  (cons (first (first segs))
        (loop [pts [], more-segs segs]
          (cond (empty? more-segs) pts
                (= (count more-segs) 1) (conj pts (second (first more-segs))) ; will not be post-nil
                :else (let [seg (first more-segs)]
                        (if (nil? seg)
                          (let [[pt1 pt2] (second more-segs)]
                            (recur (conj pts nil pt1 pt2)
                                   (drop 2 more-segs))) ; i.e. drop the nil and seg we just used
                          (recur (conj pts (second seg))
                                 (rest more-segs)))))))) ; don't use next--we already deal with nils


(defn points-to-segs
  "Given points, a sequence of 2D coordinate pairs, returns a sequence
  of line segments (pairs of coordinate pairs) connecting these points,
  in sequence [a wrapper for (partition 2 1 points)]."
  [points]
  (partition 2 1 points))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVERSION/ADAPTER FUNCTIONS
;; Functions to convert output of wrap-path into appropriate
;; formats for plotting libraries

(defn toroidal-to-cljplot
  "Given a path generated by toroidal/wrap-path in pts, in which
  \"duplicated\" sequences of points are separated by nils, replaces
  the nils with cljplot separators [#NaN ##NaN]."
  [pts]
  (replace {nil [##NaN ##NaN]} pts))


(defn toroidal-to-vega-lite
  "Given a path generated by toroidal/wrap-path in pts, in which
  \"duplicated\" sequences of points are separated by nils, converts the
  sequence to Vega-Lite's format for sequenced lines.  Points in each
  sequence of connected lines need to share a field, \"label\", that is
  different from other sequences of connected lines; this is what allows
  distinct, disconnected sequences connected lines.  base-label is a
  string that will be the initial part of each label value."
  [base-label pts]
  (let [ptseqs (nil-delimited-to-ptseqs pts)
        ptseq-labels (map (partial str base-label) (range))]
    (flatten
     (map add-walk-labels ptseq-labels ptseqs))))


(defn nil-delimited-to-ptseqs
  "Convert a sequence of nil-delimited coordinates to a sequence of 
  sequences (of points between the nils)."
  [pts]
  (remove (fn [ptseq] (= '(nil) ptseq))
          (partition-by nil? pts)))


(defn add-walk-labels
  "Given a sequence of pairs representing x,y coordinates, returns a
  Vega-Lite data specification in the form of a sequence of maps,
  where the coordinates are values for keys \"x\" and \"x\", and the 
  same \"label\" key and value is added to each map.  In addition,
  each map is given an \"ord\" key with increasing integers as values.
  This can be used with the Vega-Lite \"order\" key."
  [label pts]
  (map (fn [[x y] n] {"x" x, "y" y, "ord" n, "label" label})
       pts (range)))

