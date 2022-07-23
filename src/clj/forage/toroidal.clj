;; Functions to manipulate search paths so that they "wrap" as if 
;; in a toroidal environment, i.e. one with periodic boundary conditions.
(ns forage.toroidal
  (:require [utils.math :as m]))


;; SEE doc/ToroidalAlgorithms.md for explanation of the less obvious
;; algorithms below.  It might also be helpful to look at
;; doc/exceedingboundaries1.pdf, although that only represents certain cases.


;; VOCABULARY AND NOTATION:
;;
;;  - The "standard region" or "region" is the area that counts--it's what 
;;    would be displayed, or it's the environment in which agents move.  The
;;    result we want is that we can at least display lines as "wrapping" back
;;    to the other side of the region if they exist out at one side.
;;  - "seg" means line segment, i.e. a pair of coordinate pairs.
;;  - "bound-" means "boundary-", or as in "upper bound". 
;;     (It has nothing to do with binding.)
;;  - "sh" means "shifted".
;;  - "dir" means "direction".
;;  - sequences of points (coordinate pairs) are called either "points" or "pts".
;;  - "wrap" as in "wrap around": cause a line that leaves the standard region 
;;    on one side to come back in on the other side.

;; In this version of this code, the standard region must be a square that
;; lies between bound-min and bound-max in each of the two dimensions.


;; I recommend that one begin reading at either wrap-segs or wrap-paths.
;; The second is a wrapper (different sense) for the first.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-SEGS AND ITS SUPPORT FUNCTIONS
;; Called by wrap-path.


;; Note: the two parts of this function can't be replaced by 
;; clojure.core/compare in any simple way, because the comparison
;; should return 0 for any values in the range [bound-min bound-max].
(defn seg-dirs
  "Returns a pair in which each element is -1, 0, or 1.  The first element
  indicates the direction in which seg is going, from first point to second
  point.  This indicates whether it crosses bound-min (-1), bound-max (1),
  or neither (0).  Note that the resulting value will be the opposite of the 
  direction in which the x coordinates of seg must be shifted for the forward
  point of the shifted segment to be closer to within [bound-min bound-max].
  The second element returned by this function reports the same relationship
  for seg's y coordinates."
  [bound-min bound-max seg]
  (let [[_ [x2 y2]] seg]
    [(cond (< x2 bound-min) -1
           (> x2 bound-max) 1
           :else 0)
     (cond (< y2 bound-min) -1
           (> y2 bound-max) 1
           :else 0)]))


;; SEE doc/ToroidalAlgorithms.md for explanation
(defn choose-shifts
  "Returns a
  pair of shift values, for x and y, after determining whether one of the
  forward coordinates exceeds a boundary in dimension D at a location
  that is outside of a boundary in the other dimension.  That would mean
  that seg should not be shifted in dimension D, but only in the other
  dimension.  Shifts in both directions are appropriate only when a line
  segment goes through a corner of the standard region.  (See
  doc/exceedingboundaries1.pdf for an illustration in which the upper
  segment should be shifted down but not left, the lower segment should
  be shifted left but not down, and the dashed segment should be shifted
  in both directions.)  Note that this functon assumes that the first 
  point in the segment is within [bound-min bound-max] in both dimensions."
  [bound-min bound-max seg]
  (let [[pt1 pt2] seg
        [x1 y1] pt1
        [x2 y2] pt2
        [x-dir y-dir] (seg-dirs bound-min bound-max seg)]
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


;; SEE doc/ToroidalAlgorithms.md for explanation
;;
;; This algorithm and the one in seg-dirs are derived from generateme's
;; correct-path function at 
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;; https://github.com/generateme/cljplot/blob/f272932c0228273f293a834e6c19c50d0374d3da/sketches/examples.clj#L572
(defn wrap-segs
  "Given a sequence of line segments--pairs of pairs of numbers--representing
  a path connected by line segments, returns a transformed sequence in which
  segments whose second point that would go beyond the boundaries are
  \"duplicated\" with a new segment that is the previous version shifted so
  that, if it's short enough, the duplicate ends within boundaries.  If it
  doesn't, then another \"duplicate\" will be created that's further shifted,
  and so on, until there is a duplicate that ends within the boundaries.
  \"Duplicate\" segments are separated by nils."
  [bound-min bound-max segments]
  (let [width (- bound-max bound-min)]
    (loop [new-segs [], sh-x 0.0, sh-y 0.0, segs segments]
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-PATH AND ITS SUPPORT FUNCTIONS (except for wrap-segs, which is above)
;;
;; Takes a sequence of points, turns it into a sequence of segments,
;; passes that to wrap-segs, and takes the output and turns it into a
;; sequence of points with delimiters between shifted sequences.


;; SEE doc/ToroidalAlgorithms.md for explanation
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

;; TODO Maybe a trick would be to add a fake segment at the beginning
;; that goes from [0,0] to the start point of the real first segment.
;; Then remove that fake segment, and any other segments that got
;; created for it, at the end.
(defn fix-first-seg
  "If first segment begins outside the boundaries, shifts it in."
  [bound-min bound-max points]
  ;; TODO
  points)

(defn wrap-path
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [bound-min bound-max points]
  (->> (points-to-segs points)
       ;(fix-first-seg bound-min bound-max)
       (wrap-segs bound-min bound-max)
       (segs-to-points)))
