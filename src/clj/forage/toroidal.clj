;; Functions to manipulate search paths as if in a toroidal environment,
;; i.e. one with periodic boundary conditions.
(ns forage.toroidal
  (:require [utils.math :as m]))

;; NOTATION:
;;  - "seg" means line segment, i.e. a pair of coordinate pairs.
;;  - "bound-" means "boundary-", or as in "upper bound". 
;;     (It has nothing to do with binding.)
;;  - "sh" means "shifted".
;;  - "dir" means "direction".
;;  - sequences of points (coordinate pairs) are called either "points" or "pts".

;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;
;;  1. What is start is not inside a range? (figure out starting offset)
;;  A: This is not a problem for me, but I have a stub function for a future solution.
;;
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  A: Most of the code below is to address this.
;;
;;  3. What happened to the last point? (probably partition-all should be used)
;;  A: No, this was never a problem.



(defn noop [& _] nil) ; for debugging--can replace a print statement

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-SEGS AND SUPPORT FUNCTIONS
;; Called by wrap-path.


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
    (println "seg-dirs:" "x2,y2:" x2 y2)
    [(cond (< x2 bound-min) -1
           (> x2 bound-max) 1
           :else 0)
     (cond (< y2 bound-min) -1
           (> y2 bound-max) 1
           :else 0)]))

;; ALGORITHM:
;; Each segment treated by this function is assumed to have its
;; first point within bound-min and bound-max in both the x and y
;; dimensions.  Then the segment either:
;;   1. Does not exceed the boundaries at bound-min and bound-max
;;      in any direction.  Then [x-dir,y-dir] in the code = [0,0]
;;      In this case the shift directions to be returned are [0,0].
;;   2. Exceeds one but not the other; i.e. the segment crosses one
;;      boundary. [x-dir,y-dir] = [d,0] or [0,e], where d, e = -1 or 1.
;;      Shift direction to be returned is [-d,0] or [0,-e], respectively.
;;   3. Exceeds both boundaries: [x-dir,y-dir] = [d,e].
;;      Then either:
;;      a. The segment crosses through one boundary, and exceeds the other
;;         simply because after crossing the boundary, it goes so far
;;         in a diagonal direction that its endpoint is past bound-min
;;         or bound-max in that second dimension.  Then we want to shift
;;         in the first dimensionm but not the second.
;;         SEE doc/exceedingboundaries1.pdf FOR A GRAPHICAL ILLUSTRATION.
;;         To determine whether to return [-d,0] or [0,-e], we generate
;;         the formula for a line running through the segment, and see
;;         whether the value of the line function at bound-min, or bound-max
;;         (depending on which is relevant) in one dimension is between
;;         bound-min and bound-max in the other dimension.  If so, then
;;         the segment crosses through the first border, and should be
;;         shifted back so that the next variant of the segment is less
;;         likely (so to speak) to cross the border.  This test may have
;;         to be done in both dimensions.
;;      b. The segment crosses through both boundaries where they meet,
;;         i.e. at a corner.  Then we shift in both dimensions, returning
;;         [-d,-e].
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
  (println "c-s:" seg)
  (let [[pt1 pt2] seg
        [x1 y1] pt1
        [x2 y2] pt2
        [x-dir y-dir] (seg-dirs bound-min bound-max seg)]
    (println "c-s:" "x-dir:" x-dir, "y-dir", y-dir)(flush) ; DEBUG
    (if (or (zero? x-dir) (zero? y-dir)) ; if <= one boundary exceeded (includes vertical, horoizontal)
      (do (println "c-s branch 1")(flush)
      [(- x-dir) (- y-dir)]) ; simple shift or no shift; at least one of those = 0
      ;; Now forward point must exceed bounds in both dims (cf. doc/exceedingboundaries1.pdf):
      (let [x-bound (if (pos? x-dir) bound-max bound-min)
            y-bound (if (pos? y-dir) bound-max bound-min)
            slope (m/slope-from-coords pt1 pt2)              ; prepare to calculate line
            intercept (m/intercept-from-slope slope [x1 y1]) ;  function along seg
            y-at-x-bound (+ (* slope x-bound) intercept)  ; y coord of line at x-bound
            x-at-y-bound (/ (- y-bound intercept) slope)] ; x coord of line at y-bound
        (println "c-s:" "x-bound:" x-bound, "y-bound", y-bound "slope:" slope, "intercept", intercept, "y-at-x-bound:" y-at-x-bound "x-at-y-bound:" x-at-y-bound)(flush) ; DEBUG
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
  (loop [new-segs [], sh-x 0.0, sh-y 0.0, segs segments]
    (println)(flush)
  (let [width (- bound-max bound-min)]
    (if-not segs
      new-segs
      (let [seg (first segs)
              new-seg (shift-seg sh-x sh-y seg)
              [x-sh-dir y-sh-dir] (choose-shifts bound-min bound-max new-seg)
              [new-sh-x new-sh-y] [(+ sh-x (* x-sh-dir width)) (+ sh-y (* y-sh-dir width))]]
          (println "sh-x:" sh-x, "sh-y:" sh-y, "new-sh-x:" new-sh-x, "new-sh-y:" new-sh-y) ; DEBUG
          (if (and (== new-sh-x sh-x)
                   (== new-sh-y sh-y))
            (do (println "branch 1 ")(flush) ; DEBUG
            (recur (conj new-segs new-seg)
                   new-sh-x new-sh-y
                   (next segs)))
            (do (println "branch 2 ")(flush) ; DEBUG
            (recur (conj new-segs new-seg nil)
                   new-sh-x new-sh-y
                   segs)))))))) ; Add same seg after nil, but with new shifts;
                               ; and keep doing that until the forward end
                               ; (new-x2, new-y2) no longer goes beyond boundary.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WRAP-PATH AND SUPPORT FUNCTIONS
;;
;; Takes a sequence of points, turns it into a sequence of segments,
;; passes that to wrap-segs, and takes the output and turns it into a
;; sequence of points with delimiters between shifted sequences.

;; The algorithm is:
;; Always take only the second point of each segment (which is normally the
;; first point of the next segment), except:
;; At the beginning, we need to add on the first point in the first segment,
;; and
;; After a nil, which delimits duplicated-but-shifted points
;;  we also need to add on the first point in the segment after the nil.
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBSOLETE CODE (kept around temporarily for reference)
;; Will be deleted.

;; DEPRECATED
(defn wrap-segs-old
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
        (let [seg (first segs)
              new-seg (shift-seg sh-x sh-y seg)
              [[new-x1 new-y1] [new-x2 new-y2]] new-seg
              new-sh-x (cond (< new-x2 bound-min)  (+ sh-x width)
                                (> new-x2 bound-max) (- sh-x width)
                                :else sh-x)
              new-sh-y (cond (< new-y2 bound-min)  (+ sh-y width)
                                (> new-y2 bound-max) (- sh-y width)
                                :else sh-y)]
          (if (and (== new-sh-x sh-x)
                   (== new-sh-y sh-y))
            (recur (conj new-segs new-seg)
                   new-sh-x new-sh-y
                   (next segs))
            (recur (conj new-segs new-seg nil)
                   new-sh-x new-sh-y
                   segs))))))) ; Add same seg after nil, but with new shifts;
                               ; and keep doing that until the forward end
                               ; (new-x/y2) no longer goes beyond boundary.


;; DEPRECATED
(defn wrap-path-old
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [bound-min bound-max points]
  (->> (points-to-segs points)
       ;(fix-first-seg bound-min bound-max)
       (wrap-segs-old bound-min bound-max)
       (segs-to-points)))

;; DEPRECATED
(defn yo-old-choose-shifts
  "The forward point of seg is assumed to exceed boundaries in both 
  dimensions. Returns a pair of shift values, for x and y, after
  determining whether one of the forward coordinates exceeds a boundary
  in dimension D at a location that is outside of a boundary in the other
  dimension.  That would mean that seg should not be shifted in dimension
  D, but only in the other dimension.  Shifts in both directions are
  appropriate only when a line segment goes through a corner of the
  standard region.  (See doc/exceedingboundaries1.pdf for an illustration
  in which the upper segment should be shifted down but not left, the
  lower segment should be shifted left but not down, and the dashed
  segment should be shifted in both directions.)"
  [bound-min bound-max x-dir y-dir seg]
  ; PSEUDOCODE WITH DUMMY VALUES:
  (let [slope 0 ; calculate slope, i.e. vector direction of seg
        x-at-y-bound 0 ; use slope, first point of seg to calculate x position on line at relevant y boundary
        y-at-x-bound 0 ; use slope, first point of seg to calculate y position on line at relevant x boundary
        x-dir' (if true ; if x position at y boundary is within or equal to x boundaries,
                 x-dir  ;   we'll return x-dir
                 0)     ;   otherwise we'll return 0 for the x direction
        y-dir' (if true ; if y position at y boundary is within or equal to y boundaries,
                 y-dir  ;   we'll return y-dir
                 0)]    ;   otherwise we'll return 0 for the y direction
    [x-dir' y-dir'])) ; note both are nonzero only when line through segment passes through a corner, i.e. is equal to two boundaries


;; Probably wrong
(defn old-choose-shifts
  "The forward point of seg is assumed to exceed boundaries in both
  dimensions, so x-dir and y-dir should each be either -1 or 1. Returns a
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
  [bound-min bound-max x-dir y-dir seg]
  (let [[pt1 pt2] seg
        [x1 y1] pt1
        [x2 y2] pt2
        x-dir (compare x2 bound-min)  ; -1 means seg goes past bound-min,
        y-dir (compare y2 bound-min)] ; 1 goes past bound-max, 0 means neither
    (if (or (zero? x-dir) (zero? y-dir)) ; if no more than one boundary exceeded
      [(- x-dir) (- y-dir)] ; simple shift or no shift (INCLUDES VERTICAL SLOPE)
      ;; If we're here, forward point of seg exceeds bounds in both dimensions,
      ;; but we can work with the x bounds alone. See doc/exceedingboundaries1.pdf.
      (let [slope (m/slope-from-coords pt1 pt2) ; check whether exceeds beyond other bound
            intercept (m/intercept-from-slope slope [x1 y1])
            compare-fn (if (pos? x-dir) > <)
            x-bound (if (pos? x-dir) bound-max bound-min)
            y-bound (if (pos? y-dir) bound-max bound-min)
            y-at-x-bound (+ (* slope x-bound) intercept)] ; y coord of line at x-bound
        (cond (compare-fn y-at-x-bound x-bound) [0 (- y-dir)] ;; FIXME I DON'T THINK THIS IS RIGHT
              (compare-fn x-bound y-at-x-bound) [(- x-dir) 0]
              (= y-at-x-bound y-bound) [x-dir y-dir]))))) ; rare case
