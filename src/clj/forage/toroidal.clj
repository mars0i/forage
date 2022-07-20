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
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)


(defn shift-seg
  "Given a line segment seg (represented by its 2D endpoints as a pair
  of pairs of numbers), returns a version of the line segment in which
  the x-coordinates have been shifted by the addition of sh-x, and
  the y-coordinates have been shifted by the addition of sh-y."
  [sh-x sh-y seg]
  (let [[[x1 y1] [x2 y2]] seg]
    [[(+ x1 sh-x) (+ y1 sh-y)]
     [(+ x2 sh-x) (+ y2 sh-y)]]))

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


(defn choose-shifts
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
        x-dir (cond (< x2 bound-min) -1 ; exceeds bound-min
                    (> x2 bound-max) 1  ; exceeds bound-max
                    :else 0)            ; exceeds neither
        y-dir (cond (< y2 bound-min) -1 ; see above
                    (> y2 bound-max) 1
                    :else 0)]
    (if (or (zero? x-dir) (zero? y-dir)) ; if no more than one boundary exceeded
      [(- x-dir) (- y-dir)] ; simple shift or no shift (INCLUDES VERTICAL SLOPE)
      (let [slope (m/slope-from-coords pt1 pt2) ; check whether exceeds beyond other bound
            intercept (m/intercept-from-slope slope [x1 y1])
            x-bound (if (pos? x-dir) bound-min bound-max) ; x-dir = dir of needed shift: pos if seg crossed left bound
            y-bound (if (pos? y-dir) bound-min bound-max) ; similar for y-dir
            y-at-x-bound (+ (* slope x-bound) intercept)]
        ;; THIS PROBABLY ISN'T RIGHT.  DIRECTION OF INEQUALITY SHOULD DEPEND ON x-dir.
        (cond (< y-at-x-bound x-bound) [x-dir 0]
              (> y-at-x-bound x-bound) [0 y-dir]
              (= y-at-x-bound y-bound) [x-dir y-dir]))))) ; rare case


;; FIXME Bug: If a segment exceeds a boundary *only* beyond a boundary in
;; the other direction, it's duplicated and shifted in the first direction,
;; even though it shouldn't be.
;; FIXME: This prelim version supposed to be equivalent to wrap-segs-old, but it's not.
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
  (let [width (- bound-max bound-min)]
    (if-not segs
      new-segs
      (let [seg (first segs)
              new-seg (shift-seg sh-x sh-y seg)
              [x-sh-dir y-sh-dir] (choose-shifts bound-min bound-max seg)
              [new-sh-x new-sh-y] [(+ sh-x (* x-sh-dir width)) (+ sh-y (* y-sh-dir width))]]
          (if (and (== new-sh-x sh-x)
                   (== new-sh-y sh-y))
            (recur (conj new-segs new-seg)
                   new-sh-x new-sh-y
                   (next segs))
            (recur (conj new-segs new-seg nil)
                   new-sh-x new-sh-y
                   segs))))))) ; Add same seg after nil, but with new shifts;
                               ; and keep doing that until the forward end
                               ; (new-x2, new-y2) no longer goes beyond boundary.

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
