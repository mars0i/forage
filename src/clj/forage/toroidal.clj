;; Functions to manipulate search paths as if in a toroidal environment,
;; i.e. one with periodic boundary conditions.
(ns forage.toroidal
  (:require [utils.math :as m]))


;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)


(defn shift-seg
  "Given a line segment seg (represented by its 2D endpoints as a pair
  of pairs of numbers), returns a version of the line segment in which
  the x-coordinates have been shifted by the addition of shift-x, and
  the y-coordinates have been shifted by the addition of shift-y."
  [shift-x shift-y seg]
  (let [[[x1 y1] [x2 y2]] seg]
    [[(+ x1 shift-x) (+ y1 shift-y)]
     [(+ x2 shift-x) (+ y2 shift-y)]]))

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

(defn new-shift-dirs
  "Returns a pair in which each element is -1, 0, or 1.  The first element
  indicates the direction in which the x coordinates of seg must be shifted
  in order for its forward point to be closer to being within
  [boundary-min boundary-max].  The second element does the same for seg's
  y coordinates.  (Note that the value returned is not the direction in which
  a boundary is exceeded; it's the direction in which seg must be
  \"corrected\" by shifting it back toward the standard region.)"
  [boundary-min boundary-max seg]
  (let [[_ [x2 y2]] seg]
    [(cond (< x2 boundary-min) 1
           (> x2 boundary-max) -1
           :else 0)
     (cond (< y2 boundary-min) 1
           (> y2 boundary-max) -1
           :else 0)]))

(defn which-shifts
  "The forward point of seg is assumed to exceed boundaries in both 
  dimensions. Returns a pair of shift values, for x and y, after
  determining whether one of the forward coordinates exceeds a boundary
  in dimension D at a location that is outside of a boundary in the other
  dimension.  That would mean that seg should not be shifted in dimension
  D, but only in the other dimension.  Shifts in both directions are
  appropriate only when a line segment goes through a corner of the
  standard region."
  [boundary-min boundary-max x-dir y-dir seg]
  seg) ; temporary FIXME


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
  [boundary-min boundary-max segments]
  (loop [new-segs [], shift-x 0.0, shift-y 0.0, segs segments]
  (let [width (- boundary-max boundary-min)]
    (if-not segs
      new-segs
      (let [seg (first segs)
              new-seg (shift-seg shift-x shift-y seg)
              [x-dir y-dir] (new-shift-dirs boundary-min boundary-max new-seg) ; directions in which new shifts needed
              [new-shift-x new-shift-y] (if (or (zero? x-dir) (zero? y-dir)) ; if no more than one boundary exceeded
                                          [(+ shift-x (* x-dir width)) (+ shift-y (* y-dir width))]  ; simple method to shift
                                          (which-shifts boundary-min boundary-max x-dir y-dir seg))] ; else it's more complicated
          (if (and (== new-shift-x shift-x)
                   (== new-shift-y shift-y))
            (recur (conj new-segs new-seg)
                   new-shift-x new-shift-y
                   (next segs))
            (recur (conj new-segs new-seg nil)
                   new-shift-x new-shift-y
                   segs))))))) ; Add same seg after nil, but with new shifts;
; and keep doing that until the forward end
                            ; (new-x/y2) no longer goes beyond boundary.

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
  [boundary-min boundary-max segments]
  (let [width (- boundary-max boundary-min)]
    (loop [new-segs [], shift-x 0.0, shift-y 0.0, segs segments]
      (if-not segs
        new-segs
        (let [seg (first segs)
              new-seg (shift-seg shift-x shift-y seg)
              [[new-x1 new-y1] [new-x2 new-y2]] new-seg
              new-shift-x (cond (< new-x2 boundary-min)  (+ shift-x width)
                                (> new-x2 boundary-max) (- shift-x width)
                                :else shift-x)
              new-shift-y (cond (< new-y2 boundary-min)  (+ shift-y width)
                                (> new-y2 boundary-max) (- shift-y width)
                                :else shift-y)]
          (if (and (== new-shift-x shift-x)
                   (== new-shift-y shift-y))
            (recur (conj new-segs new-seg)
                   new-shift-x new-shift-y
                   (next segs))
            (recur (conj new-segs new-seg nil)
                   new-shift-x new-shift-y
                   segs))))))) ; Add same seg after nil, but with new shifts;
                               ; and keep doing that until the forward end
                               ; (new-x/y2) no longer goes beyond boundary.

(defn fix-first-seg
  "If first segment begins outside the boundaries, shifts it in."
  [boundary-min boundary-max points]
  ;; TODO
  points)

(defn wrap-path
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-min boundary-max points]
  (->> (points-to-segs points)
       ;(fix-first-seg boundary-min boundary-max)
       (wrap-segs boundary-min boundary-max)
       (segs-to-points)))

;; DEPRECATED
(defn wrap-path-old
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-min boundary-max points]
  (->> (points-to-segs points)
       ;(fix-first-seg boundary-min boundary-max)
       (wrap-segs-old boundary-min boundary-max)
       (segs-to-points)))

