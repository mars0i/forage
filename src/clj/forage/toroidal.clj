;; Functions to manipulate search paths as if in a toroidal environment,
;; i.e. one with periodic boundary conditions.
(ns forage.toroidal
  (:require [utils.math as m]))


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

;; FIXME Bug: If a segment exceeds a boundary *only* beyond a boundary in
;; the other direction, it's duplicated and shifted in the first direction,
;; even though it shouldn't be.
(defn wrap-segments
  "Given a sequence of line segments--pairs of pairs of numbers--representing
  a path connected by line segments, returns a transformed sequence in which
  segments whose second point that would go beyond the boundaries are
  \"duplicated\" with a new segment that is the previous version shifted so
  that, if it's short enough, the duplicate ends within boundaries.  If it
  doesn't, then another \"duplicate\" will be created that's further shifted,
  and so on, until there is a duplicate that ends within the boundaries.
  \"Duplicate\" segments are separated by nils."
  [boundary-left boundary-right segments]
  (let [width (- boundary-right boundary-left)]
    (loop [new-segs [], shift-x 0.0, shift-y 0.0, segs segments]
      (if-not segs
        new-segs
        (let [seg (first segs)
              new-seg (shift-seg shift-x shift-y seg)
              [[new-x1 new-y1] [new-x2 new-y2]] new-seg
              new-shift-x (cond (< new-x2 boundary-left)  (+ shift-x width)
                                (> new-x2 boundary-right) (- shift-x width)
                                :else shift-x)
              new-shift-y (cond (< new-y2 boundary-left)  (+ shift-y width)
                                (> new-y2 boundary-right) (- shift-y width)
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
  [boundary-left boundary-right points]
  ;; TODO
  points)

(defn wrap-path
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-left boundary-right points]
  (->> (points-to-segs points)
       ;(fix-first-seg boundary-left boundary-right)
       (wrap-segs boundary-left boundary-right)
       (segs-to-points)))



(comment

  (def rng (r/make-well19937))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 500)))
  (def stops (w/walk-stops [0 0] 
                           (w/vecs-upto-len 50 step-vector-pool)))
  (count stops)

  ;; Illustrations of bug in wrap-segs described above.
  (def stops [[0 0] [19 -2] [10 11]])
  (def stops [[0 0] [19 -2] [10 31]])

  ;; Should be identical
  (def p1 (wrap-path -4 4 stops))
  (def p2 (segs-to-points (wrap-segs -4 4 (points-to-segs stops))))

)
