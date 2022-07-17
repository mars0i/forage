;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   ;[aerial.hanami.common :as hc]
   ;[aerial.hanami.templates :as ht]
   [cljplot.build :as cb]
   [cljplot.core :as cc]
   [cljplot.render :as cr]
   ;[forage.mason.foodspot :as mf]
   [forage.walks :as w]
   [forage.toroidal :as t]
   [utils.random :as r]))


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

;; loop/recur version.  nothing fancy yet: just dupes what correct-segs-reduce did
;; (well sortof) and what the original correct-path should have done.  Specifically,
;; generateme's correct-path was missing the last line segment (as their comment
;; highlighted, implicitly); this includes it.
(defn correct-segs
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
          (recur (if (and (== new-shift-x shift-x)
                          (== new-shift-y shift-y))
                   (conj new-segs new-seg)
                   (conj new-segs
                         new-seg
                         nil
                         (shift-seg new-shift-x new-shift-y seg)))
                 new-shift-x new-shift-y
                 (next segs)))))))

(defn correct-path
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with a new segment that is
  the previous version shifted so that, if it is short enough, it will
  end within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-left boundary-right points]
  (segs-to-points
    (correct-segs boundary-left boundary-right
                  (points-to-segs points))))

;; PROPOSED STRATEGY FOR HANDLING LONG SEGMENTS:
;; In the second branch of the if, remove the last line (so nil is the
;; last output), and remove "next" before segs, so that the same
;; segment is processed again--but now with new shifts.  So to do
;; that, the recur must be pushed into the if branches; there have to
;; be two recurs.
(defn correct-segs+
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
  ;; FIXME
  points)

;; TODO test equivalence with the old version
(defn correct-path+
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-left boundary-right points]
  (->> (points-to-segs points)
       ;(fix-first-seg boundary-left boundary-right)
       (correct-segs+ boundary-left boundary-right)
       (segs-to-points)))

(defn correct-path+old
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with new segments shifted
  so that all parts of the original segment would get displayed
  within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-left boundary-right points]
  (segs-to-points
    (correct-segs+ boundary-left boundary-right
                  (points-to-segs points))))

;; ORIGINAL BY GENERATEME (with minor changes) from
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;; Also at
;; https://github.com/generateme/cljplot/blob/f272932c0228273f293a834e6c19c50d0374d3da/sketches/examples.clj#L572
;; See notes.forage/toroidal/correctpath.clj for a version with comments
;; and a description of the algorithm.
;; Note this is missing the last line segment (as one of generateme's original 
;; comments highlighted.
(defn original-correct-path
  "Given a sequence of points representing a path connected by line
  segments, returns a transformed path in which segments that would go
  beyond the boundaries are \"duplicated\" with a new segment that is
  the previous version shifted so that, if it is short enough, it will
  end within the boundaries.  A sequence of points from such segments
  are returned, where \"duplicates\" are by nils."
  [boundary-left boundary-right path]
  (let [width (- boundary-right boundary-left)]
    (first (reduce (fn [[new-path shift-x shift-y] [[curr-x curr-y] [next-x next-y]]]
                     (let [s-curr-x (+ shift-x curr-x)
                           s-curr-y (+ shift-y curr-y)
                           s-next-x (+ shift-x next-x)
                           s-next-y (+ shift-y next-y)
                           new-shift-x (cond
                                         (< s-next-x boundary-left)  (+ shift-x width)
                                         (> s-next-x boundary-right) (- shift-x width)
                                         :else shift-x)
                           new-shift-y (cond
                                         (< s-next-y boundary-left)  (+ shift-y width)
                                         (> s-next-y boundary-right) (- shift-y width)
                                         :else shift-y)]
                       [(if (and (== new-shift-x shift-x)
                                 (== new-shift-y shift-y))
                          (conj new-path [s-curr-x s-curr-y])
                          (conj new-path
                                [s-curr-x s-curr-y] [s-next-x s-next-y]
                                nil
                                [(+ curr-x new-shift-x) (+ curr-y new-shift-y)]))
                        new-shift-x
                        new-shift-y]))
                   [[] 0.0 0.0]
                   (partition 2 1 path)))))


;; deprecated. for reference.
;; supposed to do roughly same thing as generateme's correct-path
(defn correct-segs-reduce
  [boundary-left boundary-right segs]
  (let [width (- boundary-right boundary-left)]
    (first (reduce (fn [[new-segs shift-x shift-y] seg]
                     (let [new-seg (shift-seg shift-x shift-y seg)
                           [[new-x1 new-y1] [new-x2 new-y2]] new-seg
                           new-shift-x (cond
                                         (< new-x2 boundary-left)  (+ shift-x width)
                                         (> new-x2 boundary-right) (- shift-x width)
                                         :else shift-x)
                           new-shift-y (cond
                                         (< new-y2 boundary-left)  (+ shift-y width)
                                         (> new-y2 boundary-right) (- shift-y width)
                                         :else shift-y)]
                       [(if (and (== new-shift-x shift-x)
                                 (== new-shift-y shift-y))
                          (conj new-segs new-seg)
                          (conj new-segs
                                new-seg
                                nil
                                (shift-seg new-shift-x new-shift-y seg)))
                        new-shift-x
                        new-shift-y]))
                   [[] 0.0 0.0]
                   segs))))


(defn add-cljplot-path-breaks
  [pts]
  (map (fn [pt] (if-not pt [##NaN ##NaN] pt))
       pts))


;; FIXME Temporary: should be revised and moved away from the preceding
;; which is more generic
(defn plot-result
  ([display-boundary data-boundary data]
   (plot-result display-boundary data-boundary data nil))
  ([display-boundary data-boundary data filename]
   (let [plotfn (fn [chart] (if filename
                              (cc/save chart filename)
                              (cc/show chart )))]
     (-> (cb/series [:grid] [:line (add-cljplot-path-breaks data)
                             {:color [0 0 255 150] :margins nil}])
         ;; trying to add a box where the data boundary is, but it's not working:
         ;(cb/series [:grid] [:line [[(- data-boundary) (- data-boundary)]
         ;                           [(- data-boundary) data-boundary]
         ;                           [data-boundary data-boundary]
         ;                           [data-boundary (- data-boundary)]
         ;                           [(- data-boundary) (- data-boundary)]]
         ;                    {:color [0 255 0 0] :margins nil}])
         (cb/preprocess-series)
         (cb/update-scale :x :domain [(- display-boundary) display-boundary])
         (cb/update-scale :y :domain [(- display-boundary) display-boundary])
         (cb/add-axes :bottom)
         (cb/add-axes :left)
         (cr/render-lattice {:width 800 :height 800 :border 10})
         (plotfn)))))

(comment

  (def rng (r/make-well19937))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 500)))
  (def stops (w/walk-stops [0 0] 
                           (w/vecs-upto-len 50 step-vector-pool)))
  (count stops)

  (def stops [[0 0] [3 2.5]])
  (def stops [[0 0] [7 6.5]])
  (def stops [[0 0] [11 10.5]])
  (def stops [[0 0] [0.5 0.7] [1.2 -0.2] [2.8 -1.5] [4.5 -3.0] [5.0 -3.5]])
  (def stops [[0 0] [0.5 0.7] [1.2 -0.2] [8.3 -17.5] [4.5 -3.0] [5.0 -3.5]])

  (def stops
    [[0 0] [0.8639961884906487 1.0500342594681982] [39.0127813655803 -1.9674464655078325]])

  ;; TODO This seems problematic.  With correct-path+, one of the shifts creates a line
  ;; that's wholly outside the boundaries -4,4.  (Does this have to do with the fact that
  ;; the first segment is long?)
  (def stops
    [[0 0] [39.0127813655803 -1.9674464655078325] [10.8639961884906487 11.0500342594681982]])

  ;; Here, too, the shift on the second segment doesn't look right.  Shouldn't it
  ;; to the left, so it emerges from the bottom boundary under where the previous
  ;; one goes out of the top?  But it is following the intended algorithm.  Is that wrong?
  (def stops
    [[0 0] [19.0127813655803 -1.9674464655078325] [10.8639961884906487 11.0500342594681982]])

  ;; Here, too, the shift on the second segment doesn't look right.  Shouldn't it
  ;; to the left, so it emerges from the bottom boundary under where the previous
  ;; one goes out of the top?  But it is following the intended algorithm.  Is that wrong?
  (def stops [[0 0] [19 -2] [10 11]])
  (def stops [[0 0] [19 -2] [10 31]])

  (def p4 (correct-path+ -4 4 stops))
  (def p4+ (segs-to-points (correct-segs+ -4 4 (points-to-segs stops))))
  (add-cljplot-path-breaks (correct-path+ -4 4 stops))
  (= p1 (butlast p3))

  (plot-result 4 4  (correct-path+ -4 4 stops) "tight.jpg")
  (plot-result 20 4 (correct-path+ -4 4 stops) "loose.jpg")
  (plot-result 20 4 stops "raw.jpg")
  ;; based on https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288501054


  (defn foo [x & ys]
    (->> x
         (println)
         (if ys
           (println "But wait! There's more:" ys "and")
           (println "Just"))))

  (defn bar [x :truth]
    (println x truth))

)
