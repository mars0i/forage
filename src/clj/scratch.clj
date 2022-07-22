;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   ;[aerial.hanami.common :as hc]
   ;[aerial.hanami.templates :as ht]
   [cljplot.build :as cb]
   [cljplot.core :as cc]
   [cljplot.render :as cr]
   ;[clojure2d.extra.utils :exclude [triangle-shape] :as c2u]
   ;[forage.mason.foodspot :as mf]
   [forage.walks :as w]
   [forage.toroidal :as t]
   [utils.random :as r]))


;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)


;; ORIGINAL BY GENERATEME (with minor changes) from
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;; Also at
;; https://github.com/generateme/cljplot/blob/f272932c0228273f293a834e6c19c50d0374d3da/sketches/examples.clj#L572
;; See notes.forage/toroidal/correctpath.clj for a version with comments
;; and a description of the algorithm.
;; SUPERSEDED BY wrap-path and wrap-segment in forage.toroidal.
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


(defn add-cljplot-path-breaks
  [pts]
  (map (fn [pt] (if-not pt [##NaN ##NaN] pt))
       pts))


;; FIXME Temporary: should be revised and moved away from the preceding
;; which is more generic
;; based on https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288501054
;; Notes on usage:
;;    in arg to cb/series :
;;    [:grid] selects a default grid pattern
;;    [:grid nil] seems to be the same; the first nil below seems to be an argument placeholder
;;    [:grid nil {:x nil}] means that there are no vertical grid lines
;;    [:grid nil {:y nil}] means that there are no horizontal grid lines
;; Fourth element in the :color option seems to be transparency or darkness or something
;;    [:grid nil {:position [0 1]}] I don't understand; squashes plot somewhere other than gridlines
(defn plot-result
  ([display-boundary data-boundary data]
   (plot-result display-boundary data-boundary data nil))
  ([display-boundary data-boundary data filename]
   (let [plotfn (fn [chart] (if filename
                              (cc/save chart filename)
                              (cc/show chart)
                              ;(c2u/show-image  chart)
                              ))]
     (-> (cb/series [:grid] [:line (add-cljplot-path-breaks data)
                             {:color [0 0 255 150] ; fourth arg is opacity or brightness or something like that
                              :margins nil}]) 
         (cb/preprocess-series)
         (cb/update-scale :x :domain [(- display-boundary) display-boundary])
         (cb/update-scale :y :domain [(- display-boundary) display-boundary])
         (cb/add-axes :bottom)
         (cb/add-axes :left)
         (cr/render-lattice {:width 800 :height 800 :border 10})
         (plotfn)))))

         ;; trying to add a box where the data boundary is, but it's not working:
         ;(cb/series [:grid] [:line [[(- data-boundary) (- data-boundary)]
         ;                           [(- data-boundary) data-boundary]
         ;                           [data-boundary data-boundary]
         ;                           [data-boundary (- data-boundary)]
         ;                           [(- data-boundary) (- data-boundary)]]
         ;                    {:color [0 255 0 0] :margins nil}])

(comment

  (def seed (r/make-seed))
  (def seed -6115745044562722228)
  (def rng (r/make-well19937 seed))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 3000)))
  (def stops (w/walk-stops [0 0] (w/vecs-upto-len 900 step-vector-pool)))
  (count stops)
  ;(def stops (cons [10 15] (rest stops)))

  (plot-result 400 50 stops "unmod.jpg")
  (plot-result 400 50 (t/wrap-path -50 50 stops) "loose.jpg")
  (plot-result 51 50 (t/wrap-path -50 50 stops) "tight.jpg")


  (def stops [[0 0] [3 2.5]])
  (def stops [[0 0] [7 6.5]])
  (def stops [[0 0] [11 10.5]])
  (def stops [[0 0] [0.5 0.7] [1.2 -0.2] [2.8 -1.5] [4.5 -3.0] [5.0 -3.5]])
  (def stops [[0 0] [0.5 0.7] [1.2 -0.2] [8.3 -17.5] [4.5 -3.0] [5.0 -3.5]])

  (def stops
    [[0 0] [0.8639961884906487 1.0500342594681982] [39.0127813655803 -1.9674464655078325]])

  ;; TODO This seems problematic.  With wrap-path, one of the shifts creates a line
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

  (require '[utils.math :as m])
  (m/slope-from-coords [-2 -1] [5 -9/2]) ; => -1/2
  (m/slope-from-coords [-2 -1] [10 -7])  ; => -1/2
  (m/slope-from-coords [-2 -1] [20 -12])  ; => -1/2
  (def stops [[-2 -1] [5 -9/2]])
  (def stops [[-2 -1] [10 -7]])
  (def stops [[-2 -1] [20 -12]])

  (plot-result 30 4 stops "unmod.jpg")
  (plot-result 4 4  (t/wrap-path -4 4 stops) "tight.jpg")
  (plot-result 30 4 (t/wrap-path -4 4 stops) "loose.jpg")


  (def square [[-4 -4] [-4 4] [4 4] [4 -4] [-4 -4]])
  (plot-result 5 4 square "square.jpg")



)
