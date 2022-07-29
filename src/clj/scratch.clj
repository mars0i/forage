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
   [forage.viz.cljplot :as vc]
   [utils.random :as r]))


;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)





(comment

  (def seed (r/make-seed)) 
  (def seed 6310246749308873548)
  (def rng (r/make-well19937 seed))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 300)))
  (def stops (w/walk-stops [0 0] (w/vecs-upto-len 200 step-vector-pool)))
  (count stops)
  (def stops- (conj (vec (drop 20 (take 34 stops))) [35 25]))
  (count stops-)
  (first stops-)
  (last stops-)
  (vc/three-plots 50 10 stops-)


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

  (vc/three-plots 30 4 [[-2 -1] [20 -12]])
  (vc/three-plots 30 3 10 [[-2 -1] [10 -5]])
  (vc/three-plots 30 3 10 [[0 0] [2 1]])

  (vc/plot-result 30 4 [[0 0] [1 -3]])
  (vc/plot-result 30 4 [[-2 6] [0 0]])
  (vc/plot-result 30 4 (t/wrap-path -4 4 [[-2 6] [0 0]]))

  (let [stops [[6.5 0] [5 -3.5] [5 3.5]
               [12.5 2.75] [5.5 1] [8 6]]]
    (vc/plot-result 15 4 stops "unwrapped.jpg")
    (vc/plot-result 15 4 (t/wrap-path -4 4 stops) "wrapped.jpg")
    (vc/plot-result 4 4 (t/wrap-path -4 4 stops) "tight.jpg"))


)
