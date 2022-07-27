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



(defn add-cljplot-path-breaks
  [pts]
  (replace {nil [##NaN ##NaN]} pts))


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
  ([display-bound data-bound data]
   (plot-result display-bound data-bound data nil))
  ([display-bound data-bound data filename]
   (plot-result (- display-bound) display-bound
                (- data-bound) data-bound data filename))
  ([display-bound-min display-bound-max
    data-bound-min data-bound-max ; only used to make box
    data filename]
   (let [box-segs [[data-bound-min data-bound-min]
                   [data-bound-min data-bound-max]
                   [data-bound-max data-bound-max]
                   [data-bound-max data-bound-min]
                   [data-bound-min data-bound-min]
                   [##NaN ##NaN]]
         plotfn (fn [chart] (if filename
                              (cc/save chart filename)
                              (cc/show chart)
                              ;(c2u/show-image  chart)
                              ))]
     (-> (cb/series [:grid] [:line (concat box-segs (add-cljplot-path-breaks data))
                             {:color [0 0 255 150] ; fourth arg is opacity or brightness or something like that
                              :margins nil}]) 
         (cb/preprocess-series)
         (cb/update-scale :x :domain [display-bound-min display-bound-max])
         (cb/update-scale :y :domain [display-bound-min display-bound-max])
         (cb/add-axes :bottom)
         (cb/add-axes :left)
         (cr/render-lattice {:width 800 :height 800 :border 10})
         (plotfn)))))

         ;; trying to add a box where the data bound is, but it's not working:
         ;(cb/series [:grid] [:line [[(- data-bound) (- data-bound)]
         ;                           [(- data-bound) data-bound]
         ;                           [data-bound data-bound]
         ;                           [data-bound (- data-bound)]
         ;                           [(- data-bound) (- data-bound)]]
         ;                    {:color [0 255 0 0] :margins nil}])

(comment

  (defn three-plots
    ([outer inner data] (three-plots outer (- inner) inner data))
    ([outer inner- inner+ data]
     (plot-result (- outer) outer inner- inner+ data "unmod.jpg")
     (plot-result (- outer) outer inner- inner+ (t/wrap-path inner- inner+ data) "loose.jpg")
     (plot-result (dec inner-) (inc inner+)
                  inner- inner+
                  (t/wrap-path inner- inner+ data) "tight.jpg")))

  (def seed (r/make-seed))
  (def seed -6115745044562722228)
  (def rng (r/make-well19937 seed))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 500)))
  (def stops (w/walk-stops [0 0] (w/vecs-upto-len 450 step-vector-pool)))
  (count stops)

  (three-plots 250 50 stops)


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

  (three-plots 30 4 [[-2 -1] [20 -12]])

  (three-plots 30 3 10 [[-2 -1] [10 -5]])
  (three-plots 30 3 10 [[0 0] [2 1]])


  (def square [[-4 -4] [-4 4] [4 4] [4 -4] [-4 -4]])
  (plot-result 5 4 square "square.jpg")



)
