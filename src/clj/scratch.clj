;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]
   [cljplot.build :as cb]
   [cljplot.core :as cc]
   [cljplot.render :as cr]
   [forage.mason.foodspot :as mf]
   [forage.walks :as w]
   [utils.random :as r]))


;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)


(defn shift-seg
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
  [points]
  (partition 2 1 points))

;; a new strategy (needs loop/recur):
;; add seg to output
;; if not in bounds, add nil to output, then push seg back on stack

; FIXME Differs from correct-segs because generates one extra point
;; (Maybe this is the correct behavior?)
;; 
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

;; ORIGINAL BY GENERATEME (more or less) from
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;; See notes.forage/toroidal/correctpath.clj for a version with comments
;; and a description of the algorithm.
;; Note this is missing the last line segment (as one of generateme's original 
;; comments highlighted.
(defn correct-path
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


(comment

  (def rng (r/make-well19937))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 20)))
  (def stops (w/walk-stops [0 0] 
                           (w/vecs-upto-len 100 step-vector-pool)))
  (count stops)

  (def stops
    [[0 0]
     [0.9700914276659796 1.6365790878224906]
     [-0.824501634774673 2.1803587635156556]
     [1.261448866116044 2.834273995635776]
     [0.21316777054277192 1.0885351519809954]])

  (def stops [[0 0] [0.5 0.7] [1.2 -0.2] [2.8 -1.5] [4.5 -3.0] [5.0 -3.5]])
  (partition 2 1 stops)
  (partition 2 1 (range 4))

  (def p1 (correct-path -2 2 stops))
  (def p3 (segs-to-points (correct-segs -2 2 (points-to-segs stops))))
  (def p2 (segs-to-points (correct-segs-reduce -2 2 (points-to-segs stops))))
  (= p1 (butlast p3))
  (let [n 10] (= (take n p1) (take n p3)))



  (def corrected-stops
    [[0.0 0.0]
     [0.9700914276659796 1.6365790878224906]
     [-0.824501634774673 2.1803587635156556]
     nil
     [0.9700914276659796 -2.363420912177509]
     [-0.824501634774673 -1.8196412364843444]
     [1.261448866116044 -1.165726004364224]
     [0.21316777054277192 -2.911464848019005]
     nil
     [1.261448866116044 2.834273995635776]])

  (def corrected2-stops (correct-path2 -2 2 stops))

  (= corrected-stops corrected2-stops)


  (def short-stops 
    [[0 0]
     [1.261448866116044 2.834273995635776]
     [0.21316777054277192 1.0885351519809954]])

  (def corrected-short-stops (correct-path -2 2 short-stops))

  ;; based on https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288501054
  (defn plot-result
    [boundary]
    (let [data (take 5000 stops)] ; stops may have many fewer points
      (-> (cb/series [:grid] [:line (add-cljplot-path-breaks (correct-path -2 2 data))
                              {:color [0 0 255 150] :margins nil}])
          (cb/preprocess-series)
          (cb/update-scale :x :domain [(- boundary) boundary])
          (cb/update-scale :y :domain [(- boundary) boundary])
          (cb/add-axes :bottom)
          (cb/add-axes :left)
          (cr/render-lattice {:width 750 :height 750 :border 10})
          ;(cc/save "yo.jpg")
          (cc/show))))
  (plot-result 2)

  ;; old version based on https://github.com/generateme/cljplot/blob/master/sketches/vega.clj#L570
  (def plot-result
    (let [data (take 1000 stops)]
      (-> 
        ;(cb/series [:grid] [:line data {:stroke {:size 2} :point {:type \O}}])
        (cb/series [:grid] [:line (correct-path -200 200 data) {:stroke {:size 1}}])
        (cb/preprocess-series)
        ;(cb/update-scale :x :ticks 4)
        (cb/update-scale :x :domain [-200 200])
        (cb/update-scale :y :domain [-200 200])
        (cb/add-axes :bottom)
        (cb/add-axes :left)
        (cb/add-label :bottom "x")
        (cb/add-label :left "y")
        (cr/render-lattice {:width 400 :height 400 :border 20})
        (cc/save "yo.jpg")
        (cc/show)
        )))


  (def env (mf/make-env 10 1000))
  (def look-fn (partial mf/perc-foodspots-exactly-toroidal env 1))
  (def fw (w/levy-foodwalk look-fn 0.1 10000 false 10000 rng 1 2 [0 0]))

  (map count fw)
  (= (nth fw 1) (nth fw 2))



  (defn ignore-food [x y] nil)

  (require '[oz.core :as oz])
  (oz/start-server!)

  (def data [{"x" 100, "y" 200, "ord" 1, "label" "yow"}
             {"x" 500, "y" 300, "ord" 2, "label" "yow"}
             {"x" 100, "y" 500, "ord" 3, "label" "yeah"}
             {"x" 800, "y" -300, "ord" 4, "label" "yeah"}
             {"x" 250, "y" -200, "ord" 5, "label" "yeah"}
             ])

  (keep identity [1 2 3 nil 4 false 5])
  (filter identity [1 2 3 nil 4 false 5])

  (defn select-by-label
    [data label]
    (filter #(= (get % "label") label) 
            data))

  (select-by-label data "yow")
  (select-by-label data "yeah")

  (defn add-ord
    [chart]
    (assoc-in chart [:encoding :order :field] "ord"))

  (def plot
    (->
      (hc/xform
        ht/vconcat-chart
        :TITLE "Yow, yeah!"
        :TOFFSET "10" ; space between meta-title and plots
        :VCONCAT [(hc/xform
                    ht/line-chart
                    :TITLE "Yow plot"
                    :DATA (select-by-label data "yow")
                    :MSIZE 2)
                  (hc/xform
                    ht/line-chart
                    :TITLE "Yeah plot"
                    :DATA (select-by-label data "yeah")
                    :MSIZE 2)])
      (update :vconcat #(map add-ord %))))

  plot

  (oz/view! plot)

)
