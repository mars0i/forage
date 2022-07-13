;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require [forage.viz.hanami :as h]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [forage.walks :as w]
            [forage.food :as f]
            [forage.run :as fr]
            [forage.mason.foodspot :as mf] 
            [cljplot.build :as cb]
            [cljplot.render :as cr]
            [cljplot.core :as cc]
            [utils.math :as m]
            [utils.random :as r]))


;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)


;; Based on code by generateme at
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;; [most comments added by Marshall]
(defn correct-path
  [boundary-left boundary-right path]
  (let [width (- boundary-right boundary-left)]
    (first  ; <- we no longer need shift-x and shift-y (they had to be passed along during)
           (reduce (fn [[new-path shift-x shift-y]
                        [[curr-x curr-y] [next-x next-y]]] ; endpoints of a line segment
                     (let [s-curr-x (+ shift-x curr-x) ; "s-" = "shifted"
                           s-curr-y (+ shift-y curr-y) ; note first point assumed w/in bounds
                           s-next-x (+ shift-x next-x) ; Once image is shifted, it
                           s-next-y (+ shift-y next-y) ; remains so (unless back-shifted).
                           ;; If needed, we pass a new shift to the next iteration
                           ;; because we'll duplicate the current segment, shifted, and
                           ;; then all subsequent points will be additionally shifted 
                           ;; that much:
                           new-shift-x (cond
                                         (< s-next-x boundary-left) (+ shift-x width)
                                         (> s-next-x boundary-right) (- shift-x width)
                                         :else shift-x)
                           new-shift-y (cond
                                         (< s-next-y boundary-left) (+ shift-y width)
                                         (> s-next-y boundary-right) (- shift-y width)
                                         :else shift-y)]
                       [(if (and (== new-shift-x shift-x)  ; not duplicating with shift
                                 (== new-shift-y shift-y)) ; just using old shift
                          (conj new-path [s-curr-x s-curr-y]) ; so just add new point
                          (conj new-path
                                [s-curr-x s-curr-y] [s-next-x s-next-y] ; segment that runs past at least one boundary
                                nil ; break between continuous lines
                                [(+ curr-x new-shift-x) (+ curr-y new-shift-y)])) ; shifted outside boundaries--duplicate of the point that was *within* boundaries on other side
                        ; next point will be shifted as much, but inside
                        new-shift-x
                        new-shift-y]))
                   [[] 0.0 0.0]
                   (partition 2 1 path))))) ; points -> segments (with shared endpoints)

(defn shift-seg
  [shift-x shift-y seg]
  (let [[[x1 y1] [x2 y2]] seg]
    [[(+ x1 shift-x) (+ y1 shift-y)]
     [(+ x2 shift-x) (+ y2 shift-y)]]))

;; FIXME (?): should not ignore second point before a nil
(defn segs-to-points-bad
  "Convert a sequence of line segments (pairs of pairs, where the
  second pair of each segment is the first pair of the next one),
  into a sequence of points, where the overlapping points are
  deduplicated.  nils may appear as delimiters, and will be
  preserved."
  [segs]
  (conj (vec (map first segs)) ; relies on the fact that (first nil) => nil
        (last (last segs))))

;; TODO probably not right
(defn segs-to-points
  [segs]
  (if (< (count segs) 2)
    segs
    (loop [points [],
           curr-seg (first segs),
           next-seg (second segs)
           more-segs (next segs)] ; yes, it contains second
      (if-not more-segs
        points
        (if (= (count more-segs) 1)
          (conj points (last (first more-segs)))
          (recur (if next-seg ; if not a marker nil
                   (conj points (first curr-seg)) ; alaways get the first point
                   (conj points (first curr-seg) (second curr-seg))) ; if nil is next, get second point, too
                 (first more-segs)
                 (second more-segs)
                 (next more-segs)))))))

(defn points-to-segs
  [points]
  (partition 2 1 points))

;; a new strategy (needs loop/recur):
;; add seg to output
;; if not in bounds, add nil to output, then push seg back on stack

(defn correct-segs
  [boundary-left boundary-right path]
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
                                (shift-seg new-shift-x new-shift-y new-seg)))
                        new-shift-x
                        new-shift-y]))
                   [[] 0.0 0.0]
                   segs))))


(defn correct-path-loop
  [boundary-left boundary-right path]
  (let [width (- boundary-right boundary-left)]
    (loop [new-path [], shift-x 0.0, shift-y 0.0,
           segments (partition 2 1 path)]
      (if-not segments
        new-path
        (let [[[curr-x curr-y] [next-x next-y]] (first segments)
              shifted-curr-x (+ shift-x curr-x)
              shifted-curr-y (+ shift-y curr-y)
              shifted-next-x (+ shift-x next-x)
              shifted-next-y (+ shift-y next-y)
              new-shift-x (cond
                            (< shifted-next-x boundary-left) (+ shift-x width)
                            (> shifted-next-x boundary-right) (- shift-x width)
                            :else shift-x)
              new-shift-y (cond
                            (< shifted-next-y boundary-left) (+ shift-y width)
                            (> shifted-next-y boundary-right) (- shift-y width)
                            :else shift-y)]
          (recur (if (and (== new-shift-x shift-x)
                          (== new-shift-y shift-y))
                   (conj new-path [shifted-curr-x shifted-curr-y])
                   (conj new-path
                         [shifted-curr-x shifted-curr-y]
                         [shifted-next-x shifted-next-y]
                         nil  ; replace as needed for plotting system
                         [(+ curr-x new-shift-x) (+ curr-y new-shift-y)]))
                 new-shift-x new-shift-y
                 (next segments)))))))


;; new attempt at revision
;; NOT RIGHT (seems to loop infinitely)
(defn correct-path-loop*
  [boundary-left boundary-right path]
  (let [width (- boundary-right boundary-left)]
    (loop [new-path [], shift-x 0.0, shift-y 0.0,
           segments (partition 2 1 path)]
      (if-not segments
        new-path
        (let [[[curr-x curr-y] [next-x next-y]] (first segments)
              shifted-curr-x (+ shift-x curr-x)
              shifted-curr-y (+ shift-y curr-y)
              shifted-next-x (+ shift-x next-x)
              shifted-next-y (+ shift-y next-y)
              new-point [shifted-curr-x shifted-curr-y]
              [new-points new-shift-x new-shift-y]
              (loop [new-pts [new-point]
                     shift-x shift-x
                     shift-y shift-y]
                (let [new-shift-x (cond
                                    (< shifted-next-x boundary-left) (+ shift-x width)
                                    (> shifted-next-x boundary-right) (- shift-x width)
                                    :else shift-x)
                      new-shift-y (cond
                                    (< shifted-next-y boundary-left) (+ shift-y width)
                                    (> shifted-next-y boundary-right) (- shift-y width)
                                    :else shift-y)]
                  (if (and (== new-shift-x shift-x)
                           (== new-shift-y shift-y))
                    [new-pts new-shift-x new-shift-y]
                    (recur (conj new-pts
                                 [shifted-next-x shifted-next-y]
                                 nil
                                 [(+ curr-x new-shift-x) (+ curr-y new-shift-y)])
                           new-shift-x new-shift-y))))]
          (recur (conj new-path new-points)
                 new-shift-x new-shift-y
                 (next segments)))))))



;; Behavior identical to correct-path, but does the "partitioning" internally
;; rather than in the sequence passed to reduce.
(defn correct-path-loop-internal
  [boundary-left boundary-right path]
  (let [width (- boundary-right boundary-left)]
    (loop [new-path [], shift-x 0.0, shift-y 0.0, points path]
      (if-not (next points)
        new-path
        (let [[curr-x curr-y] (first points) 
              [next-x next-y] (second points)
              shifted-curr-x (+ shift-x curr-x)
              shifted-curr-y (+ shift-y curr-y)
              shifted-next-x (+ shift-x next-x)
              shifted-next-y (+ shift-y next-y)
              new-shift-x (cond
                            (< shifted-next-x boundary-left) (+ shift-x width)
                            (> shifted-next-x boundary-right) (- shift-x width)
                            :else shift-x)
              new-shift-y (cond
                            (< shifted-next-y boundary-left) (+ shift-y width)
                            (> shifted-next-y boundary-right) (- shift-y width)
                            :else shift-y)]
          (if (and (== new-shift-x shift-x)
                   (== new-shift-y shift-y))
            (recur (conj new-path [shifted-curr-x shifted-curr-y])
                   new-shift-x new-shift-y (next points))
            (recur (conj new-path
                         [shifted-curr-x shifted-curr-y]
                         [shifted-next-x shifted-next-y]
                         nil
                         [(+ curr-x new-shift-x) (+ curr-y new-shift-y)])
                   new-shift-x new-shift-y 
                   (rest points))))))))


;; Attempt at revised behavior:
(defn correct-path5a
  [boundary-left boundary-right path]
  (let [width (- boundary-right boundary-left)]
    (loop [new-path [], shift-x 0.0, shift-y 0.0, points path]
      (if-not (next points)
        new-path
        (let [[curr-x curr-y] (first points) 
              [next-x next-y] (second points)
              shifted-curr-x (+ shift-x curr-x)
              shifted-curr-y (+ shift-y curr-y)
              shifted-next-x (+ shift-x next-x)
              shifted-next-y (+ shift-y next-y)
              new-shift-x (cond
                            (< shifted-next-x boundary-left) (+ shift-x width)
                            (> shifted-next-x boundary-right) (- shift-x width)
                            :else shift-x)
              new-shift-y (cond
                            (< shifted-next-y boundary-left) (+ shift-y width)
                            (> shifted-next-y boundary-right) (- shift-y width)
                            :else shift-y)]
          (if (and (== new-shift-x shift-x)
                   (== new-shift-y shift-y))
            (recur (conj new-path [shifted-curr-x shifted-curr-y])
                   new-shift-x new-shift-y (next points))
            (let [newly-shifted-curr [(+ curr-x new-shift-x) (+ curr-y new-shift-y)]]
              (recur (conj new-path
                           [shifted-curr-x shifted-curr-y]
                           [shifted-next-x shifted-next-y]
                           nil
                           newly-shifted-curr)
                     new-shift-x new-shift-y 
                     (cons newly-shifted-curr (drop 2 points)))))))))) ; replace new head




(defn add-cljplot-path-breaks
  [pts]
  (map (fn [pt] (if-not pt [##NaN ##NaN] pt))
       pts))


(comment

  (def rng (r/make-well19937))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 10)))
  (def stops (w/walk-stops [0 0] 
                           (w/vecs-upto-len 10  step-vector-pool)))
  (count stops)
  (def corrected-stops (correct-path -2 2 stops))
  (def corrected2-stops (correct-path2 -2 2 stops))
  (= corrected-stops corrected2-stops)
  (add-cljplot-path-breaks corrected2-stops)


  (def stops
    [[0 0]
     [0.9700914276659796 1.6365790878224906]
     [-0.824501634774673 2.1803587635156556]
     [1.261448866116044 2.834273995635776]
     [0.21316777054277192 1.0885351519809954]])

  (segs-to-points (correct-path-segs-output -2 2 stops))

  (=
   (correct-path5a -2 2 stops)
   (correct-path5 -2 2 stops)
   (correct-path -2 2 stops)
   (correct-path-loop* -2 2 stops)
   )

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
  (def plot-result
    (let [data (take 5000 stops)] ; stops may have many fewer points
      (-> (cb/series [:grid] [:line (add-cljplot-path-breaks (correct-path-loop* -2 2 data))
                              {:color [0 0 255 150] :margins nil}])
          (cb/preprocess-series)
          (cb/update-scale :x :domain [-8 8])
          (cb/update-scale :y :domain [-8 8])
          (cb/add-axes :bottom)
          (cb/add-axes :left)
          (cr/render-lattice {:width 750 :height 750 :border 10})
          ;(cc/save "yo.jpg")
          (cc/show)
          )))

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


