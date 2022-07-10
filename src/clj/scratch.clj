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


;; by generateme, from 
;; https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288499104
;(def boundary-left -200.0)
;(def boundary-right 200.0)

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
                          [##NaN ##NaN] ; tells cljplot to break continuous lines
                          [(+ curr-x new-shift-x) (+ curr-y new-shift-y)])) ; shifted outside boundaries--duplicate of the point that was *within* boundaries on other side
                                                                            ; next point will be shifted as much, but inside
                  new-shift-x
                  new-shift-y]))
             [[] 0.0 0.0]
             (partition 2 1 path))))) ; points -> segments (with shared endpoints)

;; generateme asked (about the original version of this code--nearly the same):
;; The problems to solve:
;;  1. What is start is not inside a range? (figure out starting offset)
;;  2. What if jump is longer than range size? (apply proper wrapping/modulo)
;;  3. What happened to the last point? (probably partition-all should be used)

;; Answers:
;;  re 3: No, it works correctly as is.  See 



(comment

  (def rng (r/make-well19937))
  (def len-dist (r/make-powerlaw rng 1 2))
  (def step-vector-pool (repeatedly (w/step-vector-fn rng len-dist 1 4000)))
  (def stops (w/walk-stops [0 0] 
                           (w/vecs-upto-len 20 step-vector-pool)))
  (count stops)
  (correct-path -2 2 stops)

  (def stops [[0 0]
              [0.9700914276659796 1.6365790878224906]
              [-0.824501634774673 2.1803587635156556]
              [1.261448866116044 2.834273995635776]
              [0.21316777054277192 1.0885351519809954]])


  ;; based on https://clojurians.zulipchat.com/#narrow/stream/197967-cljplot-dev/topic/periodic.20boundary.20conditions.2Ftoroidal.20world.3F/near/288501054
  (def plot-result
    (let [data (take 5000 stops)] ; stops may have many fewer points
      (-> (cb/series [:grid] [:line (correct-path -2 2 data)
                              {:color [0 0 255 150] :margins nil}])
          (cb/preprocess-series)
          ;(cb/update-scale :x :domain [-2 2])
          ;(cb/update-scale :y :domain [-2 2])
          (cb/update-scale :x :domain [-3 3])
          (cb/update-scale :y :domain [-3 3])
          (cb/add-axes :bottom)
          (cb/add-axes :left)
          (cr/render-lattice {:width 750 :height 750 :border 10})
          ;(cc/save "yo.jpg")
          (cc/show)
          )))

  ;; Based on https://github.com/generateme/cljplot/blob/master/sketches/vega.clj#L570
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


