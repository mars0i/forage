(ns forage.experiment.examplelevywalks
  (:require [forage.run :as fr]
            [utils.random :as r]
            [forage.viz.hanami :as h]
            [oz.core :as oz]))

(def half-size 1000)
(def food-distance 100)
(def params {:food-distance       food-distance
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  food-distance
             :init-loc            [half-size half-size] ; i.e. center of env
             :maxpathlen          half-size  ; for straight walks, don't go too far
             :trunclen            half-size ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
             :fournier-levels     nil
             :fournier-multiplier nil
           })

(def rng (r/make-well19937))

(def fw1 (fr/levy-run rng fr/ignore-food nil params 1.01))
(def fw2 (fr/levy-run rng fr/ignore-food nil params 2))
(def fw3 (fr/levy-run rng fr/ignore-food nil params 3))

(def mu1vegawalk (h/add-walk-labels "mu=1.001" (second fw1)))
;(def mu1vegawalk (h/add-walk-labels "μ=1.001" (second fw1)))

(def mu2vegawalk (h/add-walk-labels "mu=2" (second fw2)))
;(def mu2vegawalk (h/add-walk-labels "μ=2" (second fw2)))

(take 10 mu2vegawalk)

(def mu3vegawalk (h/add-walk-labels "mu=3" (second fw3)))
;(def mu3vegawalk (h/add-walk-labels "μ=3" (second fw3)))

(def mu1plot (h/vega-walk-plot 800 mu1vegawalk))
(def mu2plot (h/vega-walk-plot 800 mu2vegawalk))
(def mu3plot (h/vega-walk-plot 800 mu3vegawalk))

;; If you are making plots for a paper:
(def mu1plot (-> mu1plot (assoc :background "white")))
(def mu2plot (-> mu2plot (assoc :background "white")))
(def mu3plot (-> mu3plot (assoc :background "white")))


(comment
  (oz/start-server!)
  (oz/view! mu3plot)
  (oz/view! mu2plot)
  (oz/view! mu1plot)
)

