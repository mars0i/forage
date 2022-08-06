;; Started from fournier7.clj, but this one will have only a single
;; Fournier universe, but it will be toroidal.
(ns forage.experiment.fournier8
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.fractal :as uf]
            [utils.math :as m]))

(def all-exponents [1.001 1.5 2.0 2.5 3.0])
(comment (count all-exponents) )
(def most-exponents (vec (take 3 all-exponents)))
(def addl-exponents (vec (drop 3 all-exponents)))

(def walks-per-combo 1000)

(def half-size 1000) ; half the full width of the env
;(def init-food-distance (double (* 2/3 half-size)))
;(def init-food  (+ half-size init-food-distance))
;(def -init-food (- half-size init-food-distance))
(def fournier-mult 0.25)
(def fournier-lvls 3)
(def perc-radius 1)

;; FOR LEVY WALKS
(def params (sorted-map ; sort so labels match values
             :food-distance       init-food ; ignored??
             :perc-radius         perc-radius ; distance that an animal can "see" in searching for food
             :powerlaw-min        perc-radius ; s/b >= per-radius (Viswanathan et al typically make them equal)
             :env-size            (* 2 half-size)
             :env-discretization  (* init-food (reduce * (repeat fournier-lvls fournier-mult)))
             :init-loc-fn  (constantly [half-size half-size])
             :maxpathlen          (* 5 half-size)  ; for straight walks, don't go too far
             :trunclen            (* 5 half-size) ; max length of any line segment
             :look-eps            0.1    ; increment within segments for food check
             :num-dirs            nil    ; split range this many times + 1 (includes range max); nil for random
             :max-frac            0.25   ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
            ))

(def env 
  (mf/make-env (params :env-discretization) (params :env-size)
               (uf/fournierize [[half-size half-size]]
                               init-food
                               fournier-mult
                               fournier-lvls)
               ;(uf/fournierize2d fournier-lvls ? [[half-size half-size]])
               ))

;(f/remove-center (params :env-size) (params :env-size)  ... )

;; NOTE TOROIDAL LOOK-FN:
(def look-fn (partial mf/perc-foodspots-exactly-toroidal env (params :perc-radius)))


(comment

  (require '[forage.viz.hanami :as h] :reload)
  (require '[utils.hanami :as uh])
  (require '[aerial.hanami.common :as hc])
  (require '[aerial.hanami.templates :as ht])
  (require '[oz.core :as oz])

  ;; plot the foodspots alone:
  (oz/start-server!)
  (oz/view! (h/vega-env-plot env 600 10))
  (oz/view! (h/vega-env-plot base-env 600 50))

  (require '[utils.fractal :as uf])
  (require '[utils.random :as r])
  (require '[forage.run :as fr])

)
