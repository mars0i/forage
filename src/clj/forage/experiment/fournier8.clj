;; Started from fournier7.clj, but this one will have only a single
;; Fournier "universe", though embedded in a toroidal environment
(ns forage.experiment.fournier8
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.fractal :as uf]
            [utils.math :as m]))

;(def all-exponents [1.001 1.5 2.0 2.5 3.0])
;(comment (count all-exponents) )

(def walks-per-combo 1000)

(def half-size 75000) ; half the full width of the env
(def fournier-init-offset 200000)
(def fournier-mult 0.25)
(def fournier-lvls 5)

(comment
  ;; Example Fournier foodspot configurations

  ;; 5-LEVEL:

  ;; Smallest horizontal/vertical gap: 195 units.
  ;; Largest horizontal/vertical gap: 16796 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 3398.5 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 6797 units.
  (def half-size 70000)
  (def fournier-init-offset 200000)
  (def fournier-mult 0.25)
  (def fournier-lvls 5)
  ;; Or if half-size is changed to 75000,
  (def half-size 75000) ; then 
  ;; nearest horizontal/vertical distance from outer point to border: 8398.5 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 16797 units,
  ;; i.e. same as internal largest gap.



  ;; 4-LEVEL:

  ;; Smallest horizontal/vertical gap: 111 units.
  ;; Largest horizontal/vertical gap: 2449 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 537 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 1074 units.
  ;;;;;;;;
  (def half-size 10000)
  (def fournier-init-offset 28500)
  (def fournier-mult 0.25)
  (def fournier-lvls 4)

  ;;;;;;;;
  ;; Smallest horizontal/vertical gap: 10 units.
  ;; Largest horizontal/vertical gap: 215 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 170 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 340 units.
  (def half-size 1000)
  (def fournier-init-offset 2500)
  (def fournier-mult 0.25)
  (def fournier-lvls 4)

  ;; 3-LEVEL:

  ;;;;;;;;
  ;; Smallest horizontal/vertical gap: 219 units.
  ;; Largest horizontal/vertical gap: 952.5 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 406 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 812 units.
  (def half-size 5000)
  (def fournier-init-offset 14000)
  (def fournier-mult 0.25)
  (def fournier-lvls 3)

  ;;;;;;;;
  ;; Smallest horizontal/vertical gap: 39 units.
  ;; Largest horizontal/vertical gap: 234 units.
  ;; Nearest horizontal/vertical distance from outer point to border: 180 units.
  ;; So the shortest horizontal/vertical toroidally across a border: 360 units.
  (def half-size 1000)
  (def fournier-init-offset 2500)
  (def fournier-mult 0.25) ; above 0.25, not really fractal: outer points too close
  (def fournier-lvls 3)
)

(def perc-radius 1)

;; FOR LEVY WALKS
(def params (sorted-map ; sort so labels match values
             :food-distance       nil
             :perc-radius         perc-radius ; distance that an animal can "see" in searching for food
             :powerlaw-min        perc-radius ; s/b >= per-radius (Viswanathan et al typically make them equal)
             :env-size            (* 2 half-size)
             :env-discretization  (* fournier-init-offset (reduce * (repeat fournier-lvls fournier-mult)))
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
                               fournier-init-offset
                               fournier-mult
                               fournier-lvls)))

(def eight-env 
  (mf/make-env (params :env-discretization) (params :env-size)
               (uf/eight-fournierize [[half-size half-size]]
                                     fournier-init-offset
                                     fournier-mult
                                     fournier-lvls)))

;(f/remove-center (params :env-size) (params :env-size)  ... )

;; NOTE TOROIDAL LOOK-FN:
(def look-fn (partial mf/perc-foodspots-exactly-toroidal env (params :perc-radius)))


(comment

  (require '[forage.viz.hanami :as h] :reload)
  (require '[oz.core :as oz])

  ;; plot the foodspots alone:
  (oz/start-server!)
  (oz/view! (h/vega-env-plot env 2000 300))
  (oz/view! (h/vega-env-plot env 600 235))
  (oz/export! (h/vega-env-plot env 600 500) "yo.svg")
  (oz/view! (h/vega-env-plot env 600 70))
  (oz/view! (h/vega-env-plot eight-env 600 70))
  (oz/export! (h/vega-env-plot eight-env 600 70) "yo.svg")
  (oz/view! (h/vega-env-plot env 600 25))
  (oz/view! (h/vega-env-plot env 600 10))
  (oz/view! (h/vega-env-plot env 600 5))

)
