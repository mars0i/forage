;; Started from fournier7.clj, but this one will have only a single
;; Fournier "universe", though embedded in a toroidal environment
(ns forage.experiment.fournier8
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.mason.foodspot :as mf]
            [utils.random :as r]
            [utils.fractal :as uf]
            [utils.math :as m]
            [clojure.math.numeric-tower :as nt :refer [expt]]))

;(def all-exponents [1.001 1.5 2.0 2.5 3.0])
;(comment (count all-exponents) )


;; NOTE that if the gap over the border is the right size, then considering only
;; the wrapping directly west/east/south/north, what you have is in effect a
;; much larger Fournier universe, i.e. one with an additional level.
;; But when you take into account the diagonals, there is something there that's
;; not in a pure Fournier universe.
;; Which also points out that one kind of way to supplement a Fournier universe
;; with diagonal elements is to make it small and let the wrapping give you 
;; additional diagonal "components" of the universe.

;; NOTE: If you have (e.g.) five levels, but they are sufficiently
;; spread out, and your maxpathlen and trunclen params are not too large,
;; then it's as if you're searching in a 3-level or 4-level world, but
;; but with fractal repetitions at a higher level (and without diagonal
;; repetitions until farther out).


;; TODO:
;; Try both with start in center (which is what I did before in ForagePSA)
;; and start from end of last walk.
;; Q: Do I need to do multiple start-from-last-walk runs?  Cap their max length?


(def fournier-mult 0.25)
;(def fournier-levels 5) 
;(def fournier-levels 4)
(def fournier-levels 3)
;; With above params, minimum distance between foodspots is:
;(def fournier-init-offset 2000000) ; min distance = 1953.125
;(def fournier-init-offset  1500000) ; min distance = 1464.84375   <-- THIS ONE
;(def fournier-init-offset 1000000) ; min dist = 976.5625:
;(def fournier-init-offset 50000) ; min dist = 1171.875
(def fournier-init-offset 25000) ; min dist = 390.625
;(def fournier-init-offset 200000) ; min dist = 195.3125  ; FOR TESTING

(def half-size (* 0.375 fournier-init-offset)) ; ?: this makes dist to edge 1/2 dist between outer points of largest strus

(comment
  ;; Minimium distance between foodspots
  (* fournier-init-offset (nt/expt fournier-mult fournier-levels))

  (apply min (map first (mf/env-foodspot-coords env))) ; min from foodspot to boundary
  (* 2 (apply min (map first (mf/env-foodspot-coords env)))) ; min dist tween foodspots across boundaries
  (- 7421.875 5078.125) ; min dist between clusters within boundaries
)

(def perc-radius 1)

(def params (sorted-map ; sort so labels match values
             :env-size            (* 2 half-size)
             :env-discretization  (* fournier-init-offset (reduce * (repeat fournier-levels fournier-mult)))
             :powerlaw-min        perc-radius ; s/b >= per-radius (Viswanathan et al typically make them equal)
             :maxpathlen          (* 20 half-size)
             :perc-radius         perc-radius ; distance that an animal can "see" in searching for food
             :trunclen            (* 1 half-size) ; max length of any line segment
             :init-loc-fn         (partial fr/end-of-walk [half-size half-size]) ; start from end of previous foodwalk, after starting in center.
             ;:init-loc-fn         (constantly [half-size half-size]) ; always start in center
             :init-pad            (* 5 perc-radius) ; if truthy, initial loc offset by this in rand dir
             :look-eps            0.1    ; increment within segments for food check
            ))

(def env 
  (mf/make-env (params :env-discretization) (params :env-size)
               (uf/fournierize [[half-size half-size]]
                               fournier-init-offset
                               fournier-mult
                               fournier-levels)))

;(f/remove-center (params :env-size) (params :env-size)  ... )

;; NOTE TOROIDAL LOOK-FN:
(def look-fn (partial mf/perc-foodspots-exactly-toroidal env (params :perc-radius)))


(comment
  ;; Display foodspot plots
  (require '[forage.viz.hanami :as h] :reload)
  (require '[oz.core :as oz])
  ;(require '[utils.toroidal :as tor])

  (oz/start-server!)
  (oz/view! (h/vega-env-plot env 800 150))
  (oz/export! (h/vega-env-plot env 5000 150) "yo.svg")

  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  (def rand-init-params
    (assoc params :init-loc-fn (partial fr/rand-foodspot-coord-pair rng env)))

  (def fw (time (fr/levy-run rng look-fn nil params 2 [half-size half-size])))
  (def fw (time (fr/levy-run rng look-fn nil params 2 (fr/rand-foodspot-coord-pair rng env nil)))) ; override start
  (first (nth fw 1))
  (class (first fw)) ; did we find food?
  (count (nth fw 1)) ; length of did path
  (count (nth fw 2)) ; length of couldve path
  (mf/foodspot-coords (first (first fw))) ; where'd we find food?
  (time (oz/view! (h/vega-envwalk-plot env 800 0.5 100 (nth fw 1)))) ; did
  (time (oz/view! (h/vega-envwalk-plot env 2000 2 1000 (nth fw 2)))) ; couldve
  (time (oz/view! (h/vega-didcould-envwalk-plot env 800 1 100 [fw])))

  (def my-params params)
  (def my-params rand-init-params)
  (def data (time (fr/levy-experiments fr/default-file-prefix env my-params
                                       ;[1.5 2.0 2.5] 
                                       [1.001 1.5 2.0 2.5 3.0] 
                                       1000 seed look-fn)))

)
