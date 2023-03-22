;; spiral20.clj
;; Copied from grid19slide.clj and heavily modified.
(ns forage.experiment.spiral20
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.env-mason :as em]
            [utils.random :as r]))

(def default-dirname "../../data.foraging/forage/")

;; FOOD-DISTANCE SHOULD DIVIDE HALF-SIZE EVENLY, OR THERE WON't BE FOOD AT CENTER,
;; WHICH IS WHERE THE SEARCH STARTS.
(def half-size 500) ; half the full width of the env
(def maxpathlen (* 20 half-size)) ; max total length of search path
(def food-distance nil) ; won't be used

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance 
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size]) ; TODO MODIFY THIS?
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            half-size
             :look-eps            0.2    ; TODO WILL THIS WORK WITH SHORTER SPIRAL SEGMENTS?
             :basename            (str default-dirname "spiral20_")
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SINGLE-FOODSPOT ENV

(defn make-single-target-env
  "Make an env with a single foodspot between the center of the core env
  and the right edge along the equator."
  [denom nomin]
  (let [half-size (/ (params :env-size) 2)] ; probably already defined as var, but should get from params
    (em/make-env (params :env-discretization)
                 (params :env-size)
                 [[(long (+ half-size (* (/ nomin denom) half-size))) ; coerce to long: avoid probs later with Ratio, BigInt
                   half-size]])))
                 
(def envs (mapv (partial make-single-target-env 10)
                (range 1 10)))

(defn make-toroidal-look-fn
  [env]
  (partial em/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(defn make-unbounded-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (partial em/perc-foodspots-exactly env (params :perc-radius)))


(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LET's SEE WHAT THESE ENVS LOOK LIKE:
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)
  ;; Piecemeal:
  (def v-env (map h/make-foodspot (em/env-foodspot-coords (envs 0))))
  (def food-plot (h/vega-food-plot v-env 1000 400 20))
  (oz/view! food-plot)
  ;; All at once, animated:
  (dotimes [i 9]
    (oz/view! (h/vega-food-plot (map h/make-foodspot (em/env-foodspot-coords (envs i))) 1000 400 20))
    (Thread/sleep 1000))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LET'S TRY RUNNING SOME SEARCHES TO MAKE SURE IT WORKS:
  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))
  (def walk-fns {"1.01" (partial fr/levy-run rng (make-toroidal-look-fn (envs 4)) nil params 1.01)
                 "1.5"  (partial fr/levy-run rng (make-toroidal-look-fn (envs 4)) nil params 1.5)
                 "2.0"  (partial fr/levy-run rng (make-toroidal-look-fn (envs 4)) nil params 2.0)
                 "3.0"  (partial fr/levy-run rng (make-toroidal-look-fn (envs 4)) nil params 3.0)})
  (def data-and-rng
    (time
      (fr/walk-experiments
        (envs 4) params walk-fns 10 seed (make-toroidal-look-fn (envs 4)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; OK, LET'S TRY SOME SAMPLE SEARCHES THAT ARE MORE LIKE WHAT'S INTENDED

)
