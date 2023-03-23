;; spiral20.clj
;; Copied from grid19slide.clj and heavily modified.
(ns forage.experiment.spiral20
  (:require [forage.run :as fr]
            [forage.food :as f]
            [forage.walks :as w]
            [utils.spiral :as sp]
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
  (def v-env (map h/make-foodspot (em/env-foodspot-coords (envs 5))))
  (def food-plot (h/vega-food-plot v-env 2000 400 20))
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
  (def data-and-rng (time (fr/walk-experiments params walk-fns 100 seed)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; OK, LET'S TRY SOME SAMPLE SEARCHES THAT ARE MORE LIKE WHAT I WANT:
  (def seed (r/make-seed))
  (def rng (r/make-well19937 seed))

  (def mu1xdist (r/make-powerlaw rng 1 1.5))
  (def mu2dist (r/make-powerlaw rng 1 2))
  (def mu3dist (r/make-powerlaw rng 1 3))
  ;; These maxlens are way too long, probably, but:
  (defn more-mu1x-vecs [] 
    (w/vecs-upto-len (* 2 half-size) (w/make-levy-vecs rng mu1xdist 1 (params :trunclen))))
  (defn more-mu3-vecs [] 
    (w/vecs-upto-len (* 2 half-size) (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))
  (defn more-spiral-vecs []
    (w/vecs-upto-len (* 2 half-size) (sp/unit-archimedean-spiral-vecs 2 0.1)))

  (defn more-mu2-vecs [] 
    (w/vecs-upto-len (* 2 half-size) (w/make-levy-vecs rng mu2dist 2 (params :trunclen))))
  ;; Note different maxpathlen:
  (defn more-mu2-vecs [] 
    (w/vecs-upto-len (params :maxpathlen) (w/make-levy-vecs rng mu2dist 2 (params :trunclen))))

  ;; Not sure if I'm violating this advice, or if the number of iterations
  ;; are such that I can blow the stack, or what the best way to avoid this is.
  ;; https://stuartsierra.com/2015/04/26/clojure-donts-concat
  (defn composite-mu1-mu3-vecs
    [maxpathlen]
    (w/vecs-upto-len maxpathlen
                     (apply concat
                            (interleave (repeatedly more-mu1x-vecs)
                                        (repeatedly more-mu3-vecs)))))

  ;; IS this one any better?
  ;; https://stuartsierra.com/2015/04/26/clojure-donts-concat
  (defn composite-mu1-mu3-vecs
    [maxpathlen]
    (w/vecs-upto-len maxpathlen
                     (apply concat
                            (repeatedly #(into [] cat [(more-mu1x-vecs) (more-mu3-vecs)])))))
  (defn composite-mu1-spiral-vecs
    [maxpathlen]
    (w/vecs-upto-len maxpathlen
                     (apply concat
                            (repeatedly #(into [] cat [(more-mu1x-vecs) (more-spiral-vecs)])))))


  ;; NOTE NEEDS TO BE A FUNCTION SO THAT YOU GET A DIFFERENT PATH FOR EACH RUN.
  (defn composite-mu1-mu3-vecs []
    (into [] cat [(more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)
                  (more-mu1x-vecs)
                  (more-mu3-vecs)]))

  ;; NOTE NEEDS TO BE A FUNCTION SO THAT YOU GET A DIFFERENT PATH FOR EACH RUN.
  (defn composite-mu1-spiral-vecs []
    (into [] cat [(more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)
                  (more-mu1x-vecs)
                  (more-spiral-vecs)]))

  (defn just-mu2-vecs []
    (into [] cat [(more-mu2-vecs)]))

  (def walk-fns
    {"just-mu2" (fn [init-loc] (w/foodwalk (make-toroidal-look-fn (envs 5))
                                           (params :look-eps) 
                                           (w/walk-stops init-loc (just-mu2-vecs))))})


  (def walk-fns
    (let [look-fn (make-toroidal-look-fn (envs 5))]
      {"composite-spiral"   (fn [init-loc] (w/foodwalk look-fn (params :look-eps) 
                                                       (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
       "composite-brownian" (fn [init-loc] (w/foodwalk look-fn (params :look-eps)
                                                       (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
       "mu2" (partial fr/levy-run rng look-fn nil params 2.0)}))

  (def data-and-rng (time (fr/walk-experiments params walk-fns 10000 seed)))

  (:found-coords data-and-rng)




  ;; What do these walks look like?
  (def walk (w/walk-stops [(* 2 half-size) (* 2 half-size)] composite-mu1-mu3-vecs))
  (def walk (w/walk-stops [(* 2 half-size) (* 2 half-size)] composite-mu1-spiral-vecs))
  (oz/view! (h/vega-envwalk-plot (envs 5) 600 1.0 20 walk))

  (def vl-walk (h/order-walk-with-labels "walk " walk))
  (def plot (h/vega-walk-plot 600 2000 1.0 vl-walk))
  (oz/view! plot)

  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

)
