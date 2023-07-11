;; ENV-MATRIX version of spiral22.clj

;; Experiments that use walks that are composites of:
;;    - Spiral walks
;;    - Levy walks with various mu values (including ballistic and Brownian)
(ns forage.matruns.spiral22mat
  (:require ;[criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
	    [clojure.core.matrix :as mx]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-mason :as emas]
            [forage.core.env-matrix :as emat]
            [utils.random :as r]
            [utils.spiral :as sp]))


;(def $ "THIS FUNCTION, $, IS AN ABBREVIATION FOR partial." partial)
;; partial is a lot slower than (fn [...] ...) with four or more args 
;; passed to the fn, but only would matter in an inner loop.


(def default-dirname "../../data.foraging/forage/spiral22mat/")

;; Amount to increase sizes so that perceptual radii can be represented as
;; an approximately circular set of cells in the environment matrix.
;; See note in env_matrix.clj for discussion.
(def env-scale 100)

(def half-size  10000) ; same as spiral22.clj
(def scaled-half-size (* env-scale half-size))

(def half-size 1000) (println "\nUSING SMALL ENV SIZE FOR TESTING.\n")
(def scaled-half-size (* 50 half-size))

(def maxpathlen (* 100 scaled-half-size)) ; max length of an entire continuous search path
(def explore-segment-len (/ maxpathlen 400.0)) ; max length of walk segments that go far
(def examine-segment-len (/ maxpathlen 50.0))  ; max length of walk segments that stay local (not exploit, but rather "look closely", examine)
(def trunclen explore-segment-len)
(def food-distance nil) ; won't be used

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :food-distance       food-distance 
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 scaled-half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [scaled-half-size scaled-half-size])
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            trunclen
             :look-eps            0.2    ; TODO WILL THIS WORK WITH SHORTER SPIRAL SEGMENTS?
             :basename            (str default-dirname "spiral22mat_")
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP

(defn make-target
  "Make coordnates of a single foodspot between the center of the core env
  and the right edge along the equator, at a distance of denom/nomin."
  [denom nomin]
  (let [half-size (/ (params :env-size) 2)] ; probably already defined as var, but should get from params
    [[(long (+ half-size (* (/ nomin denom) half-size))) ; coerce to long: avoid probs later with Ratio, BigInt
      half-size]]))

(comment
  (def t (make-target 5 2))
)

(defn make-trimmed-env
  [denom nomin]
  (let [targets (make-target denom nomin)
        env (emat/make-env (params :env-size))]
    (emat/add-trimmed-foodspots! env (params :perc-radius) targets)
    env))

;; UNUSED AT PRESENT, but CHECK BELOW.
(defn make-toroidal-env
  [denom nomin]
  (let [targets (make-target denom nomin)
        env (emat/make-env (params :env-size))]
    (emat/add-toroidal-foodspots! env (params :perc-radius) targets)
    env))

(comment
  (def trimenv (make-trimmed-env 5 2))
  (class trimenv)
  (:size trimenv)
  (class (:locs trimenv))
  (mx/shape (:locs trimenv))
  (emat/env-foodspot-coords trimenv)

  ;; Don't do this unjudiciously: (mx/pm (:locs trimenv))
  ;; There may be many millions of cells, and you'll wait a long time.

  (def torenv (make-toroidal-env 5 2))
  (class torenv)
  (:size torenv)
  (mx/shape (:locs torenv))
  (emat/env-foodspot-coords torenv)
)


(def trimmed-envs (mapv (partial make-trimmed-env 5)
                        (range 1 6))) ; five targets at 1/5, 2/4, 3/4, 4/5, 5/5 of distance to border

;; UNUSED AT PRESENT, but CHECK BELOW.
;; Make envs each with a single target but at several different distances
;; from center as proportion of size of env:
(def toroidal-envs (mapv (partial make-toroidal-env 5)
                         (range 1 6))) ; five targets at 1/5, 2/4, 3/4, 4/5, 5/5 of distance to border

(comment
  (count toroidal-envs)
  (count trimmed-envs)
)

(defn make-unbounded-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [trimmed-env]
  (partial emat/perc-foodspots-trimmed trimmed-env (params :perc-radius)))

;; UNUSED AT PRESENT, but CHECK BELOW.
(defn make-toroidal-look-fn
  [toroidal-env]
  (partial emat/perc-foodspots-toroidal toroidal-env (params :perc-radius)))


;;;;;;;;;;;;;;;;;;;
;; TODO REVISE/TEST FROM HERE
;; Stuff below worked in spiral22.clj.  Does it work here?
;; Are changes needed?
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS

(def seed (r/make-seed))
(def seed 103758742532356121)
(println "Using seed" seed)
(def rng (r/make-well19937 seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that construct component walks

;; Component distributions
(def mu1dist (r/make-powerlaw rng 1 1.1))
(def mu15dist (r/make-powerlaw rng 1 1.5))
(def mu3dist (r/make-powerlaw rng 1 3))
;; I use mu=other values as well, but only using my older levy-experiments interface

;; Component walks
(defn more-mu1-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu1dist 1 (params :trunclen))))
(defn more-mu15-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))
(defn more-mu3-vecs [] 
  (w/vecs-upto-len examine-segment-len (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))
(defn more-spiral-vecs []
  (w/vecs-upto-len examine-segment-len (sp/unit-archimedean-spiral-vecs 2 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that construct composite walks

;; composite mu=1.1 and mu=3 walk
(defn composite-mu1-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu1-vecs)
                                      (repeatedly more-mu3-vecs)))))

;; composite mu=1.5 and mu=3 walk
(defn composite-mu15-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu15-vecs)
                                      (repeatedly more-mu3-vecs)))))

;; composite mu=1.1 and spiral walk
(defn composite-mu1-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu1-vecs)
                                      (repeatedly more-spiral-vecs)))))

;; composite mu=1.5 and spiral walk
(defn composite-mu15-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu15-vecs)
                                      (repeatedly more-spiral-vecs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maps whose values are functions that run composite and non-composite 
;; walks in each of the different TRIMMED environments defined above.

;; composite mu=1.1 and mu=3
(def mu1-mu3-walk-fns
  {"composite-mu1-mu3-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))})

;; composite mu=1.5 and mu=3
(def mu15-mu3-walk-fns
  {"composite-mu15-mu3-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))})

;; composite mu=1.1 and spiral
(def mu1-spiral-walk-fns
  {"composite-mu1-spiral-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))})

;; composite mu=1.5 and spiral
(def mu15-spiral-walk-fns
  {"composite-mu15-spiral-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (trimmed-envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))})

;; pure mu=2 walks (using my older interface)
(def mu15-walk-fns
  {"mu15-env0" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 0)) nil params 1.5)
   "mu15-env1" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 1)) nil params 1.5)
   "mu15-env2" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 2)) nil params 1.5)
   "mu15-env3" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 3)) nil params 1.5)})

;; pure mu=2 walks (using my older interface)
(def mu2-walk-fns
  {"mu2-env0" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 0)) nil params 2.0)
   "mu2-env1" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 1)) nil params 2.0)
   "mu2-env2" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 2)) nil params 2.0)
   "mu2-env3" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 3)) nil params 2.0)})

;; pure mu=2.5 walks (using my older interface)
(def mu25-walk-fns
  {"mu25-env0" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 0)) nil params 2.5)
   "mu25-env1" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 1)) nil params 2.5)
   "mu25-env2" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 2)) nil params 2.5)
   "mu25-env3" (partial fr/levy-run rng (make-unbounded-look-fn (trimmed-envs 3)) nil params 2.5)})


   ;; TO ADD: Lévy searchers or ballistic searches with perceptual
   ;; radius equal to the spiral size.

   ;; AND MAYBE ADD: Full spiral of length equal to maxpathlen
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO repeat the above using TOROIDAL ENVS.

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; EXAMINE, CONFIGURE:

  (clojure.repl/pst)

  ;; WHAT DO THESE WALKS LOOK LIKE?
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def env (trimmed-envs 2))

  (def walk1s (time (w/walk-stops [half-size half-size] (composite-mu1-spiral-vecs (params :maxpathlen)))))
  (def vwalk1s (time (h/vega-envwalk-plot env 600 0.5 15 walk1s :foodspots-on-top? true)))
  (time (oz/view! vwalk1s))

  (def walk15s (time (w/walk-stops [half-size half-size] (composite-mu15-spiral-vecs (params :maxpathlen)))))
  (def vwalk15s (time (h/vega-envwalk-plot env 600 0.5 15 walk15s :foodspots-on-top? true)))
  (time (oz/view! vwalk15s))

  (def walk13 (time (w/walk-stops [half-size half-size] (composite-mu1-mu3-vecs (params :maxpathlen)))))
  (def vwalk13 (time (h/vega-envwalk-plot env 600 0.5 15 walk13 :foodspots-on-top? true)))
  (time (oz/view! vwalk13))

  (def walk153 (time (w/walk-stops [half-size half-size] (composite-mu15-mu3-vecs (params :maxpathlen)))))
  (def vwalk153 (time (h/vega-envwalk-plot env 600 0.5 15 walk153 :foodspots-on-top? true)))
  (time (oz/view! vwalk153))

  ;; Try this instead:
  ;(require '[aerial.hanami.common :as hc])
  ;(oz/export! vwalk13
  ;            "compositeBrownianWalkExampleSpiral21commit_02cbde0_seed-7370724773351240133.png")

  ;(time (h/write-foodwalk-plots "compositemu1m3" :svg seed (envs 0) 600 1 1 1.0 0 "1.25,3" params [walk13]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RUN THE EXPERIMENTS

  ;; These took about two hours each:
  (def mu1-spiral-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "mu1-spiral")) mu1-spiral-walk-fns 2000 seed)))
  (def mu1-mu3-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "mu1-mu3")) mu1-mu3-walk-fns 2000 seed)))

  ;; This took one hour:
  (def mu2-data-and-rng  (time (fr/walk-experiments (update params :basename #(str % "mu2"))  mu2-walk-fns  2000 seed)))
  ;; An hour and 45 minutes:
  (def mu25-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "mu25")) mu25-walk-fns 2000 seed)))

  ;; 45-50 minutes:
  (def mu15-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "mu15")) mu15-walk-fns 2000 seed)))

  ;; two hours and 10 minutes:
  (def mu15-mu3-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "mu15-mu3")) mu15-mu3-walk-fns 2000 seed)))

  ;; one hour, 35 minutes:
  (def mu15-spiral-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "mu15-spiral")) mu15-spiral-walk-fns 2000 seed)))

)

