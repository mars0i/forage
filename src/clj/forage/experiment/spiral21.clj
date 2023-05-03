;; spiral21.clj
;; Copied from spiral20.clj and modified.
(ns forage.experiment.spiral21
  (:require ;[criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.env-mason :as em]
            [utils.random :as r]
            [utils.spiral :as sp]))

(def default-dirname "../../data.foraging/forage/")

(def half-size  50000) ; half the full width of the env
(def maxpathlen (* 200 half-size)) ; max length of an entire continuous search path
(def explore-segment-len (/ maxpathlen 1000.0)) ; max length of walk segments that go far
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
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size]) ; TODO MODIFY THIS?
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            trunclen
             :look-eps            0.2    ; TODO WILL THIS WORK WITH SHORTER SPIRAL SEGMENTS?
             :basename            (str default-dirname "spiral21_")
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP

(defn make-single-target-env
  "Make an env with a single foodspot between the center of the core env
  and the right edge along the equator."
  [denom nomin]
  (let [half-size (/ (params :env-size) 2)] ; probably already defined as var, but should get from params
    (em/make-env (params :env-discretization)
                 (params :env-size)
                 [[(long (+ half-size (* (/ nomin denom) half-size))) ; coerce to long: avoid probs later with Ratio, BigInt
                   half-size]])))

;; Make envs each with a single target but at several different distances
;; from center as proportion of size of env:
(def envs (mapv (partial make-single-target-env 5)
                (range 1 5))) ; four targets at 1/5, 2/4, 3/4, 4/5 of distance to border

(comment
  (count envs)
)

(defn make-toroidal-look-fn
  [env]
  (partial em/perc-foodspots-exactly-toroidal env (params :perc-radius)))

(defn make-unbounded-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (partial em/perc-foodspots-exactly env (params :perc-radius)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS

(def seed (r/make-seed))
(def seed -7370724773351240133)
(def rng (r/make-well19937 seed))
(def mu1xdist (r/make-powerlaw rng 1 1.25))
(def mu3dist (r/make-powerlaw rng 1 3))

(defn more-mu1x-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu1xdist 1 (params :trunclen))))
(defn more-mu3-vecs [] 
  (w/vecs-upto-len examine-segment-len (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))
(defn more-spiral-vecs []
  (w/vecs-upto-len examine-segment-len (sp/unit-archimedean-spiral-vecs 2 0.1)))

(defn composite-mu1-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu1x-vecs)
                                      (repeatedly more-mu3-vecs)))))
(defn composite-mu1-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly more-mu1x-vecs)
                                      (repeatedly more-spiral-vecs)))))

(def brown-walk-fns
  {"composite-brownian-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-brownian-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-brownian-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-brownian-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))})

(def spiral-walk-fns
  {"composite-spiral-env0"   (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-spiral-env1"   (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-spiral-env2"   (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-spiral-env3"   (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))})

(def mu2-walk-fns
  {"mu2-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 2.0)
   "mu2-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 2.0)
   "mu2-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 2.0)
   "mu3-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 2.0)})

   ;; TO ADD: Lévy searchers or ballistic searches with perceptual
   ;; radius equal to the spiral size.

   ;; AND MAYBE ADD: Full spiral of length equal to maxpathlen
   


(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; EXAMINE, CONFIGURE:

  ;; What do these walks look like?
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def walk1s (time (w/walk-stops [half-size half-size] (composite-mu1-spiral-vecs (params :maxpathlen)))))
  (time (oz/view! (h/vega-envwalk-plot (envs 0) 600 1.0 1000 walk1s)))

  (def walk13 (time (w/walk-stops [half-size half-size] (composite-mu1-mu3-vecs (params :maxpathlen)))))
  (def vwalk13 (time (h/vega-envwalk-plot (envs 0) 600 1.0 1000 walk13)))  ; 8 minutes
  (time (oz/view! vwalk13)) ; returns quickly but DOESN'T DISPLAY
  ;; Try this instead:
  ;(require '[aerial.hanami.common :as hc])
  (oz/export! vwalk13
              "compositeBrownianWalkExampleSpiral21commit_02cbde0_seed-7370724773351240133.png")

  ;(time (h/write-foodwalk-plots "compositemu1m3" :svg seed (envs 0) 600 1 1 1.0 0 "1.25,3" params [walk13]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RUN THE EXPERIMENTS
  (def spiral-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "spiral")) spiral-walk-fns 100 seed)))
  (def mu2-data-and-rng    (time (fr/walk-experiments (update params :basename #(str % "mu2")) mu2-walk-fns 100 seed)))
  (def brown-data-and-rng  (time (fr/walk-experiments (update params :basename #(str % "brown")) brown-walk-fns 100 seed)))

)
