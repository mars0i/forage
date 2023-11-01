; ; Like spiral23profiling and spiral24profiling, but uses env-single instead 
;; of env-mason or env-minimal namespace, and usees single-target envs.
(ns forage.experiment.spiral25profiling
  (:require ;[criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
            ;[clojure.math :as cmath]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-single :as env]
            [utils.math :as um]
            [utils.random :as r]
            [utils.spiral :as sp]
            [utils.csv :as csv]))



(def targets-per-env 1)

(def homedir (System/getenv "HOME"))
(def default-dirname (str homedir "/docs/src/data.foraging/forage/spiral25"))

(def half-size  10000) ; half the full width of the env
(def maxpathlen (* 1000 half-size)) ; max length of an entire continuous search path
(def explore-segment-len (/ maxpathlen 400.0)) ; max length of walk segments that go far
(def examine-segment-len (/ maxpathlen 50.0))  ; max length of walk segments that stay local (not exploit, but rather "look closely", examine)
(def trunclen explore-segment-len)
(def food-distance nil) ; won't be used

;; Initial default params, with:
;; (a) Search starts in a random initial direction
;; (b) Search starts exactly from init-loc (e.g. for destructive search)
(def params (sorted-map ; sort so labels match values
             :perc-radius         1  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1
             :env-size            (* 2 half-size)
             :init-loc-fn         (constantly [half-size half-size])
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            trunclen
             :basename            (str default-dirname "spiral25_")
             :look-eps            nil ; shouldn't be used
             :foodspot-coords-fn  env/foodspot-coords
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP

(defn shift-point
  "Shifts the point [x y] to the right by inc-x and up by inc-y (where
  these last two values may be negative)."
  [inc-x inc-y [x y]]
  [(+ x inc-x) (+ y inc-y)])

;; A bit more complicated than needed because it's a modified version
;; of make-multiple-target-env from spiral2[34]*.clj.
(defn make-single-target-env
  "Make an env with a num-targets multiple foodspots at the same
  distance--the proportion denom/nomin of half-size from center of env,
  equally spaced in radians."
  [denom nomin]
  ;; coerce to doubles to avoid probs later with Ratio, BigInt:
  (let [half-size (double (/ (params :env-size) 2)) ; probably already defined as var, but should get from params
        distance-from-zero (* (/ nomin denom) half-size)
        zero-tgt [distance-from-zero 0] ; east from origin--will be shifted later
        directions [0]
        target (shift-point half-size half-size zero-tgt)]
    (prn target) ; DEBUG
    (env/make-single-foodspot-env (target 0) (target 1))))


;; Make envs each with a single target but at several different distances
;; from center as proportion of size of env:
(def envs (mapv (partial make-single-target-env 5)
                (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border
;; NOTE The fifth env may be unused below.

(defn make-unbounded-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (env/make-look-fn env (params :perc-radius)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS

(def seed (r/make-seed))
(def seed -7370724773351240133)
(println "Using seed" seed)
(def rng (r/make-well19937 seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that construct component walks

;; Component distributions
(def mu1dist (r/make-powerlaw rng 1 1.1))
(def mu15dist (r/make-powerlaw rng 1 1.5))
(def mu2dist (r/make-powerlaw rng 1 2))
(def mu3dist (r/make-powerlaw rng 1 3))
;; I use mu=other values as well, but only using my older levy-experiments interface

;; Component walks
(defn more-mu1-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu1dist 1 (params :trunclen))))
(defn more-mu15-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))
(defn more-mu2-vecs [] 
  (w/vecs-upto-len explore-segment-len (w/make-levy-vecs rng mu2dist  1 (params :trunclen))))

(defn more-mu3-vecs [] 
  (w/vecs-upto-len examine-segment-len (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))
(defn more-spiral-vecs []
  (w/vecs-upto-len examine-segment-len (sp/unit-archimedean-spiral-vecs 2 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that construct composite walks

(defn mu2-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen (more-mu2-vecs)))

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
;; walks in each of the different environments defined above.

;; NOTE Only using the first four envs (i.e. the first four target distances).

(def mu2-walk-fns
  {"mu2-env0" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
   "mu2-env1" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
   "mu2-env2" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
   "mu2-env3" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))})


(defn straight-path [init-loc] [init-loc [(params :maxpathlen) (init-loc 1)]])

(def straight-walk-fns
  {"mu2-env0" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 0)) "IGNORED" (straight-path init-loc)))
   "mu2-env1" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 1)) "IGNORED" (straight-path init-loc)))
   "mu2-env2" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 2)) "IGNORED" (straight-path init-loc)))
   "mu2-env3" (fn [init-loc] (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 3)) "IGNORED" (straight-path init-loc)))
   "mu2-env4" (fn [init-loc]
                (prn (envs 4) (straight-path init-loc)) ; DEBUG
                (w/foodwalk env/find-in-seg (make-unbounded-look-fn (envs 4)) "IGNORED" (straight-path init-loc)))})



;; FIXME
;; composite mu=1.1 and mu=3
(def mu1-mu3-walk-fns
  {"composite-mu1-mu3-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
   "composite-mu1-mu3-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))})

;; FIXME
;; composite mu=1.5 and mu=3
(def mu15-mu3-walk-fns
  {"composite-mu15-mu3-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
   "composite-mu15-mu3-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))})

;; FIXME
;; composite mu=1.1 and spiral
(def mu1-spiral-walk-fns
  {"composite-mu1-spiral-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
   "composite-mu1-spiral-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))})

;; FIXME
;; composite mu=1.5 and spiral
(def mu15-spiral-walk-fns
  {"composite-mu15-spiral-env0" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env1" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env2" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
   "composite-mu15-spiral-env3" (fn [init-loc] (w/foodwalk (make-unbounded-look-fn (envs 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))})

;; pure mu=1.5 walks (using my older interface)
;; FIXME
; (def mu15-walk-fns
;   {"mu15-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 1.5)
;    "mu15-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 1.5)
;    "mu15-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 1.5)
;    "mu15-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 1.5)})
; 
; ;; pure mu=2 walks (using my older interface)
; (def mu2-walk-fns
;   {"mu2-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 2.0)
;    "mu2-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 2.0)
;    "mu2-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 2.0)
;    "mu2-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 2.0)})
; 
; ;; pure mu=2.5 walks (using my older interface)
;; FIXME
; (def mu25-walk-fns
;   {"mu25-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 2.5)
;    "mu25-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 2.5)
;    "mu25-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 2.5)
;    "mu25-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 2.5)})



(comment
  ;; TESTS


  (def walks-per-fn 1000)

  (def seed -7370724773351240133)
  (def rng (r/make-well19937 seed))
  (def initial-state (r/get-state rng))

  ;; This is finding no targets.  It should find a target every time!
  ;; I should try this with spiral23.
  (def straight-data-and-rng
    (time (fr/walk-experiments (update params :basename #(str % "straight"))
                               straight-walk-fns 10 seed)))

  ;; This is finding no targets.  Should it?
  (def mu2-data-and-rng
    (time (fr/walk-experiments (update params :basename #(str % "mu2"))
                               mu2-walk-fns walks-per-fn seed)))

  (clojure.repl/pst)

  ;(crit/quick-bench ; will run at least 60 iterations
  (time
    (do
      ;; These setup calls are needed to make each Criterium run the same.
      ;; On my MBP the average time added by them is 2.859466 µs, i.e. < 3/1,000,000 second.
      (r/set-state rng initial-state)
;      (let [mu2-walk-fns
;            {"mu2-env0" (partial fr/levy-run rng (make-unbounded-look-fn (envs 0)) nil params 2.0)
;             "mu2-env1" (partial fr/levy-run rng (make-unbounded-look-fn (envs 1)) nil params 2.0)
;             "mu2-env2" (partial fr/levy-run rng (make-unbounded-look-fn (envs 2)) nil params 2.0)
;             "mu2-env3" (partial fr/levy-run rng (make-unbounded-look-fn (envs 3)) nil params 2.0)}]
;      (fr/walk-experiments (update params :basename #(str % "mu2"))
;                           mu2-walk-fns walks-per-fn seed rng))
     )
    )
  ;) ; quick-bench

)
