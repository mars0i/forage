;; Based on spiral25profiling.clj, but may incorporate test code from spiral26profiling.clj. 
;; Purpose is to compare speed of env-single/new-make-look-fn with alternatives.
(ns forage.experiment.spiral27profiling
  (:require [criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
            ;[clojure.math :as cmath]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.findfood :as ff]
            [forage.core.walks :as w]
            [forage.core.env-single :as envsingle]
            [forage.core.env-minimal :as envminimal]
            [forage.core.env-mason :as envmason]
            [utils.math :as um]
            [utils.random :as r]
            [utils.spiral :as sp]
            [utils.csv :as csv])
    (:import [clojure.lang IFn$DDO]))


(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(def targets-per-env 1)

(def homedir (System/getenv "HOME"))
(def default-dirname (str homedir "/docs/src/data.foraging/forage/spiral27/"))

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
             :perc-radius         1.0  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1.0
             :env-size            (* 2 half-size)
             :env-discretization  5 ; for Continuous2D; see foodspot.clj
             :init-loc-fn         (constantly [half-size half-size])
             :init-pad            nil ; if truthy, initial loc offset by this in rand dir
             :maxpathlen          maxpathlen
             :trunclen            trunclen
             :basename            (str default-dirname "spiral27_")
             :look-eps            0.2 ; shouldn't be used
             :foodspot-coords-fn  envsingle/foodspot-coords
             :rpt-to-stdout?       false ; write-experiments writes to stdout only if true
             :save-to-files?       true ; write-experiments saves summary data to files only if true
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
(defn make-single-target-envsingle
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
    ;(prn target) ; DEBUG
    (envsingle/make-env (target 0) (target 1))))

;; A bit more complicated than needed because it's a modified version
;; of make-multiple-target-env from spiral2[34]*.clj.
(defn make-single-target-envminimal
  "Make an env with a num-targets multiple foodspots at the same
  distance--the proportion denom/nomin of half-size from center of env,
  equally spaced in radians."
  [denom nomin]
  ;; coerce to doubles to avoid probs later with Ratio, BigInt:
  (let [half-size (double (/ (params :env-size) 2)) ; probably already defined as var, but should get from params
        distance-from-zero (* (/ nomin denom) half-size)
        zero-tgt [distance-from-zero 0] ; east from origin--will be shifted later
        directions [0]
        targets [(shift-point half-size half-size zero-tgt)]]
    ;(prn targets) ; DEBUG
    (envminimal/make-env targets)))

(defn make-single-target-envmason
  "Make an env with a num-targets multiple foodspots at the same
  distance--the proportion denom/nomin of half-size from center of env,
  equally spaced in radians."
  [denom nomin]
  ;; coerce to doubles to avoid probs later with Ratio, BigInt:
  (let [half-size (double (/ (params :env-size) 2)) ; probably already defined as var, but should get from params
        distance-from-zero (* (/ nomin denom) half-size)
        zero-tgt [distance-from-zero 0] ; east from origin--will be shifted later
        directions [0]
        targets [(shift-point half-size half-size zero-tgt)]]
    ;(prn targets) ; DEBUG
    (envmason/make-env (params :env-discretization)
                       (params :env-size)
                       targets)))


;; Make envs each with a single target but at several different distances
;; from center as proportion of size of env:
(def envsingles (mapv (partial make-single-target-envsingle 5) (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border
(def envminimals (mapv (partial make-single-target-envminimal 5) (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border
(def envmasons (mapv (partial make-single-target-envmason 5) (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border

(defn make-unbounded-envsingle-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (envsingle/make-look-fn env (params :perc-radius)))

(defn make-unbounded-envsingle-new-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (envsingle/new-make-look-fn env (params :perc-radius)))

(defn make-unbounded-envminimal-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (envminimal/make-look-fn env (params :perc-radius)))


#_
(defn make-unbounded-envmason-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  ^IFn$DDO [env]
  (partial envmason/perc-foodspots-exactly env (params :perc-radius)))

(defn make-unbounded-envmason-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (let [^double perc-radius (params :perc-radius)]
    ^IFn$DDO (fn [^double x ^double y]
               (envmason/perc-foodspots-exactly env perc-radius x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS

;(def seed (r/make-seed))
;(def seed -7370724773351240133)
(def seed 550554657766126322)
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
;; Functions that construct composite (and other) walks

(defn mu1-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen (more-mu1-vecs)))

(defn mu2-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen (more-mu2-vecs)))

(defn mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen (more-mu3-vecs)))

; ;; composite mu=1.1 and mu=3 walk
; (defn composite-mu1-mu3-vecs
;   [maxpathlen]
;   (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
;                    (apply concat
;                           (interleave (repeatedly more-mu1-vecs)
;                                       (repeatedly more-mu3-vecs)))))
; 
; ;; composite mu=1.5 and mu=3 walk
; (defn composite-mu15-mu3-vecs
;   [maxpathlen]
;   (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
;                    (apply concat
;                           (interleave (repeatedly more-mu15-vecs)
;                                       (repeatedly more-mu3-vecs)))))
; 
; ;; composite mu=1.1 and spiral walk
; (defn composite-mu1-spiral-vecs
;   [maxpathlen]
;   (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
;                    (apply concat
;                           (interleave (repeatedly more-mu1-vecs)
;                                       (repeatedly more-spiral-vecs)))))
; 
; ;; composite mu=1.5 and spiral walk
; (defn composite-mu15-spiral-vecs
;   [maxpathlen]
;   (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
;                    (apply concat
;                           (interleave (repeatedly more-mu15-vecs)
;                                       (repeatedly more-spiral-vecs)))))
; 
; ;; Doesn't use more-mu2-vecs because that is limited to
; ;; examine-segment-len, total.  This extends the walk to maxpathlen.
; (defn mu2-vecs
;   [maxpathlen]
;   (w/vecs-upto-len maxpathlen (w/make-levy-vecs rng mu2dist  1 (params :trunclen))))
; 
; (def first-mu2-vecs (mu2-vecs (params :maxpathlen)))
; 
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; Maps whose values are functions that run composite and non-composite 
; ;; walks in each of the different environments defined above.
; 
; ;; NOTE Only using the first four envs (i.e. the first four target distances).
; 
; (def fixed-mu2-walk-fns
;   {["mu2" "env0"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (w/walk-stops init-loc first-mu2-vecs)))
;    ["mu2" "env1"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (w/walk-stops init-loc first-mu2-vecs)))
;    ["mu2" "env2"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (w/walk-stops init-loc first-mu2-vecs)))
;    ["mu2" "env3"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (w/walk-stops init-loc first-mu2-vecs)))})
; 
; (def new-mu2-walk-fns
;   {["mu2" "env0"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
;    ["mu2" "env1"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
;    ["mu2" "env2"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
;    ["mu2" "env3"] (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))})
; 
; 
; (defn straight-path [init-loc] [init-loc [(params :maxpathlen) (init-loc 1)]])
; 
; (def straight-walk-fns
;   {["straight" "env0"] (fn [init-loc]
;                      ;(print "\nenv:" (envsingles 0) (straight-path init-loc)) ; DEBUG
;                      (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (straight-path init-loc)))
;    ["straight" "env1"] (fn [init-loc]
;                      ;(print "\nenv:" (envsingles 1) (straight-path init-loc)) ; DEBUG
;                      (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (straight-path init-loc)))
;    ["straight" "env2"] (fn [init-loc]
;                      ;(print "\nenv:" (envsingles 2) (straight-path init-loc)) ; DEBUG
;                      (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (straight-path init-loc)))
;    ["straight" "env3"] (fn [init-loc] 
;                      ;(print "\nenv:" (envsingles 3) (straight-path init-loc)) ; DEBUG
;                      (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (straight-path init-loc)))
;    ["straight" "env4"] (fn [init-loc]
;                      ;(print "\nenv:" (envsingles 4) (straight-path init-loc)) ; DEBUG
;                      (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 4)) "IGNORED" (straight-path init-loc)))})
; 
; 
; 
; ;; FIXME
; ;; composite mu=1.1 and mu=3
; (def mu1-mu3-walk-fns
;   {["composite-mu1-mu3" "env0"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
;    ["composite-mu1-mu3" "env1"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
;    ["composite-mu1-mu3" "env2"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))
;    ["composite-mu1-mu3" "env3"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-mu3-vecs (params :maxpathlen)))))})
; 
; ;; FIXME
; ;; composite mu=1.5 and mu=3
; (def mu15-mu3-walk-fns
;   {["composite-mu15-mu3" "env0"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
;    ["composite-mu15-mu3" "env1"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
;    ["composite-mu15-mu3" "env2"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))
;    ["composite-mu15-mu3" "env3"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-mu3-vecs (params :maxpathlen)))))})
; 
; ;; FIXME
; ;; composite mu=1.1 and spiral
; (def mu1-spiral-walk-fns
;   {["composite-mu1-spiral" "env0"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
;    ["composite-mu1-spiral" "env1"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
;    ["composite-mu1-spiral" "env2"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))
;    ["composite-mu1-spiral" "env3"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu1-spiral-vecs (params :maxpathlen)))))})
; 
; ;; FIXME
; ;; composite mu=1.5 and spiral
; (def mu15-spiral-walk-fns
;   {["composite-mu15-spiral" "env0"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 0)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
;    ["composite-mu15-spiral" "env1"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 1)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
;    ["composite-mu15-spiral" "env2"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 2)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))
;    ["composite-mu15-spiral" "env3"] (fn [init-loc] (ff/foodwalk (make-unbounded-envsingle-look-fn (envsingles 3)) (params :look-eps) (w/walk-stops init-loc (composite-mu15-spiral-vecs (params :maxpathlen)))))})

;; pure mu=1.5 walks (using my older interface)
;; FIXME
; (def mu15-walk-fns
;   {["mu15" "env0"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 0)) nil params 1.5)
;    ["mu15" "env1"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 1)) nil params 1.5)
;    ["mu15" "env2"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 2)) nil params 1.5)
;    ["mu15" "env3"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 3)) nil params 1.5)})
; 
; ;; pure mu=2 walks (using my older interface)
; (def mu2-walk-fns
;   {["mu2" "env0"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 0)) nil params 2.0)
;    ["mu2" "env1"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 1)) nil params 2.0)
;    ["mu2" "env2"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 2)) nil params 2.0)
;    ["mu2" "env3"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 3)) nil params 2.0)})
; 
; ;; pure mu=2.5 walks (using my older interface)
;; FIXME
; (def mu25-walk-fns
;   {["mu25" "env0"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 0)) nil params 2.5)
;    ["mu25" "env1"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 1)) nil params 2.5)
;    ["mu25" "env2"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 2)) nil params 2.5)
;    ["mu25" "env3"] (partial fr/levy-run rng (make-unbounded-envsingle-look-fn (envsingles 3)) nil params 2.5)})



(comment
  ;; TESTS

  (def straight-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "straight")) straight-walk-fns 10000 seed)))

  ;; CHOOSE ONE OF THESE:
  (defmacro mybench [expr] `(crit/quick-bench ~expr))
  (defmacro mybench [expr] `(crit/bench ~expr))
  ;; consider wrapping criterium calls in one or more of these:
  ; (binding [crit/*report-progress* true]
  ;           crit/*report-debug* true
  ;           crit/*report-warn* true
  ;           crit/*warmup-jit-period* n]
  ;   (crit/bench (blah blah)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; VERSIONS IN WHICH GENERATION OF WALKS IS NOT INCLUDED IN THE TIME
  ;; Note that in this case a *different* walk is used in each env
  ;; HOWEVER, the same series of walks is used in each env type, i.e.
  ;; env-single, env-minimal, env-mason.
  ;; An alternative is to use the same walk in each env.
  ;; THIS ONLY WORKS WITH FEW WALKS.  OTHERWISE THE MEMORY OVERHEAD IS TOO MUCH.
  ;; However, there is no need to reload or reset the PRNG once the walks
  ;; are stored.

  ;; If I'm done with the other walks, don't clutter up the heap:
  (ns-unmap *ns* 'mu2walks)
  (ns-unmap *ns* 'mu3walks)

  ;;;;;;;;;;;;;
  ;;; MU=1

  ;;; using crit/bench:
  ;;; env-single/new-make-look-fn: 4.255 ms
  ;;; env-single/make-look-fn:     4.640 ms

  (do ;; SETUP mu=1 walks
      ;; These are each different because the RNG advances.
      (def mu1walks [(w/walk-stops [half-size half-size] (mu1-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu1-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu1-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu1-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu1-vecs (params :maxpathlen)))])
      (def walks-per-fn 1))

  ;; ENV-SINGLE USING NEW NOV 2023 MAKE-WALK-FN:
  (let [new-env-single-walk-fns {"mu1-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 0)) "IGNORED" (mu1walks 0)))
                                 "mu1-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 1)) "IGNORED" (mu1walks 1)))
                                 "mu1-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 2)) "IGNORED" (mu1walks 2)))
                                 "mu1-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 3)) "IGNORED" (mu1walks 3)))
                                 "mu1-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 4)) "IGNORED" (mu1walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_single_NEW_mu1_1each")) new-env-single-walk-fns walks-per-fn seed))))

  ;; ENV-SINGLE USING ORIGINAL MAKE-WALK-FN:
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-single-walk-fns {"mu1-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (mu1walks 0)))
                             "mu1-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (mu1walks 1)))
                             "mu1-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (mu1walks 2)))
                             "mu1-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (mu1walks 3)))
                             "mu1-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 4)) "IGNORED" (mu1walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_single_mu1_1each")) env-single-walk-fns walks-per-fn seed))))

  ;; env-minimal
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envminimal/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-minimal-walk-fns {"mu1-env0" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 0)) "IGNORED" (mu1walks 0)))
                              "mu1-env1" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 1)) "IGNORED" (mu1walks 1)))
                              "mu1-env2" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 2)) "IGNORED" (mu1walks 2)))
                              "mu1-env3" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 3)) "IGNORED" (mu1walks 3)))
                              "mu1-env4" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 4)) "IGNORED" (mu1walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_minimal_mu1_1each")) env-minimal-walk-fns walks-per-fn seed))))

  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-mason-walk-fns {"mu1-env0" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 0)) (params :look-eps) (mu1walks 0)))
                            "mu1-env1" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 1)) (params :look-eps) (mu1walks 1)))
                            "mu1-env2" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 2)) (params :look-eps) (mu1walks 2)))
                            "mu1-env3" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 3)) (params :look-eps) (mu1walks 3)))
                            "mu1-env4" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 4)) (params :look-eps) (mu1walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_mason_mu1_1each")) env-mason-walk-fns walks-per-fn seed))))


  ;;;;;;;;;;;;;
  ;;; MU=2
  ;;; For mu=2, according to quick-bench, the fastest search methods, in order are:
  ;;; env-single with new-make-look-fn
  ;;; env-single with old make-look-fn (but the difference from preceding isn't as great as with mu=3)
  ;;; env-minimal (a LOT slower)
  ;;; env-mason (a VERY lot slower)
  ;;; [That last point is due to the fact that there's only one target.
  ;;; With six targets, env-minimal and env-mason almost the same, with env-minimal
  ;;; being slightly faster.]
  ;;;
  ;;; BUT BUT BUT:
  ;;; Full criterium/bench is reporting that the new-make-look-fn and old
  ;;; make-look-fn have identical performance!
  ;;;   new env-single 7.13899 ms
  ;;;   old env-single 7.14275 ms
  ;;;   env-minimal   11.40635 ms
  ;;;   env-mason     29.41337 ms
  ;;; 
  ;;; (So env-single is 4X faster than env-mason.
  ;;;  So env-mason is still faster per target with six targets.)

  ;; If I'm done with the other walks, don't clutter up the heap:
  (ns-unmap *ns* 'mu1walks)
  (ns-unmap *ns* 'mu3walks)

  (do ;; SETUP mu=2 walks
      ;; These are each different because the RNG advances.
      (def mu2walks [(w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))])
      (def walks-per-fn 1))

  ;; ENV-SINGLE USING NEW NOV 2023 MAKE-WALK-FN:
  (let [new-env-single-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 0)) "IGNORED" (mu2walks 0)))
                                 "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 1)) "IGNORED" (mu2walks 1)))
                                 "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 2)) "IGNORED" (mu2walks 2)))
                                 "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 3)) "IGNORED" (mu2walks 3)))
                                 "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 4)) "IGNORED" (mu2walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_single_NEW_mu2_1each")) new-env-single-walk-fns walks-per-fn seed))))

  ;; ENV-SINGLE USING ORIGINAL MAKE-WALK-FN:
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-single-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (mu2walks 0)))
                             "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (mu2walks 1)))
                             "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (mu2walks 2)))
                             "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (mu2walks 3)))
                             "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 4)) "IGNORED" (mu2walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_single_mu2_1each")) env-single-walk-fns walks-per-fn seed))))

  ;; env-minimal
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envminimal/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-minimal-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 0)) "IGNORED" (mu2walks 0)))
                              "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 1)) "IGNORED" (mu2walks 1)))
                              "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 2)) "IGNORED" (mu2walks 2)))
                              "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 3)) "IGNORED" (mu2walks 3)))
                              "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 4)) "IGNORED" (mu2walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_minimal_mu2_1each")) env-minimal-walk-fns walks-per-fn seed))))

  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-mason-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 0)) (params :look-eps) (mu2walks 0)))
                            "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 1)) (params :look-eps) (mu2walks 1)))
                            "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 2)) (params :look-eps) (mu2walks 2)))
                            "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 3)) (params :look-eps) (mu2walks 3)))
                            "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 4)) (params :look-eps) (mu2walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_mason_mu2_1each")) env-mason-walk-fns walks-per-fn seed))))


  ;;;;;;;;;;;;;
  ;;; MU=3

  ;;; For mu=3, according to quick-bench, the fastest search methods, in order are:
  ;;; env-single with new-make-look-fn
  ;;; env-single with old make-look-fn
  ;;; env-minimal
  ;;; env-mason

  ;; If I'm done with the other walks, don't clutter up the heap:
  (ns-unmap *ns* 'mu1walks)
  (ns-unmap *ns* 'mu2walks)

  (do ;; SETUP mu=3 walks
      ;; These are each different because the RNG advances.
      (def mu3walks [(w/walk-stops [half-size half-size] (mu3-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu3-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu3-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu3-vecs (params :maxpathlen)))
                     (w/walk-stops [half-size half-size] (mu3-vecs (params :maxpathlen)))])
      (def walks-per-fn 1))

  ;; ENV-SINGLE USING NEW NOV 2023 MAKE-WALK-FN:
  (let [new-env-single-walk-fns {"mu3-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 0)) "IGNORED" (mu3walks 0)))
                                 "mu3-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 1)) "IGNORED" (mu3walks 1)))
                                 "mu3-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 2)) "IGNORED" (mu3walks 2)))
                                 "mu3-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 3)) "IGNORED" (mu3walks 3)))
                                 "mu3-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-new-look-fn (envsingles 4)) "IGNORED" (mu3walks 4)))}]
    (time 
     (binding [crit/*report-progress* true
               ;crit/*report-debug* true
               ;crit/*report-warn* true
               ;crit/*warmup-jit-period* 50000000000 ; doesn't work
               crit/*default-benchmark-opts* (assoc crit/*default-benchmark-opts* :warmup-jit-period (* 2 crit/*warmup-jit-period*)) ; does work
              ]
       (mybench (fr/walk-experiments (update params :basename #(str % "env_single_NEW_mu3_1each")) new-env-single-walk-fns walks-per-fn seed))
   )))

  ;; ENV-SINGLE USING ORIGINAL MAKE-WALK-FN:
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-single-walk-fns {"mu3-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (mu3walks 0)))
                             "mu3-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (mu3walks 1)))
                             "mu3-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (mu3walks 2)))
                             "mu3-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (mu3walks 3)))
                             "mu3-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 4)) "IGNORED" (mu3walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_single_mu3_1each")) env-single-walk-fns walks-per-fn seed))))

  ;; ENV-MINIMAL
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envminimal/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-minimal-walk-fns {"mu3-env0" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 0)) "IGNORED" (mu3walks 0)))
                              "mu3-env1" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 1)) "IGNORED" (mu3walks 1)))
                              "mu3-env2" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 2)) "IGNORED" (mu3walks 2)))
                              "mu3-env3" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 3)) "IGNORED" (mu3walks 3)))
                              "mu3-env4" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 4)) "IGNORED" (mu3walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_minimal_mu3_1each")) env-minimal-walk-fns walks-per-fn seed))))

  ;; ENV-MASON
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [env-mason-walk-fns {"mu3-env0" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 0)) (params :look-eps) (mu3walks 0)))
                            "mu3-env1" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 1)) (params :look-eps) (mu3walks 1)))
                            "mu3-env2" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 2)) (params :look-eps) (mu3walks 2)))
                            "mu3-env3" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 3)) (params :look-eps) (mu3walks 3)))
                            "mu3-env4" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 4)) (params :look-eps) (mu3walks 4)))}]
    (time (mybench (fr/walk-experiments (update params :basename #(str % "env_mason_mu3_1each")) env-mason-walk-fns walks-per-fn seed))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ATTEMPT TO PRE-COMPUTE A LARGE NUMBER OF DIFFERENT WALKS
  ;; MAY BE VERY SLOW.


  (def walks-per-fn 2)

  ;; Note this puts the walks in reverse order:
  (def walks$ (atom (into () (repeatedly (* 5 walks-per-fn) #(w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))))))

   ;; Returns popped walks in list order (i.e. reverse of repeatedly):
  (defn next-walk!
   "ws$ is an atom containing a list of walks. (It must be a list.)  This
   function pops off the first walk and returns it. The list in the atom
   will contain the rest of the walks.  If the list becomes empty, the next
   call to this function will generate an error."
   [ws$]
   (print ".")
   (ffirst (swap-vals! ws$ pop)))

  (next-walk! walks$)

;; env-single
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (next-walk! walks$)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (next-walk! walks$)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (next-walk! walks$)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (next-walk! walks$)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 4)) "IGNORED" (next-walk! walks$)))}]
    (crit/quick-bench
      (fr/walk-experiments (update params :basename #(str % "env_single_mu2_1each")) new-mu2-walk-fns walks-per-fn seed)))

  ;; env-minimal
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envminimal/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 0)) "IGNORED" (next-walk! walks$)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 1)) "IGNORED" (next-walk! walks$)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 2)) "IGNORED" (next-walk! walks$)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 3)) "IGNORED" (next-walk! walks$)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 4)) "IGNORED" (next-walk! walks$)))}]
    (crit/quick-bench
      (fr/walk-experiments (update params :basename #(str % "env_minimal_mu2_1each")) new-mu2-walk-fns walks-per-fn seed)))

  ;; TODO NOTE I SHOULD REPLACE walks/find-in-seg WITH NUERNBER'S VERSION.
  ;; THAT SHOULD SPEED THIS UP A LITTLE.
  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 0)) (params :look-eps) (next-walk! walks$)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 1)) (params :look-eps) (next-walk! walks$)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 2)) (params :look-eps) (next-walk! walks$)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 3)) (params :look-eps) (next-walk! walks$)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 4)) (params :look-eps) (next-walk! walks$)))}]
    (crit/quick-bench
      (fr/walk-experiments (update params :basename #(str % "env_mason_mu2_1each")) new-mu2-walk-fns walks-per-fn seed)))




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; VERSIONS IN WHICH GENERATION OF WALKS IS INCLUDED IN THE TIME
  ;; Note that in this case a *different* walk is used in each env
  ;; because mu2-vecs ultimately calls make-levy-vecs with an 
  ;; updated rng state. HOWEVER, the same series of walks is
  ;; (supposed to be at least) used in each env type, i.e.
  ;; env-single, env-minimal, env-mason.

  ;; env-single
  (do
    (r/set-state rng initial-state)
    (let [new-mu2-walk-fns
         {"mu2-env0" (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (ff/foodwalk envsingle/find-in-seg (make-unbounded-envsingle-look-fn (envsingles 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))

  ;; env-minimal
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envminimal/foodspot-coords)
  (do
    (r/set-state rng initial-state)
    (let [new-mu2-walk-fns
         {"mu2-env0" (fn [init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (ff/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (envminimals 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))

  (clojure.repl/pst)

  ;; TODO NOTE I SHOULD REPLACE walks/find-in-seg WITH NUERNBER'S VERSION.
  ;; THAT SHOULD SPEED THIS UP A LITTLE.
  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  (do
    (r/set-state rng initial-state)
    (let[new-mu2-walk-fns
         {"mu2-env0" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 1)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 2)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-unbounded-envmason-look-fn (envmasons 3)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))

  (def fixed-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) fixed-mu2-walk-fns 10 seed)))


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
