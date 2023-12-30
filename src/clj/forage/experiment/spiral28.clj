;; Experiments comparing composite random and random+spiral walks.
(ns forage.experiment.spiral28
  (:require [criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
            ;[clojure.math :as cmath]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.findfood :as ff]
            [forage.core.walks :as w]
            [forage.core.env-minimal :as envmin]  ; seems to be a little faster
            ;[forage.core.env-mason :as envmas]     ;  than env-mason with six-target envs
            [utils.misc :as misc]
            [utils.math :as um]
            [utils.random :as r]
            [utils.spiral :as sp]
            [utils.csv :as csv])
    ;(:import [clojure.lang IFn$DDO])
    )

;; Based on spiral26profiling, which was initially copied from spiral25profiling.clj, 
;; which features comparisons of env-single, env-minimal, and env-mason on 
;; single-target envs. This is similar, but uses six-target envs, as used in spiral24* and
;; previous "spiral" namespaces.  Thus env-single is not used below.

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(def targets-per-env 6)

(def homedir (System/getenv "HOME"))
(def default-dirname (str homedir "/docs/src/data.foraging/forage/spiral28/"))
(def basename (str default-dirname "spiral28_"))

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
             :basename            basename
             :perc-radius         1.0  ; distance that an animal can "see" in searching for food
             :powerlaw-min        1.0  ; parameter to powerlaw random distributions
             :env-size            (* 2 half-size)
             :init-loc-fn         (constantly [half-size half-size])
             :init-pad            nil ; if truthy, initial loc offset by this in a random direction
             :maxpathlen          maxpathlen
             :examine-segment-len examine-segment-len
             :explore-segment-len explore-segment-len
             :trunclen            trunclen
             ;:env-discretization  5 ; only need for env-mason (for Continuous2D--see foodspot.clj)
             ;:look-eps            0.2 ; only needed for env-mason, not env-minimal
             :foodspot-coords-fn  envmin/foodspot-coords
             :rpt-to-stdout?       true ; write-experiments writes to stdout only if true
             :save-to-files?       true ; write-experiments saves summary data to files only if true
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE SOME ENVIRONMENTS
;; Each environment has six foodspots at the vertices of an equilateral hexagon.
;; This approximates searching for a single foodspot, but is more efficient
;; since one walk has a chance to find any one of the six, but the
;; probability that such a walk would have found another one is very low.

;; coordinates of targets/foodspots in different environments
(def target-coords (mapv (partial f/radial-target-coords (params :env-size) 
                           targets-per-env 5)
                  (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border

;; Make the environments, and store them in a vecdtor.
;; There are 5 envs, with indexes 0 through 4.
;; (env-minimal seems to be a little more efficient than env-mason for six
;; foodspots.  With a significantly larger number of foodspots, env-mason
;; is probably more efficient.)
(def envs (mapv envmin/make-env target-coords))
;(def mason-envs (mapv (partial envmas/make-env (params :env-discretization) (params :env-size)) target-coords))

(comment
  ;; Visually check that envs are as intended
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def minimal-env-plots (mapv (fn [env] (h/vega-food-plot 
                                           (map h/make-foodspot (envmin/env-foodspot-coords env))
                                           (params :env-size)
                                           600
                                           200))
                               envs))

  (oz/view! (minimal-env-plots 0)) ; there are five envs, with indexes 0 through 4.
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE THE LOOK FNS AND FOODWALK FN

(defn make-envmin-look-fn
  "Make a non-toroidal look-fn from env.  (Searches that leave the core env
  will just continue without success unless they wander back.)"
  [env]
  (envmin/make-look-fn env (params :perc-radius)))

;(defn make-envmas-look-fn
;  "Make a non-toroidal look-fn from env.  (Searches that leave the core env
;  will just continue without success unless they wander back.)"
;  [env]
;  (let [^double perc-radius (params :perc-radius)]
;    ^IFn$DDO (fn [^double x ^double y]
;               (envmas/perc-foodspots-exactly env perc-radius x y))))

(def look-fns 
  "A vector of non-toroidal look-fns, one for each env from env.  (Searches
  that leave the core env will just continue without success unless they
  wander back.)"
  (mapv make-envmin-look-fn envs))

(defn make-walk-fn
  "Create a walk function of an initial location. This function can be
  passed to run/walk-experiments and then run/run-and-collect.  The
  resulting function calls findfood/foodwalk with envmin/find-in-seg
  and look-fn, and constructs a walk from the mathematical vectors in
  the finite sequence vecs."
  [look-fn vecs]
  (fn [init-loc]
    (ff/foodwalk envmin/find-in-seg
                 look-fn
                 :ignored-eps
                 (w/walk-stops init-loc vecs))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE THE PRNG
(def seed (r/make-seed))
;(def seed -1645093054649086646)
(println "Using seed" seed)
(def rng (r/make-mrg32k3a seed))
;(def rng (r/make-well19937 seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE COMPONENT WALKS

;; ------------------------------------------------
;; PROBABILITY DISTRIBUTIONS FOR CONSTRUCTING RANDOM WALKS
;; The last parameter is the Lévy mu value:
(def mu1dist (r/make-mrg32k3a-powerlaw rng 1 1.1))
(def mu15dist (r/make-mrg32k3a-powerlaw rng 1 1.5))
(def mu2dist (r/make-mrg32k3a-powerlaw rng 1 2))
(def mu25dist (r/make-mrg32k3a-powerlaw rng 1 2.5))
(def mu3dist (r/make-mrg32k3a-powerlaw rng 1 3))
;; I may use mu=other values as well below, but only using my older levy-experiments interface

;; ------------------------------------------------
;; THE (INFINITE) SPIRAL WALK
;; No need to regenerate this--it should be the same every time.
(def spiral (sp/unit-archimedean-spiral-vecs 2 0.1)) 
(def spiral-max (w/vecs-upto-len (params :maxpathlen) spiral))
(def spiral-examine (w/vecs-upto-len (params :examine-segment-len) spiral))

;; NOTE the random walk functions below will return a different walk each
;; time they're called, and will advance the state of the PRNG.

;; ------------------------------------------------
;; STANDALONE (NON-COMPONENT) WALKS

(defn mu15-vecs
  "Returns a random walk with exponent mu=2 of length (params :maxpathlen)."
  [maxpathlen]
  (w/vecs-upto-len maxpathlen
                   (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))

(defn mu2-vecs
  "Returns a random walk with exponent mu=2 of length (params :maxpathlen)."
  [maxpathlen]
  (w/vecs-upto-len maxpathlen
                   (w/make-levy-vecs rng mu2dist 1 (params :trunclen))))

(defn mu25-vecs
  "Returns a random walk with exponent mu=2 of length (params :maxpathlen)."
  [maxpathlen]
  (w/vecs-upto-len maxpathlen
                   (w/make-levy-vecs rng mu25dist 1 (params :trunclen))))

(defn spiral-vecs
  "Returns a spiral walk of length (params :maxpathlen)."
  []
  spiral-max)

;; FINITE COMPONENT WALK FUNCTIONS
;; Note that these are functions, so the *random* walks generated will be 
;; different each time.  (There's no need to regenerate the spiral walks,
;; as they are the same except for length.)

;; ------------------------------------------------
;; LOCAL CLOSE EXAMINATION ("exploit") WALKS

(defn component-spiral-vecs
  "Returns a spiral walk of length (params :examine-segment-len)."
  []
  spiral-examine)

(defn component-mu3-vecs
  "Returns a random walk with exponent mu=3 of length (params :examine-segment-len)."
  []
  (w/vecs-upto-len (params :examine-segment-len) (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))

;; ------------------------------------------------
;; LONG-RANGE EXPLORATION RANDOM WALKS:

(defn component-mu1-vecs 
  "Returns a random walk with exponent mu=1.1 of length (params :explore-segment-len)."
  []
  (w/vecs-upto-len (params :explore-segment-len) (w/make-levy-vecs rng mu1dist 1 (params :trunclen))))

(defn component-mu15-vecs
  "Returns a random walk with exponent mu=1.5 of length (params :explore-segment-len)."
  []
  (w/vecs-upto-len (params :explore-segment-len) (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))

(defn component-mu2-vecs
  "Returns a random walk with exponent mu=2 of length (params :explore-segment-len)."
  []
  (w/vecs-upto-len (params :explore-segment-len) (w/make-levy-vecs rng mu2dist  1 (params :trunclen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS FOR CONSTRUCTING FINITE WALKS THAT COMBINE 
;; DIFFERENT KINDS OF WALKS

;; composite mu=1.1 and mu=3 walk
(defn composite-mu1-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu1-vecs)
                                      (repeatedly component-mu3-vecs)))))

;; composite mu=1.5 and mu=3 walk
(defn composite-mu15-mu3-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu15-vecs)
                                      (repeatedly component-mu3-vecs)))))

;; composite mu=1.1 and spiral walk
(defn composite-mu1-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu1-vecs)
                                      (repeatedly component-spiral-vecs)))))

;; composite mu=1.5 and spiral walk
(defn composite-mu15-spiral-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu15-vecs)
                                      (repeatedly component-spiral-vecs)))))




;; PURE SPIRAL WALKS
;; Separate these from other walks because they only need to run once each.
;; EXAMPLE USAGE:
;; (fr/walk-experiments params walk-fns 1 "spiral")
(def spiral-walk-fns
  {
   ["spiral" "env0"] 'fixme
   ["spiral" "env1"] 'fixme
   ["spiral" "env2"] 'fixme
   ["spiral" "env3"] 'fixme
   ["spiral" "env4"] 'fixme
   })

(comment
  ;; EXAMPLE USAGE of walk-fns below:
  (fr/walk-experiments params walk-fns-subset walks-per-fn label-seed)
  ;; or more specifically, e.g.:
  (def some-walks (select-keys walk-fns
                                     [["mu15" "env0"] ["mu15" "env1"] ["mu15" "env2"]
                                      ["mu15" "env3"] ["mu15" "env4"]]))
  ;; or to make sure they appear in order:
  (def some-walks (into (sorted-map)
                        (select-keys walk-fns
                                     [["mu2" "env0"] ["mu2" "env1"] ["mu2" "env2"]
                                      ["mu2" "env3"] ["mu2" "env4"]])))
  ;; (Due to Clojure's flexible map implementations, they'll probably be in
  ;; order if the result is small, but maybe not if it's too large.)

  (def walks-per-fn 2)
  ;; And then:
  (fr/walk-experiments params some-walks walks-per-fn seed)

  ;; tests
  (require '[tech.v3.dataset :as ds] '[tech.v3.dataset.print :as dsp])
  (def nippyname (str basename seed "data.nippy"))
  (def data (ds/->dataset nippyname))
  (ds/descriptive-stats data)

  (take 15 (mu15-vecs))
  (take 45 (w/walk-stops [half-size half-size] (mu15-vecs)))
)


;; FIXME I think that with this setup, rather than in the previous version,
;; I'm generating the random walks once during the evaluation of defs, and never again.
;; This is suggested by who long it's taking to eval the file.
(def walk-fns
  (let [maxpathlen (params :maxpathlen)]
    {;; PURE RANDOM WALKS:

     ["mu15" "env0"] (make-walk-fn (look-fns 0) (mu15-vecs maxpathlen))
     ["mu15" "env1"] (make-walk-fn (look-fns 1) (mu15-vecs maxpathlen))
     ["mu15" "env2"] (make-walk-fn (look-fns 2) (mu15-vecs maxpathlen))
     ["mu15" "env3"] (make-walk-fn (look-fns 3) (mu15-vecs maxpathlen))
     ["mu15" "env4"] (make-walk-fn (look-fns 4) (mu15-vecs maxpathlen))

     ["mu2"  "env0"] (make-walk-fn (look-fns 0) (mu2-vecs maxpathlen))
     ["mu2"  "env1"] (make-walk-fn (look-fns 1) (mu2-vecs maxpathlen))
     ["mu2"  "env2"] (make-walk-fn (look-fns 2) (mu2-vecs maxpathlen))
     ["mu2"  "env3"] (make-walk-fn (look-fns 3) (mu2-vecs maxpathlen))
     ["mu2"  "env4"] (make-walk-fn (look-fns 4) (mu2-vecs maxpathlen))

     ["mu25" "env0"] (make-walk-fn (look-fns 0) (mu25-vecs maxpathlen))
     ["mu25" "env1"] (make-walk-fn (look-fns 1) (mu25-vecs maxpathlen))
     ["mu25" "env2"] (make-walk-fn (look-fns 2) (mu25-vecs maxpathlen))
     ["mu25" "env3"] (make-walk-fn (look-fns 3) (mu25-vecs maxpathlen))
     ["mu25" "env4"] (make-walk-fn (look-fns 4) (mu25-vecs maxpathlen))

     ;; COMPOSITE RANDOM WALKS:

     ["mu1-mu3" "env0"] (make-walk-fn (look-fns 0) (composite-mu1-mu3-vecs maxpathlen))
     ["mu1-mu3" "env1"] (make-walk-fn (look-fns 1) (composite-mu1-mu3-vecs maxpathlen))
     ["mu1-mu3" "env2"] (make-walk-fn (look-fns 2) (composite-mu1-mu3-vecs maxpathlen))
     ["mu1-mu3" "env3"] (make-walk-fn (look-fns 3) (composite-mu1-mu3-vecs maxpathlen))
     ["mu1-mu3" "env4"] (make-walk-fn (look-fns 4) (composite-mu1-mu3-vecs maxpathlen))

     ["mu15-mu3" "env0"] (make-walk-fn (look-fns 0) (composite-mu15-mu3-vecs maxpathlen))
     ["mu15-mu3" "env1"] (make-walk-fn (look-fns 1) (composite-mu15-mu3-vecs maxpathlen))
     ["mu15-mu3" "env2"] (make-walk-fn (look-fns 2) (composite-mu15-mu3-vecs maxpathlen))
     ["mu15-mu3" "env3"] (make-walk-fn (look-fns 3) (composite-mu15-mu3-vecs maxpathlen))
     ["mu15-mu3" "env4"] (make-walk-fn (look-fns 4) (composite-mu15-mu3-vecs maxpathlen))

     ;; COMPOSITE RANDOM-SPIRAL WALKS:

     ["mu1-spiral" "env0"] (make-walk-fn (look-fns 0) (composite-mu1-spiral-vecs maxpathlen))
     ["mu1-spiral" "env1"] (make-walk-fn (look-fns 1) (composite-mu1-spiral-vecs maxpathlen))
     ["mu1-spiral" "env2"] (make-walk-fn (look-fns 2) (composite-mu1-spiral-vecs maxpathlen))
     ["mu1-spiral" "env3"] (make-walk-fn (look-fns 3) (composite-mu1-spiral-vecs maxpathlen))
     ["mu1-spiral" "env4"] (make-walk-fn (look-fns 4) (composite-mu1-spiral-vecs maxpathlen))

     ["mu15-spiral" "env0"] (make-walk-fn (look-fns 0) (composite-mu15-spiral-vecs maxpathlen))
     ["mu15-spiral" "env1"] (make-walk-fn (look-fns 1) (composite-mu15-spiral-vecs maxpathlen))
     ["mu15-spiral" "env2"] (make-walk-fn (look-fns 2) (composite-mu15-spiral-vecs maxpathlen))
     ["mu15-spiral" "env3"] (make-walk-fn (look-fns 3) (composite-mu15-spiral-vecs maxpathlen))
     ["mu15-spiral" "env4"] (make-walk-fn (look-fns 4) (composite-mu15-spiral-vecs maxpathlen))

    }))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TESTS

  (def straight-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "straight")) straight-walk-fns 10000 seed)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; VERSIONS IN WHICH GENERATION OF WALKS IS NOT INCLUDED IN THE TIME,
  ;; A *DIFFERENT* WALK IS USED IN EACH ENV.
  ;; HOWEVER, THE SAME SERIES OF WALKS IS USED IN EACH ENV TYPE, i.e.
  ;; env-single, env-minimal, env-mason.
  ;; (An alternative is to use the same walk in each env.)

  (do ;; SETUP

      ;; NOTE that mu2-vecs is defined above using the PRNG defined in the
      ;; main code in this file.  So the only way to reset and get exactly
      ;; the same walks is to re-evaluate the file.  At the moment I don't want to pass
      ;; in PRNG to all of the functions that the following depends on.
      ;(def seed -7370724773351240133)
      ;(def rng (r/make-well19937 seed))
      ;(def initial-state (r/get-state rng)) ; only used for some of the tests below

      ;; A different walk for each of the five environments.
      ;; These are each different because the RNG advances.
      (def walks [(w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                  (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                  (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                  (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))
                  (w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))])

      (println "# of coordinate pairs in walks 0 through 4, respectively:")
      (prn (map count walks))

      (def walks-per-fn 1)
  )

  ;; consider wrapping criterium calls in one or more of these:
  ;(binding [crit/*report-progress* true
  ;          crit/*report-debug* true
  ;          crit/*report-warn* true])

  ;; env-minimal
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmin/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 0)) "IGNORED" (walks 0)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 1)) "IGNORED" (walks 1)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 2)) "IGNORED" (walks 2)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 3)) "IGNORED" (walks 3)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 4)) "IGNORED" (walks 4)))}]
    (time (crit/quick-bench
            (fr/walk-experiments (update params
                                         :basename #(str % "env_minimal_mu2_1each")
                                         :foodspot-coords-fn  env-minimal/foodspot-coords)
                                 new-mu2-walk-fns walks-per-fn seed))))


  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmas/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 0)) (params :look-eps) (walks 0)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 1)) (params :look-eps) (walks 1)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 2)) (params :look-eps) (walks 2)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 3)) (params :look-eps) (walks 3)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 4)) (params :look-eps) (walks 4)))}]
    (time (crit/quick-bench
            (fr/walk-experiments (update params
                                         :basename #(str % "env_mason_mu2_1each")
                                         :foodspot-coords-fn  env-mason/foodspot-coords)
                                 new-mu2-walk-fns walks-per-fn seed))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ATTEMPT TO PRE-COMPUTE A LARGE NUMBER OF DIFFERENT WALKS
  ;; SETUP MAY BE VERY SLOW.

  ;; Create a different walk for each env in run.  So if the number of 5-env runs = walks-per-fn, 
  ;; every time there is a walk through an env, it will be different (unless you repeat the whole
  ;; process, e.g. during benchmarking.)

  (def walks-per-fn 10) ;; On MBP setting walks-per-fn to 5 to create 25 walks takes aobut 15 seconds.

  (defn make-walks 
    "Returns a lazy sequence of (5 X walks-per-fn) [fully realized] walks."
    [walks-per-fn]
    (repeatedly (* 5 walks-per-fn)
                #(w/walk-stops [half-size half-size] (mu2-vecs (params :maxpathlen)))))

  ;; This vector of walks can be reused in different tests below. (Should be a vector to index into it efficiently.)
  (def walks (time (vec (make-walks walks-per-fn))))

  ;; env-minimal
  (let [i$ (atom -1)
        next-walk! #(walks (swap! i$ inc))
        new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 0)) "IGNORED" (next-walk!)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 1)) "IGNORED" (next-walk!)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 2)) "IGNORED" (next-walk!)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 3)) "IGNORED" (next-walk!)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 4)) "IGNORED" (next-walk!)))}]
    (time
      ;(crit/quick-bench
      (def result-minimal
        (-> params
            (assoc :foodspot-coords-fn  envmin/foodspot-coords)
            (update :basename #(str % "env_minimal_mu2_" walks-per-fn "each"))
            (fr/walk-experiments new-mu2-walk-fns walks-per-fn seed))
      )
    )
  )


  ;; env-mason
  (let [i$ (atom -1)
        next-walk! #(walks (swap! i$ inc))
        new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 0)) (params :look-eps) (next-walk!)))
                          "mu2-env1" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 1)) (params :look-eps) (next-walk!)))
                          "mu2-env2" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 2)) (params :look-eps) (next-walk!)))
                          "mu2-env3" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 3)) (params :look-eps) (next-walk!)))
                          "mu2-env4" (fn [ignored-init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 4)) (params :look-eps) (next-walk!)))}]
    (time
      ;(crit/quick-bench
      (def result-mason
        (-> params
            (assoc :foodspot-coords-fn  envmas/foodspot-coords)
            (update :basename #(str % "env_mason_mu2_" walks-per-fn "each"))
            (fr/walk-experiments new-mu2-walk-fns walks-per-fn seed))
      )
    )
  )


  (= result-mason result-minimal)
  ;; With seed = -8260641786968384627, and 10 runs per env, there is one
  ;; target found, in path 6 in env0.  The path length until finding the
  ;; foodspot is not identical in the two env types, but it's close:
  ;;    env-mason: 120904.28057527842
  ;;    env-minimal: 120905.21856515628
  ;; Maybe that makes sense given that env-mason is checking very 0.2
  ;; steps, and then checking from the first endpoint of the little
  ;; subsegment (unless the angle of the segment is such that the endpoints
  ;; are swapped).  While env-minimal should be finding the precise closest
  ;; point to the total step segment.  Note though that even that is an
  ;; approximation, because in practice, the animal should be able to see
  ;; the foodspot a little bit before that closest point.  This is
  ;; especially true with larger perceptual raddi.  So despite the
  ;; precision of env-minimal (and env-single), they don't give one
  ;; the correct finding location.  For large perc-radius, env-mason
  ;; should be closer to correct.



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; VERSIONS IN WHICH GENERATION OF WALKS IS INCLUDED IN THE TIME
  ;; Note that in this case a *different* walk is used in each env
  ;; because mu2-vecs ultimately calls make-levy-vecs with an 
  ;; updated rng state. HOWEVER, the same series of walks is
  ;; (supposed to be at least) used in each env type, i.e.
  ;; env-minimal, env-mason.
 
  (def walks-per-fn 20)

  ;; NOTE It's not enough to reset the PRNG to a known state here.  You
  ;; have to revaluate this whole file WITH THE SAME SEED; I think parts of 
  ;; the code in the file uses random numbers in code outside of comments when first run.
  (let [new-mu2-walk-fns
        {"mu2-env0" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env1" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env2" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env3" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env4" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 4)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
    (time
      ;(crit/quick-bench
      (def result-minimal
        (-> params
            (assoc :foodspot-coords-fn  envmin/foodspot-coords)
            (update :basename #(str % "env_minimal_mu2_" walks-per-fn "each"))
            (fr/walk-experiments new-mu2-walk-fns walks-per-fn seed))
      )
    )
  )


  ;; env-mason
  (let[new-mu2-walk-fns
       {"mu2-env0" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env1" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 1)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env2" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 2)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env3" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 3)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env4" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 4)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
    (time
      ;(crit/quick-bench
      (def result-mason
        (-> params
            (assoc :foodspot-coords-fn  envmas/foodspot-coords)
            (update :basename #(str % "env_mason_mu2_" walks-per-fn "each"))
            (fr/walk-experiments new-mu2-walk-fns walks-per-fn seed))
      )
    )
  )


  ;; Quick test: did the two env types find the same foodspots?
  (= (result-mason :found-coords) (result-minimal :found-coords))
  (= (result-mason :data) (result-minimal :data))




)


(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; OLD/OBSOLETE
  
  (do
    (r/set-state rng initial-state)
    (let [new-mu2-walk-fns
         {"mu2-env0" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (ff/foodwalk envmin/find-in-seg (make-envmin-look-fn (envs 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))


  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmas/foodspot-coords)
  (do
    (r/set-state rng initial-state)
    (let[new-mu2-walk-fns
         {"mu2-env0" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 1)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 2)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (ff/foodwalk ff/find-in-seg (make-envmas-look-fn (mason-envs 3)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))

  (def fixed-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) fixed-mu2-walk-fns 10 seed)))

)
