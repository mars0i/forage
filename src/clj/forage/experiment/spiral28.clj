;; Experiments comparing composite random and random+spiral walks.
(ns forage.experiment.spiral28
  (:require ;[criterium.core :as crit]
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
;; SEE BELOW FOR COMMENTED CODE DISPLAYING THE ENVS.
(def envs (mapv envmin/make-env target-coords))
;(def mason-envs (mapv (partial envmas/make-env (params :env-discretization) (params :env-size)) target-coords))




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


;; NOTE We pass a mathematical vector generating *function* to make-foodwalk-fn, 
;; not a sequence of vectors.  The function will be called in the function
;; returned by by make-foodwalk-fn, and the resulting sequence of vectors will
;; be turned into stop coordinates by walks/walk-stops. That way, every time 
;; the walk fn is called in run/run-and-collect, it will have a new random 
;; sequence of stops.  If instead we evaluated the vector generating
;; function as it was passed to make-foodwalk-fn, or passed the sequence of
;; vectors itself, every call to the walk fn would use the same sequence
;; of walk stops.

(defn make-foodwalk-fn
  "Create a walk function expects an initial location--a coordinate pair--
  and returns a sequence of walk-stops . This function can be passed to
  run/walk-experiments and then run/run-and-collect.  The resulting
  function calls findfood/foodwalk with envmin/find-in-seg and look-fn, 
  uses a new walk constructed by walks/walk-stops from the
  finite sequence of
  mathematical vectors generated by the function vecs-gen.  (By passing a
  vector-generating function and calling it when needed, we avoid
  evaluating this function before it's passed to make-foodwalk-fn, which
  would result in multiple walks using the very same sequence.  An
  alternative, pregenerating sequences in advance, uses too much memory.)"
  [look-fn vecs-gen]
  (fn [init-loc]
    (ff/foodwalk envmin/find-in-seg
                 look-fn
                 :ignored-eps
                 (w/walk-stops init-loc (vecs-gen)))))




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
;; THE SPIRAL WALKS
;; No need to regenerate this sequence--it should be the same every time.

(def spiral (sp/unit-archimedean-spiral-vecs 2 0.1)) 
(def spiral-max (w/vecs-upto-len (params :maxpathlen) spiral))
(def spiral-examine (w/vecs-upto-len (params :examine-segment-len) spiral))

(defn spiral-vecs-gen
  "Returns the sequence of spiral vectors with length (params :maxpathlen)."
  []
  spiral-max)

(defn component-spiral-vecs-gen
  "Returns the sequence of spiral vectors with length (params :examine-segment-len)."
  []
  spiral-examine)

;; NOTE the random walk functions below will return a different walk each
;; time they're called, and will advance the state of the PRNG.

;; ------------------------------------------------
;; STANDALONE (NON-COMPONENT) WALKS


(defn mu15-vecs-gen
  "Returns a random walk with exponent mu=1.5 of length (params :maxpathlen)."
  []
  (w/vecs-upto-len (params :maxpathlen) (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))

(defn mu2-vecs-gen
  "Returns a random walk with exponent mu=2 of length (params :maxpathlen)."
  []
  (w/vecs-upto-len (params :maxpathlen) (w/make-levy-vecs rng mu2dist 1 (params :trunclen))))

(defn mu25-vecs-gen
  "Returns a random walk with exponent mu=2.5 of length (params :maxpathlen)."
  []
  (w/vecs-upto-len (params :maxpathlen) (w/make-levy-vecs rng mu25dist 1 (params :trunclen))))

(defn mu3-vecs-gen
  "Returns a random walk with exponent mu=3 of length (params :maxpathlen)."
  []
  (w/vecs-upto-len (params :maxpathlen) (w/make-levy-vecs rng mu3dist 1 (params :trunclen))))

;; FINITE COMPONENT WALK FUNCTIONS
;; Note that these are functions, so the *random* walks generated will be 
;; different each time.  (There's no need to regenerate the spiral walks,
;; as they are the same except for length.)

;; ------------------------------------------------
;; LOCAL CLOSE EXAMINATION ("exploit") WALKS

(defn component-mu3-vecs-gen
  "Returns a random walk with exponent mu=3 of length (params :examine-segment-len)."
  []
  (w/vecs-upto-len (params :examine-segment-len) (w/make-levy-vecs rng mu3dist  1 (params :trunclen))))

;; ------------------------------------------------
;; LONG-RANGE EXPLORATION RANDOM WALKS:

(defn component-mu1-vecs-gen 
  "Returns a random walk with exponent mu=1.1 of length (params :explore-segment-len)."
  []
  (w/vecs-upto-len (params :explore-segment-len) (w/make-levy-vecs rng mu1dist 1 (params :trunclen))))

(defn component-mu15-vecs-gen
  "Returns a random walk with exponent mu=1.5 of length (params :explore-segment-len)."
  []
  (w/vecs-upto-len (params :explore-segment-len) (w/make-levy-vecs rng mu15dist 1 (params :trunclen))))

(defn component-mu2-vecs-gen
  "Returns a random walk with exponent mu=2 of length (params :explore-segment-len)."
  []
  (w/vecs-upto-len (params :explore-segment-len) (w/make-levy-vecs rng mu2dist  1 (params :trunclen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS FOR CONSTRUCTING FINITE WALKS THAT COMBINE 
;; DIFFERENT KINDS OF WALKS

;; composite mu=1.1 and mu=3 walk
(defn composite-mu1-mu3-vecs-gen
  []
  (w/vecs-upto-len (params :maxpathlen) ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu1-vecs-gen)
                                      (repeatedly component-mu3-vecs-gen)))))

;; composite mu=1.5 and mu=3 walk
(defn composite-mu15-mu3-vecs-gen
  []
  (w/vecs-upto-len (params :maxpathlen) ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu15-vecs-gen)
                                      (repeatedly component-mu3-vecs-gen)))))

;; composite mu=1.1 and spiral walk
(defn composite-mu1-spiral-vecs-gen
  []
  (w/vecs-upto-len (params :maxpathlen) ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu1-vecs-gen)
                                      (repeatedly component-spiral-vecs-gen)))))

;; composite mu=1.5 and spiral walk
(defn composite-mu15-spiral-vecs-gen
  []
  (w/vecs-upto-len (params :maxpathlen) ; vecs-upto-len is eager, so whatever it takes will be realized
                   (apply concat
                          (interleave (repeatedly component-mu15-vecs-gen)
                                      (repeatedly component-spiral-vecs-gen)))))



;; PURE SPIRAL WALKS
;; Separate these from other walks because they only need to run once each.
;; EXAMPLE USAGE:
;; (fr/walk-experiments params walk-fns 1 "spiral")
(def spiral-walk-fns
  (sorted-map ["spiral" "env0"] (make-foodwalk-fn (look-fns 0) spiral-vecs-gen)
              ["spiral" "env1"] (make-foodwalk-fn (look-fns 1) spiral-vecs-gen)
              ["spiral" "env2"] (make-foodwalk-fn (look-fns 2) spiral-vecs-gen)
              ["spiral" "env3"] (make-foodwalk-fn (look-fns 3) spiral-vecs-gen)
              ["spiral" "env4"] (make-foodwalk-fn (look-fns 4) spiral-vecs-gen)))

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

  (def spiral-walk-result ((spiral-walk-fns ["spiral" "env0"]) ((params :init-loc-fn))))

  ;; FIXME NEXT LINE FAILS BECAUSE hanami fns get env size from env, which
  ;; is assumed to be an env-mason.  But my env-minimal envs *have no
  ;; sizes* at present.  Neither do env-single envs.
  (def plot (h/vega-didcould-envwalk-plot (envs 0) 600 1 100 spiral-walk-result))


)



;; NOTE We pass a mathematical vector generating *function* to make-foodwalk-fn, 
;; not a sequence of vectors.  The function will be called in the function
;; returned by by make-foodwalk-fn, and the resulting sequence of vectors will
;; be turned into stop coordinates by walks/walk-stops. That way, every time 
;; the walk fn is called in run/run-and-collect, it will have a new random 
;; sequence of stops.  If instead we evaluated the vector generating
;; function as it was passed to make-foodwalk-fn, or passed the sequence of
;; vectors itself, every call to the walk fn would use the same sequence
;; of walk stops.
(def random-walk-fns
  (sorted-map ; won't necess be the order below, but better than letting Clojure's default map structures decide.

    ;; PURE RANDOM WALKS:

    ["mu15" "env0"] (make-foodwalk-fn (look-fns 0) mu15-vecs-gen)
    ["mu15" "env1"] (make-foodwalk-fn (look-fns 1) mu15-vecs-gen)
    ["mu15" "env2"] (make-foodwalk-fn (look-fns 2) mu15-vecs-gen)
    ["mu15" "env3"] (make-foodwalk-fn (look-fns 3) mu15-vecs-gen)
    ["mu15" "env4"] (make-foodwalk-fn (look-fns 4) mu15-vecs-gen)

    ["mu2"  "env0"] (make-foodwalk-fn (look-fns 0) mu2-vecs-gen)
    ["mu2"  "env1"] (make-foodwalk-fn (look-fns 1) mu2-vecs-gen)
    ["mu2"  "env2"] (make-foodwalk-fn (look-fns 2) mu2-vecs-gen)
    ["mu2"  "env3"] (make-foodwalk-fn (look-fns 3) mu2-vecs-gen)
    ["mu2"  "env4"] (make-foodwalk-fn (look-fns 4) mu2-vecs-gen)

    ["mu25" "env0"] (make-foodwalk-fn (look-fns 0) mu25-vecs-gen)
    ["mu25" "env1"] (make-foodwalk-fn (look-fns 1) mu25-vecs-gen)
    ["mu25" "env2"] (make-foodwalk-fn (look-fns 2) mu25-vecs-gen)
    ["mu25" "env3"] (make-foodwalk-fn (look-fns 3) mu25-vecs-gen)
    ["mu25" "env4"] (make-foodwalk-fn (look-fns 4) mu25-vecs-gen)

    ["mu3"  "env0"] (make-foodwalk-fn (look-fns 0) mu3-vecs-gen)
    ["mu3"  "env1"] (make-foodwalk-fn (look-fns 1) mu3-vecs-gen)
    ["mu3"  "env2"] (make-foodwalk-fn (look-fns 2) mu3-vecs-gen)
    ["mu3"  "env3"] (make-foodwalk-fn (look-fns 3) mu3-vecs-gen)
    ["mu3"  "env4"] (make-foodwalk-fn (look-fns 4) mu3-vecs-gen)

    ;; COMPOSITE RANDOM-RANDOM WALKS:

    ["mu1-mu3" "env0"] (make-foodwalk-fn (look-fns 0) composite-mu1-mu3-vecs-gen)
    ["mu1-mu3" "env1"] (make-foodwalk-fn (look-fns 1) composite-mu1-mu3-vecs-gen)
    ["mu1-mu3" "env2"] (make-foodwalk-fn (look-fns 2) composite-mu1-mu3-vecs-gen)
    ["mu1-mu3" "env3"] (make-foodwalk-fn (look-fns 3) composite-mu1-mu3-vecs-gen)
    ["mu1-mu3" "env4"] (make-foodwalk-fn (look-fns 4) composite-mu1-mu3-vecs-gen)

    ["mu15-mu3" "env0"] (make-foodwalk-fn (look-fns 0) composite-mu15-mu3-vecs-gen)
    ["mu15-mu3" "env1"] (make-foodwalk-fn (look-fns 1) composite-mu15-mu3-vecs-gen)
    ["mu15-mu3" "env2"] (make-foodwalk-fn (look-fns 2) composite-mu15-mu3-vecs-gen)
    ["mu15-mu3" "env3"] (make-foodwalk-fn (look-fns 3) composite-mu15-mu3-vecs-gen)
    ["mu15-mu3" "env4"] (make-foodwalk-fn (look-fns 4) composite-mu15-mu3-vecs-gen)

    ;; COMPOSITE RANDOM-SPIRAL WALKS:

    ["mu1-spiral" "env0"] (make-foodwalk-fn (look-fns 0) composite-mu1-spiral-vecs-gen)
    ["mu1-spiral" "env1"] (make-foodwalk-fn (look-fns 1) composite-mu1-spiral-vecs-gen)
    ["mu1-spiral" "env2"] (make-foodwalk-fn (look-fns 2) composite-mu1-spiral-vecs-gen)
    ["mu1-spiral" "env3"] (make-foodwalk-fn (look-fns 3) composite-mu1-spiral-vecs-gen)
    ["mu1-spiral" "env4"] (make-foodwalk-fn (look-fns 4) composite-mu1-spiral-vecs-gen)

    ["mu15-spiral" "env0"] (make-foodwalk-fn (look-fns 0) composite-mu15-spiral-vecs-gen)
    ["mu15-spiral" "env1"] (make-foodwalk-fn (look-fns 1) composite-mu15-spiral-vecs-gen)
    ["mu15-spiral" "env2"] (make-foodwalk-fn (look-fns 2) composite-mu15-spiral-vecs-gen)
    ["mu15-spiral" "env3"] (make-foodwalk-fn (look-fns 3) composite-mu15-spiral-vecs-gen)
    ["mu15-spiral" "env4"] (make-foodwalk-fn (look-fns 4) composite-mu15-spiral-vecs-gen)
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPERIMENTS

(comment
  ;; doesn't seem to do anything
  (require 'utils.misc)
  (utils.misc/set-pp-width 100)

  (def some-walks
    (into (sorted-map) ; this line is optional; in large submaps, the experiments might not be in order.
          (select-keys random-walk-fns
                       [;; PURE RANDOM WALKS:

                        ["mu15" "env0"]
                        ["mu15" "env1"]
                        ["mu15" "env2"]
                        ["mu15" "env3"]
                        ["mu15" "env4"]

                        ["mu2"  "env0"]
                        ["mu2"  "env1"]
                        ["mu2"  "env2"]
                        ["mu2"  "env3"]
                        ["mu2"  "env4"]

                        ["mu25" "env0"]
                        ["mu25" "env1"]
                        ["mu25" "env2"]
                        ["mu25" "env3"]
                        ["mu25" "env4"]

                        ;; COMPOSITE RANDOM-RANDOM WALKS:

                        ["mu1-mu3" "env0"]
                        ["mu1-mu3" "env1"]
                        ["mu1-mu3" "env2"]
                        ["mu1-mu3" "env3"]
                        ["mu1-mu3" "env4"]

                        ["mu15-mu3" "env0"]
                        ["mu15-mu3" "env1"]
                        ["mu15-mu3" "env2"]
                        ["mu15-mu3" "env3"]
                        ["mu15-mu3" "env4"]

                        ;; COMPOSITE RANDOM-SPIRAL WALKS:

                        ["mu1-spiral" "env0"]
                        ["mu1-spiral" "env1"]
                        ["mu1-spiral" "env2"]
                        ["mu1-spiral" "env3"]
                        ["mu1-spiral" "env4"]

                        ["mu15-spiral" "env0"]
                        ["mu15-spiral" "env1"]
                        ["mu15-spiral" "env2"]
                        ["mu15-spiral" "env3"]
                        ["mu15-spiral" "env4"]
                       ])))

  (def walks-per-fn 10)
  (def result (time (fr/walk-experiments params some-walks walks-per-fn seed)))

  (require '[tech.v3.dataset :as ds])
  (require '[forage.core.techmlds :as ft])
  (def prev-seed seed)
  (def prev-seed 892177989826665365)
  (def nippyname (str basename seed "data.nippy"))
  (def data (ds/->dataset nippyname)) ; read from file automatically created earlier
  (ft/prall data)
  (ds/descriptive-stats data)

  (def spiral-result (time (fr/walk-experiments params spiral-walk-fns 1 seed)))
  (def spiral-seed 6605747748945559656)
  (def spiralnippyname (str basename spiral-seed "data.nippy"))
  (def spiral-data (ds/->dataset spiralnippyname))
  (ft/prall spiral-data)

)
