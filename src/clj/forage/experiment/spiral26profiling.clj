;; Initially copied from spiral25profiling.clj, which features comparisons
;; of env-single, env-minimal, and env-mason on single-target envs.
;; This is similar, but uses six-target envs, as used in spiral24* and
;; previous "spiral" namespaces.  Thus env-single is not used below.
(ns forage.experiment.spiral26profiling
  (:require [criterium.core :as crit]
            ;[clj-async-profiler.core :as prof]
            ;[clojure.math :as cmath]
            [forage.core.run :as fr]
            [forage.core.food :as f]
            [forage.core.walks :as w]
            [forage.core.env-minimal :as envminimal]
            [forage.core.env-mason :as envmason]
            [utils.misc :as misc]
            [utils.math :as um]
            [utils.random :as r]
            [utils.spiral :as sp]
            [utils.csv :as csv])
    (:import [clojure.lang IFn$DDO]))


(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(def targets-per-env 6)

(def homedir (System/getenv "HOME"))
(def default-dirname (str homedir "/docs/src/data.foraging/forage/spiral25/"))

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
             :basename            (str default-dirname "spiral25_")
             :look-eps            0.2 ; shouldn't be used
             :foodspot-coords-fn  "UPDATE FOR ENV TYPE"
             :rpt-to-stdout       true ; write-experiments writes to stdout only if true
             :save-to-files       true ; write-experiments saves summary data to files only if true
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP

(defn shift-point
  "Shifts the point [x y] to the right by inc-x and up by inc-y (where
  these last two values may be negative)."
  [inc-x inc-y [x y]]
  [(+ x inc-x) (+ y inc-y)])

(defn radial-target-coords
  "Create coordinates for num-targets that are evenly spaced radially
  around the center of env (i.e. around [half-size, half-size]) at a
  distance nomin/denom X half-size."
  [env-size num-targets denom nomin]
  (let [half-size (double (/ (params :env-size) 2)) ; probably already defined as var, but should get from params
        distance-from-zero (* (/ nomin denom) half-size)
        first-zero-tgt [distance-from-zero 0] ; east from origin--will be shifted later
        directions (map (fn [n] (* um/pi2 (/ n num-targets)))
                        (rest (range num-targets))) ; don't divide by zero
        zero-targets (cons first-zero-tgt
                           (map (fn [dir] (um/rotate dir first-zero-tgt))
                                directions))
        targets (map (partial shift-point half-size half-size)
                     zero-targets)]
    targets))

(def target-coords (mapv (partial radial-target-coords (params :env-size) 
                           targets-per-env 5)
                  (range 1 6))) ; five targets at 1/5, 2/5, 3/5, 4/5, 5/5 of distance to border

(def minimal-envs (mapv envminimal/make-env target-coords))
(def mason-envs (mapv (partial envmason/make-env (params :env-discretization) (params :env-size)) target-coords))

(comment
  ;; Visually check that envs are as intended
  (require '[forage.viz.hanami :as h])
  (require '[oz.core :as oz])
  (oz/start-server!)

  (def minimal-env-plots (mapv (fn [env] (h/vega-food-plot 
                                           (map h/make-foodspot (envminimal/env-foodspot-coords env))
                                           (params :env-size)
                                           600
                                           200))
                               minimal-envs))

  (oz/view! (minimal-env-plots 2))

  (def mason-env-plots (mapv (fn [env] (h/vega-food-plot 
                                           (map h/make-foodspot (envmason/env-foodspot-coords env))
                                           (params :env-size)
                                           600
                                           200))
                               mason-envs))

  (oz/view! (mason-env-plots 1))
)


(defn make-unbounded-envminimal-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (envminimal/make-look-fn env (params :perc-radius)))

(defn make-unbounded-envmason-look-fn
  "Make a non-toroidal look-fn from env.  Searches that leave the core env
  will just continue without success unless they wander back."
  [env]
  (let [^double perc-radius (params :perc-radius)]
    ^IFn$DDO (fn [^double x ^double y]
               (envmason/perc-foodspots-exactly env perc-radius x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE THE EXPERIMENTS

(def seed (r/make-seed))
;(def seed -7370724773351240133)
(def seed -1645093054649086646)
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

;; Doesn't use more-mu2-vecs because that is limited to
;; examine-segment-len, total.  This extends the walk to maxpathlen.
(defn mu2-vecs
  [maxpathlen]
  (w/vecs-upto-len maxpathlen (w/make-levy-vecs rng mu2dist  1 (params :trunclen))))

(def first-mu2-vecs (mu2-vecs (params :maxpathlen)))


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
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envminimal/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" (walks 0)))
                          "mu2-env1" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 1)) "IGNORED" (walks 1)))
                          "mu2-env2" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 2)) "IGNORED" (walks 2)))
                          "mu2-env3" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 3)) "IGNORED" (walks 3)))
                          "mu2-env4" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 4)) "IGNORED" (walks 4)))}]
    (time (crit/quick-bench
            (fr/walk-experiments (update params
                                         :basename #(str % "env_minimal_mu2_1each")
                                         :foodspot-coords-fn  env-minimal/foodspot-coords)
                                 new-mu2-walk-fns walks-per-fn seed))))

  (clojure.repl/pst)

  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  ;(r/set-state rng initial-state) ; not needed since walks are pre-generated
  (let [new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) (params :look-eps) (walks 0)))
                          "mu2-env1" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 1)) (params :look-eps) (walks 1)))
                          "mu2-env2" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 2)) (params :look-eps) (walks 2)))
                          "mu2-env3" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 3)) (params :look-eps) (walks 3)))
                          "mu2-env4" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 4)) (params :look-eps) (walks 4)))}]
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
        new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" (next-walk!)))
                          "mu2-env1" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 1)) "IGNORED" (next-walk!)))
                          "mu2-env2" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 2)) "IGNORED" (next-walk!)))
                          "mu2-env3" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 3)) "IGNORED" (next-walk!)))
                          "mu2-env4" (fn [ignored-init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 4)) "IGNORED" (next-walk!)))}]
    (time
      ;(crit/quick-bench
      (def result-minimal
        (-> params
            (assoc :foodspot-coords-fn  envminimal/foodspot-coords)
            (update :basename #(str % "env_minimal_mu2_" walks-per-fn "each"))
            (fr/walk-experiments new-mu2-walk-fns walks-per-fn seed))
      )
    )
  )

  (clojure.repl/pst)

  ;; env-mason
  (let [i$ (atom -1)
        next-walk! #(walks (swap! i$ inc))
        new-mu2-walk-fns {"mu2-env0" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) (params :look-eps) (next-walk!)))
                          "mu2-env1" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 1)) (params :look-eps) (next-walk!)))
                          "mu2-env2" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 2)) (params :look-eps) (next-walk!)))
                          "mu2-env3" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 3)) (params :look-eps) (next-walk!)))
                          "mu2-env4" (fn [ignored-init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 4)) (params :look-eps) (next-walk!)))}]
    (time
      ;(crit/quick-bench
      (def result-mason
        (-> params
            (assoc :foodspot-coords-fn  envmason/foodspot-coords)
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
 
  (def walks-per-fn 100)

  ;; NOTE It's not enough to reset the PRNG to a known state here.  You
  ;; have to revaluate this whole file WITH THE SAME SEED; I think parts of 
  ;; the code in the file uses random numbers in code outside of comments when first run.
  (let [new-mu2-walk-fns
        {"mu2-env0" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env1" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env2" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env3" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         "mu2-env4" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 4)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
    (time
      ;(crit/quick-bench
      (def result-minimal
        (-> params
            (assoc :foodspot-coords-fn  envminimal/foodspot-coords)
            (update :basename #(str % "env_minimal_mu2_" walks-per-fn "each"))
            (fr/walk-experiments new-mu2-walk-fns walks-per-fn seed))
      )
    )
  )

  (clojure.repl/pst)

  ;; env-mason
  (let[new-mu2-walk-fns
       {"mu2-env0" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env1" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 1)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env2" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 2)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env3" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 3)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        "mu2-env4" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 4)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
    (time
      ;(crit/quick-bench
      (def result-mason
        (-> params
            (assoc :foodspot-coords-fn  envmason/foodspot-coords)
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
         {"mu2-env0" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))

  (clojure.repl/pst)

  ;; env-mason
  ;; note if needed: params s/b/ (update params :foodspot-coords-fn envmason/foodspot-coords)
  (do
    (r/set-state rng initial-state)
    (let[new-mu2-walk-fns
         {"mu2-env0" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env1" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 1)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env2" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 2)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
          "mu2-env3" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 3)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))}]
      (def new-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) new-mu2-walk-fns 10 seed)))))

  (def fixed-mu2-data-and-rng (time (fr/walk-experiments (update params :basename #(str % "new-mu2")) fixed-mu2-walk-fns 10 seed)))

)
