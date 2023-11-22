;; VERSION OF spiral26profiling.clj TO INVESTIGATE A DISCREPANCY BETWEEN
;; OUTCOMES WITH env-mason AND env-minimal.

(ns forage.experiment.spiral26discrepancy-1645093054649086646
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
             :look-eps            0.2
             :foodspot-coords-fn  "UPDATE FOR ENV TYPE"
             :rpt-to-stdout?       true ; write-experiments writes to stdout only if true
             :save-to-files?       true ; write-experiments saves summary data to files only if true
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

(first target-coords)
(first minimal-envs)

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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; VERSIONS IN WHICH GENERATION OF WALKS IS INCLUDED IN THE TIME
  ;; Note that in this case a *different* walk is used in each env
  ;; because mu2-vecs ultimately calls make-levy-vecs with an 
  ;; updated rng state. HOWEVER, the same series of walks is
  ;; (supposed to be at least) used in each env type, i.e.
  ;; env-minimal, env-mason.
 

  ;; THE PROBLEM BELOW WAS DUE TO A BUG IN env-minimal/make-look-fn.  It
  ;; was stepping through the env incorrectly, and finding spurious
  ;; foodspots.
  ;; The discrepancy between the last point of the walk til found and the
  ;; the corresponding point when nothing is found was due to
  ;; walks/path-with-food, which is supposed to return as the last point of
  ;; the sequence finding the target, the point closest to the target, not
  ;; the end of the segment.
  ;; TODO QUESTION: Does this work with env-minimal and env-single?
  ;; Yes, but the point they return is slightly different.

  ;;;; INVESTIGATE THIS:
  ;;;; 
  ;;;; seed = -1645093054649086646
  ;;;;
  ;;;; Why is env-minimal finding a target that env-mason is not?
  ;;;; [Note that I accidentally didn't run the last env. s/b 5. This was my pattern previously.]
  ;;;; [Also interesting that env-mason is faster. I've seen that before.]
  ;;;; 
  ;;;; ; eval (effective-root-form): (def walks-per-fn 100)
  ;;;; #'forage.experiment.spiral26profiling/walks-per-fn
  ;;;; ; --------------------------------------------------------------------------------
  ;;;; ; eval (effective-root-form): (let [new-mu2-walk-fns {"mu2-env0" (fn [init-loc] (w/foodwalk e...
  ;;;; ; (out) Performing 400 runs in groups of 100 ...
  ;;;; ; (out) group 1 [walk-fn mu2-env0, init-dir nil] ... num found =   1, efficiency = 0.0000000010100929485382696
  ;;;; ; (out) group 2 [walk-fn mu2-env1, init-dir nil] ... num found =   1, efficiency = 0.0000000010100778726471375
  ;;;; ; (out) group 3 [walk-fn mu2-env2, init-dir nil] ... num found =   0, efficiency = 0.0
  ;;;; ; (out) group 4 [walk-fn mu2-env3, init-dir nil] ... num found =   2, efficiency = 0.000000002040252240509354
  ;;;; ; (out)  done.
  ;;;; ; (out) "Elapsed time: 1932363.727214 msecs"
  ;;;; #'forage.experiment.spiral26profiling/result-minimal
  ;;;; ; --------------------------------------------------------------------------------
  ;;;; ; eval (file): .../clojure/forage/src/clj/forage/experiment/spiral26profiling.clj
  ;;;; ; (out) Using seed -1645093054649086646
  ;;;; nil
  ;;;; ; --------------------------------------------------------------------------------
  ;;;; ; eval (effective-root-form): (let[new-mu2-walk-fns {"mu2-env0" (fn [init-loc] (w/foodwalk w/...
  ;;;; ; (out) Performing 400 runs in groups of 100 ...
  ;;;; ; (out) group 1 [walk-fn mu2-env0, init-dir nil] ... num found =   0, efficiency = 0.0
  ;;;; ; (out) group 2 [walk-fn mu2-env1, init-dir nil] ... num found =   1, efficiency = 0.0000000010100778731046172
  ;;;; ; (out) group 3 [walk-fn mu2-env2, init-dir nil] ... num found =   0, efficiency = 0.0
  ;;;; ; (out) group 4 [walk-fn mu2-env3, init-dir nil] ... num found =   2, efficiency = 0.000000002040252242673076
  ;;;; ; (out)  done.
  ;;;; ; (out) "Elapsed time: 1114066.769844 msecs"
  ;;;; #'forage.experiment.spiral26profiling/result-mason
  ;;;;
  ;;;; More info:
  ;;;;
  ;;;; (def badruns-minimal (first (result-minimal :found-coords)))
  ;;;; (def badruns-mason (first (result-mason :found-coords)))
  ;;;; (def inconsistency (vec (map not= badruns-minimal badruns-mason)))
  ;;;; (.indexOf inconsistency true) 
  ;;;; (badruns-minimal 12) ;=> [10000.0 11000.0]
  ;;;; (badruns-mason 12) ;=> nil


  (def walks-per-fn 12) ;; I'm really interested in the 13th run, but I want to eat the first 12 to get to it.

  ;; NOTE It's not enough to reset the PRNG to a known state here.  You
  ;; have to revaluate this whole file WITH THE SAME SEED; I think parts of 
  ;; the code in the file uses random numbers in code outside of comments when first run.
  (let [new-mu2-walk-fns
        {"mu2-env0" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         ;"mu2-env1" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 1)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         ;"mu2-env2" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 2)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         ;"mu2-env3" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 3)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         ;"mu2-env4" (fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 4)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
         }]
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

  (def fwdata-minimal
    ((fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
     ((params :init-loc-fn))))

  (def lil-walk (take 12 (drop 1262 mason-tilfound)))

  (def fwdata-minimal-lil
    ((fn [init-loc] (w/foodwalk envminimal/find-in-seg (make-unbounded-envminimal-look-fn (minimal-envs 0)) "IGNORED" lil-walk))
     ((params :init-loc-fn))))

  ;; env-mason
  (let[new-mu2-walk-fns
       {"mu2-env0" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        ;"mu2-env1" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 1)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        ;"mu2-env2" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 2)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        ;"mu2-env3" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 3)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        ;"mu2-env4" (fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 4)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
        }]
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

  (def fwdata-mason
    ((fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) (params :look-eps) (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
     ((params :init-loc-fn))))

  (def fwdata-mason-eps01
    ((fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) 0.1 (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
     ((params :init-loc-fn))))

  (def fwdata-mason-eps001
    ((fn [init-loc] (w/foodwalk w/find-in-seg (make-unbounded-envmason-look-fn (mason-envs 0)) 0.01 (w/walk-stops init-loc (mu2-vecs (params :maxpathlen)))))
     ((params :init-loc-fn))))

  (first fwdata-minimal) ; check that found a target
  (def minim-tilfound (second fwdata-minimal))
  (def minim-pastfound (nth fwdata-minimal 2))
  (count minim-tilfound) ;=> 1268
  (count minim-pastfound) ;=> 1008652
  (+ 1268 1008652) ;=> 1009920
  (drop 1262 minim-tilfound)
  (take 6 minim-pastfound)

  (first fwdata-mason) ; check that failed to find a target
  (def mason-tilfound (second fwdata-mason)) ; should be the whole walk, since nothing found
  (def mason-pastfound (nth fwdata-mason 2)) ; should be nil
  (count mason-tilfound) ;=> 1009920
  (take 12 (drop 1262 mason-tilfound))


  ;; SOMETHING WEIRD IS HAPPENING IN THE LAST POINT IN the env-minimal
  ;; tilfound sequence.  There's a point there that doesn't appear in
  ;; the env-mason tilfound sequence, [9999.562482364767 11000.293304096997].
  ; --------------------------------------------------------------------------------
  ; eval (word): last-segment
  ; [[10008.406713748816 11013.486120836493]
  ;  [9999.562482364767 11000.293304096997]]
  ; --------------------------------------------------------------------------------
  ; eval (word): should-be-last-segment
  ; [[10008.406713748816 11013.486120836493]
  ;  [9991.226003348465 10987.857894490566]]

  (first fwdata-mason-eps01) ; check that failed to find a target
  (def mason-tilfound-eps01 (second fwdata-mason-eps01)) ; should be the whole walk, since nothing found
  (def mason-pastfound-eps01 (nth fwdata-mason-eps01 2)) ; should be nil
  (count mason-tilfound-eps01) ;=> 1009918  ;; WHY TWO POINTS SHORTER?
  (take 12 (drop 1262 mason-tilfound-eps01))


  (first fwdata-mason-eps001) ; check that failed to find a target
  (def mason-tilfound-eps001 (second fwdata-mason-eps001)) ; should be the whole walk, since nothing found
  (def mason-pastfound-eps001 (nth fwdata-mason-eps001 2)) ; should be nil
  (count mason-tilfound-eps001) ;=> 1009918  ;; WHY TWO POINTS SHORTER?
  (take 12 (drop 1262 mason-tilfound-eps001))


  (def equal-steps? (map (fn [minimal mason index] [(= minimal mason) index])
                         walk-til-found-minimal
                         walk-unfound-mason
                         (range)))


  (filter (fn [[b i]] (not b)) equal-steps?) 
  (nth equal-steps? 1267)



)
