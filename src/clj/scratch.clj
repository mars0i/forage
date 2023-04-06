;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [utils.random :as ur]
   [utils.math :as um]
   [utils.toroidal :as t]
   [forage.food :as f]
   [forage.walks :as w]
   ;[clojure.math.numeric-tower :as nt]
   ;[oz.core :as oz]
   ;[forage.viz.hanami :as h]   ; don't load with cljplot
   ;[aerial.hanami.common :as hc]
   ;[aerial.hanami.templates :as ht]
   ;[forage.viz.cljplot :as cp] ; don't load with hanami
   ))

(def $ "$ is an abbreviation for partial." partial)
;; partial is a lot slower than (fn [...] ...) with four or more args 
;; passed to the fn, but that would only matter in an inner loop.


(def maxpathlen 100)

(defn realized-fitness-benefit
  [benefit success]
  (* benefit success))

(defn realized-fitness-cost
  [cost pathlen]
  (* cost pathlen))

(defn realized-fitness
  [benefit cost [success pathlen]]
  (- (* benefit success) (* cost pathlen)))


(comment

  (def results1 [[1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]])

  (def results2 [[1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.3)]
                 [1 (* maxpathlen 0.4)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.6)]
                 [1 (* maxpathlen 0.7)]
                 [1 (* maxpathlen 0.8)]
                 [1 (* maxpathlen 0.9)]
                 [1 (* maxpathlen 1.0)]])

  (def results3 [[1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]])

  (def results4 [[0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]
                 [1 maxpathlen]])

  (map ($ realized-fitness maxpathlen 1) results7)
  (um/variance (map ($ realized-fitness maxpathlen 1) results1))
  (um/variance (map ($ realized-fitness maxpathlen 1) results2))
  (um/variance (map ($ realized-fitness maxpathlen 1) results3))
  (um/variance (map ($ realized-fitness maxpathlen 1) results4))

  (um/sample-variance (map ($ realized-fitness maxpathlen 1) results1))
  (um/sample-variance (map ($ realized-fitness maxpathlen 1) results2))
  (um/sample-variance (map ($ realized-fitness maxpathlen 1) results3))
  (um/sample-variance (map ($ realized-fitness maxpathlen 1) results4))
)


"loaded"
