;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
   [utils.random :as ur]
   [utils.math :as um]
   [utils.toroidal :as t]
   [forage.food :as f]
   [forage.walks :as w]
   [forage.viz.hanami :as h]   ; don't load with cljplot
   ;[forage.viz.cljplot :as cp] ; don't load with hanami
   [oz.core :as oz]
   ;[clojure.math.numeric-tower :as nt]
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]
   ))

(def $ "$ is an abbreviation for partial." partial)
;; partial is a lot slower than (fn [...] ...) with four or more args 
;; passed to the fn, but that would only matter in an inner loop.


(def maxpathlen 100)

(defn fitness-benefit
  [benefit success]
  (* benefit success))

(defn fitness-cost
  [cost pathlen]
  (* cost pathlen))

(defn fitness
  [benefit cost [success pathlen]]
  (- (* benefit success) (* cost pathlen)))


(comment

(def results1 [[0 maxpathlen]
              [1 (* maxpathlen 0.5)]
              [0 maxpathlen]
              [0 maxpathlen]
              [1 (* maxpathlen 0.9)]
              [0 maxpathlen]
              [0 maxpathlen]
              [0 maxpathlen]
              [1 (* maxpathlen 0.75)]
              [0 maxpathlen]])

  (map (comp ($ fitness-benefit maxpathlen) first) results1)
  (map (comp ($ fitness-cost 1) second) results1)

  (map ($ fitness maxpathlen 1) results1)

  (count results1)
  (um/variance '(-100 50.0 -100 -100 10.0 -100 -100 -100 25.0 -100))

  (um/variance '(-100 50.0 -100 -100 10.0 -100 -100 -100 25.0 -100))
  (um/variance (map ($ fitness maxpathlen 1) results1))

  (um/sample-variance '(-100 50.0 -100 -100 10.0 -100 -100 -100 25.0 -100))
  (um/sample-variance (map ($ fitness maxpathlen 1) results1))


  (def results2 [[0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]])

  (def results3 [[0 maxpathlen]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]])

  (def results4 [[0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]
                 [1 (* maxpathlen 0.1)]])

  (def results5 [[0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [1 (* maxpathlen 0.9)]
                 [1 (* maxpathlen 0.9)]
                 [1 (* maxpathlen 0.9)]
                 [1 (* maxpathlen 0.9)]
                 [1 (* maxpathlen 0.9)]])

  (def results6 [[0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [0 maxpathlen]
                 [1 (* maxpathlen 0.5)]
                 [1 (* maxpathlen 0.5)]])

  (map ($ fitness maxpathlen 1) results6)

  (um/variance (map ($ fitness maxpathlen 1) results1))
  (um/variance (map ($ fitness maxpathlen 1) results2))
  (um/variance (map ($ fitness maxpathlen 1) results3))
  (um/variance (map ($ fitness maxpathlen 1) results4))
  (um/variance (map ($ fitness maxpathlen 1) results5))
  (um/variance (map ($ fitness maxpathlen 1) results6))

  (um/sample-variance (map ($ fitness maxpathlen 1) results1))
  (um/sample-variance (map ($ fitness maxpathlen 1) results2))
  (um/sample-variance (map ($ fitness maxpathlen 1) results3))
)


"loaded"
