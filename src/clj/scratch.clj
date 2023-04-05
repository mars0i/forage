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

(defn fitness
  [benefit cost [success pathlen]]
  (+ (* benefit success) (* cost pathlen)))

(def results [[0 maxpathlen]
              [1 (* maxpathlen 0.5)]
              [0 maxpathlen]
              [0 maxpathlen]
              [1 (* maxpathlen 0.9)]
              [0 maxpathlen]
              [0 maxpathlen]
              [0 maxpathlen]
              [1 (* maxpathlen 0.75)]
              [0 maxpathlen]])

(count results)



(comment
  (um/sample-variance (map ($ fitness maxpathlen -1) results))
  (um/variance (map ($ fitness maxpathlen -1) results))

)


"loaded"
