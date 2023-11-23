;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
    [clojure.repl :refer :all] ; doc, apropos, dir, find-doc, pst, etc.
    [fastmath.core :as m]
    ;[clojure.core.matrix :as mx]
   ;[utils.random :as ur]
   ;[utils.math :as um]
   ;[utils.toroidal :as t]
   ;[forage.food :as f]
   ;[forage.walks :as w]
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


(defn infinite
  []
  (repeat 1))

(defn cutfinite
  [n]
  (doall (take n (infinite))))

(defn finite-list
  [n]
  (loop [acc '() 
         n' n]
    (if (zero? n)
      acc
      (recur (conj acc 1) (dec n)))))

(defn finite-vec
  [n]
  (loop [acc '() 
         n' n]
    (if (zero? n)
      acc
      (recur (conj acc 1) (dec n)))))

(comment
  (require '[criterium.core :as crit])
  (crit/quick-bench (cutfinite 10000))
  (crit/quick-bench (finite-list 10000))
  (crit/quick-bench (finite-vec 10000))
)


"loaded"
