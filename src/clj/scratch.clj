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



(comment
  (doc m/rint)
  (class 1.234e100)
  (m/rint 1.234e100)
  (class (m/rint 1.234e100))
  (m/round 1.234e100)
  Long/MAX_VALUE
  (class (m/round 1.234e100))

  (m/rint 4.5)
  (m/rint 5.5)
  (m/round 4.5)

  ;; This is standard Java rint behavior .5 is rounded to nearest even integer:
  (m/rint 42.5) ; rounds down
  (m/rint 43.5) ; rounds up
  ;; In fastmath's scaled version, the behavior is less obviously consistent:
  (m/rint 42.05 0.1) ; rounds down
  (m/rint 42.15 0.1) ; rounds down
  (m/rint 42.25 0.1) ; rounds down
  (m/rint 42.35 0.1) ; rounds up
  (m/rint 42.45 0.1) ; rounds down
  (m/rint 42.55 0.1) ; rounds down
  (m/rint 42.65 0.1) ; rounds down
  (m/rint 42.75 0.1) ; rounds up
  (m/rint 42.85 0.1) ; rounds down
  (m/rint 42.95 0.1) ; rounds up
)


"loaded"
