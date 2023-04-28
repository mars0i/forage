;; File for misc experiments.  Think of it as a repl.
(ns scratch
  (:require
    [clojure.core.matrix :as mx]
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
  (def m (mx/matrix :ndarray [[nil nil nil] [nil nil nil] [nil nil nil]]))
  (mx/mset! m 0 1 (list :a))
  (mx/pm m) ; succeeds
  (mx/mset! m 0 0 (list :a))
  (mx/shape m) ; => [3 3]
  (mx/pm m)
; eval (effective-root-form): (mx/pm m)
; (err) Execution error (ExceptionInfo) at clojure.core.matrix.impl.persistent-vector/eval22141$fn (persistent_vector.cljc:571).
; (err) Can't convert to persistent vector array: inconsistent shape.
  (mx/mset! m 0 0 nil) ; This fixes the problem

;; These are also not OK:
  (mx/mset! m 1 0 (range 1))
  (mx/mset! m 2 0 [:a])

  ;; These don't cause problems:
  (mx/mset! m 0 0 #{:a})
  (mx/mset! m 0 0 {:a 1})
)


"loaded"
