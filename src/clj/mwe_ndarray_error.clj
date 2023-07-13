(ns mwe-ndarray-error
  (:require [clojure.core.matrix :as mx]))

(defn make-mat
  [size]
  (println "In make-mat with size=" size)
  (let [m (mx/new-matrix :ndarray size size)]
    (println "Made matrix, with dimensions" (mx/shape m))
    (doseq [row (range size)
            col (range size)]
      (mx/mset! m col row 1))
    (println "Done initializing matrix")
    m))

(def m (make-env 100000))

(clojure.repl/pst)
