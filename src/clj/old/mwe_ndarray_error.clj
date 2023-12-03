(ns mwe-ndarray-error
  (:require [clojure.core.matrix :as mx]))

(defn make-mat
  [mattype size]
  (println "In make-mat with size =" size)
  (let [m (mx/new-matrix mattype size size)]
    (println "Made matrix, with dimensions" (mx/shape m))
    (doseq [row (range size)
            col (range size)]
      (mx/mget m col row)
      ;(mx/mset! m col row 1)
      )
    (println "Done initializing matrix")
    m))

(def size 100000)

(comment
  (def ndmat (make-mat :ndarray size))
  (def psmat (make-mat :persistent-vector size))
)
