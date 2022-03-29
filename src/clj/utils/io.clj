(ns utils.io
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;

(defn spit-csv
  "Given a sequence of sequences of data in rows, opens a file and
  writes to it using write-csv.  options are those that can be passed
  to clojure.java.io/writer."
  [filename rows & options]
   (with-open [w (apply io/writer filename options)]
     (csv/write-csv w rows)))
