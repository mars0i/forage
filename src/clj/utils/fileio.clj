(ns utils.fileio
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;

(defn spit-csv
  "Given a sequence of sequences of data, opens a file and writes to it
  using write-csv.  Options are those that can be passed to 
  clojure.java.io/writer."
  [filename rows & options]
   (with-open [w (apply io/writer filename options)]
     (csv/write-csv w rows)))
