(ns utils.fileio
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

(defn spit-csv-with-header
  "Given a sequence of sequences of data in rows, opens a file and
  writes to it using write-csv after prepending the header row.  
  options are those that can be passed to clojure.java.io/writer."
  [header filename rows & options]
  (let [header+rows (cons header rows)]
    (apply spit-csv filename header+rows options)))
