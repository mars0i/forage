(ns utils.file
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; Note nils are converted to empty cells by write-csv.
(defn spit-csv
  "Given a sequence of sequences of data in rows, opens a file and
  writes to it using write-csv.  options are those that can be passed
  to clojure.java.io/writer."
  [filename rows & options]
   (with-open [w (apply io/writer filename options)]
     (csv/write-csv w rows)))

(defn slurp-csv
  "Given a sequence of sequences of data in rows, opens a file and
  writes to it using write-csv.  options are those that can be passed
  to clojure.java.io/writer."
  [filename & options]
  (with-open [r (apply io/reader filename options)]
    (doall (csv/read-csv r)))) ; read-csv is lazy, so need to force evaluation before closing the reader

(defn concat-rows
  ([num-initial-cols key-col csv-seqs]
   (concat-rols 1 num-initial-cols key-col csv-seqs))
  ([num-header-rows num-initial-cols key-col csv-seqs]
   (let [rows (drop num-header-rows csv-seqs)
         data-maps (map (fn 

         (into {}
                        (map (fn [row]
                               [(nth row key-col), (drop non-data-cols row)])
                             csvs))
         ks (keys data-map)]
     (map (fn [k] 
            (map 
            )
          ks)

   ))


(comment
  (def filename "yo.csv")
  (def out-data [["this", "that", 42, 17.05, nil]
                 ["they", "them", 15, -19.27, true]
                 ["what", "wait", -99, 103.450, false]])
  (spit-csv filename out-data)
  (def in-data (slurp-csv filename))
)
