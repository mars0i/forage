;; Utility functions for managing csv data
(ns utils.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File I/O functions

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

(comment
  (def filename "yo.csv")
  (def out-data [["this", "that", 42, 17.05, nil]
                 ["they", "them", 15, -19.27, true]
                 ["what", "wait", -99, 103.450, false]])
  (spit-csv filename out-data)
  (def in-data (slurp-csv filename))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for concatenating data from different rows

;; Utility function for concat-rows
(defn add-key-to-front
  "Given a map in which values are sequences, returns a sequence of those
  sequences with the key consed onto the front of each sequence."
  [[k v]]
  (cons k v))

;; Utility function for concat-rows
(defn create-data-map
  "Given a sequence of sequences, uses the element at key-col in each
  sequence as a key in a new map, from keys to the concatenation of
  subsequences (after dropping init-cols columns).  Note that key-col might
  appear in the dropped columns, or even in the remaining data columns."
  [init-cols key-col seqs]
  (reduce (fn [data-map row]
            (update data-map
                    (nth row key-col) ; the key
                    (fn [prev-data] (concat prev-data ; add new data to value
                                            (drop init-cols row)))))
          {} seqs))

(defn concat-rows
  "Runs through a sequence (top) of sequences (middle) of sequences
  (bottom). Drops the first header-rows (default 1) from each middle
  sequence.  Then creates a new sequence of sequences. The first element of
  each is a unique element that was at key-col in the bottom sequences.
  The rest of each sequence is the concatenation of post init-cols elements
  in bottom sequences that shared the same element at key-col. In other
  words, that element becomes an identifying name for the data in the rest
  of each row."
  ([init-cols key-col seqs]
   (concat-rows 1 init-cols key-col seqs))
  ([header-rows init-cols key-col seqs]
   (let [headless-seqs (mapcat (partial drop header-rows) seqs)]
     (map add-key-to-front 
          (create-data-map init-cols key-col headless-seqs)))))


(comment
  (def csv-seqs [[["NOTHING", "KEY" "NUMNAME", "LETTER", "INDEX"]
                  [1, "first", "one", "a", 1]
                  [2, "first", "two", "b", 1]
                  [3, "second", "three", "c", 1]]
                 [["NOTHING", "KEY" "NUMNAME", "LETTER", "INDEX"]
                  [4, "second", "four", "d", 2]
                  [5, "third", "five", "e", 2]
                  [5, "first" "six", "f" 2]]])

  (def header-rows 1)
  (def init-cols 2)
  (def key-col 1)
  (create-data-map init-cols key-col csv-seqs)
  (concat-rows header-rows init-cols key-col csv-seqs)
  (concat-rows init-cols key-col csv-seqs)

)
