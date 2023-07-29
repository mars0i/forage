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

;; since vectors are maps, can I do this with merge-with or something?
(defn create-data-map
  "Given a sequence of sequence of sequences, iterates over the contained
  2d structures, and uses the element at key-col in each inner 1d sequence
  as a key in a new map, from keys to the concatenation of subsequences
  from the different 2d structures (after dropping init-cols columns).
  Note that key-col might appear in the dropped columns, or even in the
  remaining data columns."
  [header-rows init-cols key-col seqs3d]
  (let [headless-rows (mapcat (partial drop header-rows) seqs3d)]
    (reduce (fn [data-map row]
              (update data-map
                      (nth row key-col) ; the key
                      (fn [prev-data] (concat prev-data ; add new data to value
                                              (drop init-cols row)))))
            {} headless-rows)))

;; since vectors are maps, can I do this with merge-with or something?
(defn create-sum-map
  "Given a sequence of sequence of sequences, iterates over the contained
  2d structures, and uses the element at key-col in each inner 1d sequence
  as a key in a new map, from keys to sums of numeric values at column
  sum-col in each inner 1d sequence (after dropping init-cols columns).
  Note that key-col might appear in the dropped columns, or even in the
  remaining data columns."
  [ks header-rows key-col sum-col seqs3d]
  (prn ks) ; DEBUG
  (prn header-rows key-col sum-col) ; DEBUG
  (let [sum-map (zipmap ks (repeat 0))
        headless-rows (mapcat (partial drop header-rows) seqs3d)]
    (prn sum-map) ; DEBUG
    (reduce (fn [smap row]
              (update smap
                      (prn (nth row key-col)) ; DEBUG
                      (nth row key-col) ; get key from row
                      (partial + (nth row sum-col)))) ; add in its value
            sum-map
            headless-rows)))


(defn create-data-map-with-sums
  [header-rows init-cols key-col sum-cols seqs3d]
  "Given a sequence of sequence of sequences, iterates over the contained
  2d structures, and uses the element at key-col in each inner 1d sequence
  as a key in a new map, from keys to sequences of data.  The data
  sequences contain, first, sums of numeric values from columns sum-cols
  (in order) in each inner 1d sequence (this is done by iterating
  create-sum-map), followed by concatenation of data sequences after
  dropping the first init-cols from each inner 1d sequence (as performed by
  create-data-map). Note that key-col might appear in the dropped columns,
  or even in the remaining data columns."
  (let [map-wout-sums (create-data-map header-rows init-cols key-col seqs3d)
        ks (keys map-wout-sums)
        reversed-sum-cols (reverse sum-cols) ; We'll conj onto front in order found
        sum-maps (map (fn [sum-col] (create-sum-map ks header-rows key-col sum-col seqs3d))
                      reversed-sum-cols)] ; first conj will be last, so conj reversed sum-cols
    ;; Note below we want conj not cons, because first arg needs to be map-wout-sums.
    ;; However, note this means that map-wout-sums vals must be non-vector sequences,
    ;;:because we want the sum columns to be added to the front.
    (apply merge-with 
           conj
           map-wout-sums sum-maps)))


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
   (map add-key-to-front 
        (create-data-map header-rows init-cols key-col seqs))))


(comment
  (def csv-seqs-3d [[["NOTHING", "KEY" "NUMNAME", "LETTER", "INDEX"]
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
  (def data-map (create-data-map 1 init-cols key-col csv-seqs-3d))

  (def sum-map4 (create-sum-map header-rows (keys data-map) 1 4 csv-seqs-3d))
  (def sum-map0 (create-sum-map header-rows (keys data-map) 1 0 csv-seqs-3d))

  (apply merge-with conj data-map [sum-map sum-map])

  (def sum-data-map
    (create-data-map-with-sums header-rows init-cols key-col [4 0] csv-seqs-3d))

)
