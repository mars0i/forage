;; Utility functions for managing csv data
(ns utils.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; Consider using instead/in addition the dataframe stuff in the scicloj project.

;; Also note dk.ative/docjure, which allows creating multilayer Excel files.


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

;(defn spit-3d-csv
;  [filename seq-seqs-of-rows & options]
;  (with-open [w (apply io/writer filename options)]
;    (doseq [rows seq-seqs-of-rows]
;      (csv/write-csv w rows))))


(defn slurp-csv
  "Given a sequence of sequences of data in rows, opens a file and reads it
  using read-csv (which is lazy).  options are those that can be passed to
  clojure.java.io/reader.  (NOTE assumes 2d data--not 1d, not 3d, etc.)"
  [filename & options]
  (with-open [r (apply io/reader filename options)]
    (doall (csv/read-csv r)))) ; read-csv is lazy, so need to force evaluation before closing the reader

(def digit-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

;; Note read-string interprets strings that begin with an alphabetic
;; character as symbols, and ignores anhything after the first space.
;; So it can't be used as is for what I want.
(defn number-or-string
  "Kludgey method to interpret a string as a number (long or double as
  needed) or a mere string.  If string s begins with a digit, assumes the
  string represents a number; otherwise, returns s as is.  NOTE uses
  `read-string`, which should not be used with untrusted data."
  [s]
  (let [c0 (first s)]
    (if (or (digit-chars c0) ; looks like a positive number
            (and (= c0 \-) (digit-chars (second s)))) ; looks like a negative number
      (read-string s)
      s)))

(defn numbers-or-strings
  "Convenience abstraction that maps number-or-string over a sequence of
  strings ss."
  [ss]
  (mapv number-or-string ss))


(defn read-2d-files-to-3d-vector
  "After prepending dirname to csv filenames in collection datafiles, reads
  each 2d file using slurp-csv, then converts data to numbers when possible
  using number-or-string, and returns a 3d vector of 2d vectors of the
  resulting data.  (This function is eager not lazy.)"
  [dirname datafiles]
  (mapv (fn [relpath]  ; note use of mapv's rather than map to thwart laziness
          (let [rows (slurp-csv (str dirname relpath))]
            (mapv numbers-or-strings rows)))
        datafiles))

(comment
  (number-or-string "that")
  (number-or-string "true") ; just a string--doesn't handle booleans
  (number-or-string "nil") ; just a string
  (number-or-string "25")   ; long
  (number-or-string "-25")   ; long
  (number-or-string "25.2") ; double
  (number-or-string "-25.2") ; double
  (number-or-string "9.71E+08") ; double
  (number-or-string "-9.71E+08") ; double
  (number-or-string "9.71E-03") ; double
  (number-or-string "011")  ; octal
  (number-or-string "-011")  ; octal
  (number-or-string "0x11") ; hex
  (number-or-string "-0x11") ; hex
  (number-or-string "2r11") ; binary
  (number-or-string "-2r11") ; binary
  (number-or-string "3r11") ; ternary
  (number-or-string "-3r11") ; ternary

  (def filename "yo.csv")
  (def out-data [["this", "that", 42, 17.05, nil]
                 ["they", "them", 15, -19.27, true]
                 ["what", "wait", -99, 103.450, false]])
  (spit-csv filename out-data)
  (def in-data (slurp-csv filename))

  (def filename2 "yo2.csv")
  (def out-data2 [["this2", "that2", 43, 18.05, ()]
                  ["they2", "them2", 16, -18.27, false]
                  ["what2", "wait2", -98, 104.450, true]])
  (spit-csv filename2 out-data2)
  (def in-data2 (slurp-csv filename2))

  (def in-data3d (read-2d-files-to-3d-vector "./" ["yo.csv" "yo2.csv"]))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for concatenating data from different rows

;; Consider also/instead using dk.ative/docjure.  This allows creating
;; multilayer Excel files.

;; Consider using the dataframe stuff in the scicloj project.

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


(defn concat-data-rows
  "Runs through a sequence (top) of sequences (middle) of sequences
  (bottom). Drops the first header-rows (default 1) from each middle
  sequence.  Then creates a new sequence of sequences. The first element of
  each is a unique element that was at key-col in the bottom sequences.
  The rest of each sequence is the concatenation of post init-cols elements
  in bottom sequences that shared the same element at key-col. In other
  words, that element becomes an identifying name for the data in the rest
  of each row."
  [header-rows init-cols key-col sum-cols seqs]
  (map add-key-to-front 
       (create-data-map-with-sums header-rows init-cols key-col sum-cols seqs)))


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

  (csv/spit-csv (str "./" "yo.csv") test-data-3d)
)
