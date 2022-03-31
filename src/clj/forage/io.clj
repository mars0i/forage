(ns forage.io
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;

;; to write to file:
;; - seed at start of runs
;;     - for multiple runs with same params
;;     - for multiple parameter choices
;; - parameters for each set of runs
;; - count of foodspots found in a set of runs
;; 
;; - number of points or segments in each full walk (so we know how many
;;    PRNG calls there were in case want to fast-forward)
;; - or maybe just the total for a set of runs??
;; - maybe: overall length in (inches, whatever--not the number of segments
;;    or points) of walk up to food. Or this can be gotten later if needed by 
;;    re-running if necessary.

(defn spit-csv
  "Given a sequence of sequences of data in rows, opens a file and
  writes to it using write-csv.  options are those that can be passed
  to clojure.java.io/writer."
  [filename rows & options]
   (with-open [w (apply io/writer filename options)]
     (csv/write-csv w rows)))

(defn append-row
  "Given a value for seed, a sequence of parameters, a count of found
  foodspots in a collection of runs, and the total number of segments in
  all of the full walks (before truncation due to finding food) in the
  collection, appends a new row to existing rows and returns the result.
  Apart from params being a sequence, there are no restrictions on content,
  so this can be used to write a row of labels as well."
  ([values]
   (append-row [] values))
  ([prev-rows values]
   (conj (vec prev-rows) values)))


(defn append-labels
  "Appends a new row of labels.  param-names is a sequence containing
  strings, keywords, or symbols, which will be converted to strings as
  needed."
  ([param-names]
   (append-row (map name param-names)))
  ([prev-rows param-names] 
   (append-row prev-rows (map name param-names))))

