(ns forage.io
  (:require [utils.io :as uio]))

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

(defn write-param-names
  [filename param-names]
  (uio/spit-csv filename
                (concat ["seed"] param-names "found" "segments")))

(defn write-set-of-runs
  [filename seed parameter-seq found-count segment-count]
  )

(defn add-row-from-runs
  ([seed params found-count total-segment-count]
   (add-row-from-runs [] seed params found-count total-segment-count))
  ([prev-rows seed params found-count total-segment-count]
   (conj prev-rows (concat [seed] params [found-count total-segment-count]))))

