(ns forage.io
  (:require [utils.io :as uio]))

;; to write to file:
;; - seed at start of runs
;; - parameters for each run
;; - whether found food (or count of foodspots)
;; - number of points or segments in full walk (so we know how many
;;    PRNG calls there were in case want to fast-forward)
;; - maybe: overall length in (inches, whatever--not the number of segments
;;    or points) of walk up to food. Or this can be gotten later if needed by 
;;    re-running if necessary.
