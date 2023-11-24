
;; Speed tests for different sequence generation methods:

(defn cutfinite
  [n]
  (doall (take n (repeat 1))))

(defn finite-list
  [n]
  (loop [acc '() 
         i n]
    (if (zero? i)
      acc
      (recur (conj acc 1) (dec i)))))

(defn finite-vec
  [n]
  (loop [acc '() 
         i n]
    (if (zero? i)
      acc
      (recur (conj acc 1) (dec i)))))

(comment
  (def yovec (finite-vec 5))
  (def yolist (finite-list 1000))
  (def yocut (cutfinite 1000))

  (require '[criterium.core :as crit])

  ;; The lazy version takes more tnan 4X of the time needed for the list 
  ;; and vec versions (which take about the same amount of time, although 
  ;; perhaps the list version is slightly faster):

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Times on MBP:
  (time (crit/quick-bench (finite-vec 1000)))  ; 11 microsecs
  (time (crit/quick-bench (finite-list 1000))) ; 11 microsecs
  (time (crit/quick-bench (cutfinite 1000)))   ; 46 microsecs 

  (time (crit/quick-bench (finite-vec 100000)))  ; 1.14 millisecs
  (time (crit/quick-bench (finite-list 100000))) ; 1.10 millisecs
  (time (crit/quick-bench (cutfinite 100000)))   ; 4.70 millisecs

  (time (crit/bench (finite-vec 100000)))  ; 1.10 millisecs
  (time (crit/bench (finite-list 100000))) ; 1.07 millisecs
  (time (crit/bench (cutfinite 100000)))   ; 4.91 millisecs
)
