(ns forage.explore.repeatedsearch
  (:require [forage.walks :as w]))

;; How long to walk?
;; Until enough food?
;; Until total foodwalk length?
;; ?

(defn next-init-loc
  [rng found-food]
  ;; calculate next loc here
  ;; get coord of food
  ;; maybe move randomly away
  )

(defn repeated-search
  [rng end-test foodwalk-fn init-loc]
  (loop [loc init-loc
         foodwalks []]
    (let [foodwalk (foodwalk-fn loc)
          foodwalks (conj foodwalks+ foodwalk+)]
      (if (end-test foodwalk)
        foodwalks
        (recur (next-init-loc rng (first (first foodwalks)))
               foodwalks)))))




   
