(ns forage.experiment.grid2
  (:require
   [forage.run :as fr]
   [utils.random :as r]
   [utils.math :as m]))

(def seed (inc (r/make-seed)))
;(def seed 1649988521705)
(println "SEED:" seed)

;; Alternative parameters for variation in runs:
;(def exponents [1.01 1.5 2 2.5 3])
(def exponents [1.4 1.5 1.6 1.7 1.8 1.9 2 2.1 2.2 2.3])

(def walks-per-combo 1000) ; only for levy-experiments (straight-experiments isn't random)

;; Fixed parameters for all runs
(def half-size 10000) ; half the full width of the env
(def params (sorted-map ; sort so labels match values
              :powerlaw-min      1
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :food-distance     200
              :env-size          (* 2 half-size)
              :init-loc          [half-size half-size] ; i.e. center of env
              :maxpathlen        half-size  ; for straight walks, don't go too far
              :trunclen          half-size  ; max length of any line segment
              :look-eps          0.1  ; increment within segments for food check
              :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min)
              :num-dirs          50; split range this many times + 1 (includes range max)
             ))

(comment
  ;; Parameters for testing:
  (def exponents [2 3])
  (def params (assoc params :num-dirs 20))
  (def init-dirs (doall (map #(* (/ % (params :num-dirs)) (/ m/pi 2))
                             (range (params :num-dirs)))))
  (def walks-per-combo 1)

  (use 'clojure.pprint)
  (time (def data (fr/levy-experiments seed params exponents walks-per-combo)))
  (time (def data (fr/straight-experiments params)))
  (pprint data)
  (pprint fw+)
)
