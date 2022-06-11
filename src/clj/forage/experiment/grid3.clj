(ns forage.experiment.grid3
  (:require
   [forage.run :as fr]
   [utils.random :as r]
   [utils.math :as m]))

(def seed (inc (r/make-seed)))
(println "SEED:" seed)

(def exponents [1.001 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2 2.1 2.2 2.4 2.5  3])
(def walks-per-combo 5000) ; only for levy-experiments (straight-experiments isn't random)
;; Note Excel might have a 16K columns max

;(/ (/ (* (* 16 5000) 0.1) 60) 60)

;; TESTING
;(def exponents [1.001 1.5 2 2.5 3])
;(def walks-per-combo 100)
;(def exponents [1.001 2])
;(def walks-per-combo 2)


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
              :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
              :init-dirs          nil ; split range this many times + 1 (includes range max); nil for random
             ))

(comment
  ;; Parameters for testing:
  (require '[forage.run :as fr])
  ;(def params (assoc params :init-dirs 20))
  (def exponents [1.001 2])
  (def walks-per-combo 2)

  (use 'clojure.pprint)
  (time (def data (fr/levy-experiments fr/default-file-prefix seed params exponents walks-per-combo)))
  (time (def data (fr/straight-experiments fr/default-file-prefix params)))
  (pprint data)
  (pprint fw+)
)

