;; LARGE ENVIRONMENT, SPARSER FOODSPOTS
(ns forage.experiment.grid4
  (:require
   [forage.run :as fr]
   [utils.random :as r]
   [utils.math :as m]))

;(def seed (inc (r/make-seed)))
;(println "SEED:" seed)

;; Breaking up the experiments into parts because doing all them
;; requires too much space in the JVM, leading it to abort.
(def all-exponents [1.001 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3])
(comment (count all-exponents) )
(def exponents1 (vec (take 5 all-exponents)))
(def exponents2 (vec (take 6 (drop 5 all-exponents))))
(def exponents3 (vec (take 5 (drop 11 all-exponents))))
(def exponents4 (vec (take 5 (drop 16 all-exponents))))

(def walks-per-combo 5000)
;; Note Excel might have a 16K columns max

;(/ (/ (* (* 16 5000) 0.1) 60) 60)

;; TESTING
;(def exponents [1.001 1.5 2 2.5 3])
;(def walks-per-combo 100)
;(def exponents [1.001 2])
;(def walks-per-combo 2)

;; NOTE LARGE ENVIRONMENT, SPARSER FOODSPOTS
;; Taking 23-30 seconds per individual run.
;; So 25000 runs takes about ten minutes.
(def half-size 50000) ; half the full width of the env
(def params (sorted-map ; sort so labels match values
              :food-distance     400
              :perc-radius       1  ; distance that an animal can "see" in searching for food
              :powerlaw-min      1
              :env-size          (* 2 half-size)
              :init-loc          [half-size half-size] ; i.e. center of env
              :maxpathlen        half-size  ; for straight walks, don't go too far
              :trunclen          half-size  ; max length of any line segment
              :look-eps          0.1  ; increment within segments for food check
              :max-frac          0.25 ; proportion of pi to use as maximum direction (0 is min) ; ignored if num-dirs is falsey
              :num-dirs          nil ; split range this many times + 1 (includes range max); nil for random
             ))

(comment
  (require '[forage.run :as fr])
  (require '[utils.random :as r])
  (time (fr/levy-experiments fr/default-file-prefix (r/make-seed) params exponents1 walks-per-combo))
  (time (fr/levy-experiments fr/default-file-prefix (r/make-seed) params exponents2 walks-per-combo))
  (time (fr/levy-experiments fr/default-file-prefix (r/make-seed) params exponents3 walks-per-combo))
  (time (fr/levy-experiments fr/default-file-prefix (r/make-seed) params exponents4 walks-per-combo))
)

