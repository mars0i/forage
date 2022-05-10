(ns forage.experiment.fournier5plots
  (:use forage.experiment.fournier5)
  (:require [forage.run :as fr]
            [utils.random :as r]
            [forage.walks :as w]))

;(def seed (r/make-seed))
(def seed 9178237170000800769) ; Used for sims for PSA paper, I think
(def rng (r/make-well19937 seed))

(comment
  (time (def fws (doall (repeatedly 360 #(fr/levy-run rng look-fn nil params 1.5))))) ; 2
  (time (def fws (doall (repeatedly 16 #(fr/levy-run rng look-fn nil params 3)))))

  ;; place runs with found foodspots first:
  (def sorted-fws (sort-by #(if (first %) 0 1) fws))

  ;; NOTE: Don't use trim-full-walk here: IT'S ALREADY CALLED in levy-foodwalk

  (time (fr/write-foodwalk-plots (str (System/getenv "HOME") "/docs/src/data.foraging/forage/yo_mu3") :svg seed env 200 16 4 1000 3 params sorted-fws)) ; 2

  "/src/data.foraging/forage/fournier5may2022/mu2successes13"
)

