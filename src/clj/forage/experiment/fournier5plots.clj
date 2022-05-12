(ns forage.experiment.fournier5plots
  (:require [utils.random :as r]
            [forage.run :as fr]
            [forage.experiment.fournier5 :as f5]))

(defn f5plots
  [seed runs mu]
  (println "Running walks ...")
  (let [rng (r/make-well19937 seed)
        sorted-fws (time
                    (sort-by #(if (first %) 0 1)
                             (doall (repeatedly runs #(fr/levy-run rng f5/look-fn nil f5/params mu)))))
        success-count (count (filter first sorted-fws))]
    (println success-count "successful out of" runs "runs with mu =" mu "using seed" seed)
    (println "Creating svg files ...")
    (time (fr/write-foodwalk-plots
           (str (System/getenv "HOME") "/docs/src/data.foraging/forage/fournier5mu" mu)
           :svg seed f5/env 800 9 3 1000 mu f5/params sorted-fws))))


(comment

  (def seed (r/make-seed))
  (def seed 9178237170000800769) ; Used for sims for PSA paper, I think

  (f5plots seed 360 1.001)
  (f5plots seed 360 1.5)
  (f5plots seed 360 2.0)
  (f5plots seed 360 2.5)
  (f5plots seed 360 3.0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; OLD:

  (require '[utils.random :as r] '[forage.run :as fr] '[forage.experiment.fournier5 :as f5])

  (time (def fws (doall (repeatedly 360 #(fr/levy-run rng f5/look-fn nil f5/params 1.5))))) 
  (def sorted-fws (sort-by #(if (first %) 0 1) fws));; place runs with found foodspots first:

  ;; NOTE: Don't use trim-full-walk here: IT'S ALREADY CALLED in levy-foodwalk
    (time (fr/write-foodwalk-plots
            (str (System/getenv "HOME") "/docs/src/data.foraging/forage/fournier5mu" mu)
           :svg seed f5/env 800 9 3 1000 mu f5/params sorted-fws))


  "/src/data.foraging/forage/fournier5may2022/mu2successes13"
)

