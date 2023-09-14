;; Functions for calculating various fitness measures and estimates
(ns forage.core.fitness
  (:require [fastmath.stats :as fstats]
            ;[clojure.math :as math :refer [cos sin tan atan2 sqrt round]]
            [utils.math :as um :refer [variance sample-variance]]))

;; Note that currently, efficiency as a fitness measure is defined in
;; run.clj.  Should maybe be moved here?  Eh.  But maybe added here.

(defn food-and-distance-fitness
  "Returns a fitness value that is the benefit of foodspots found minus the
  cost of distance traveled while finding them: benefit-per-foodspot *
  num-foodspots - cost-per-distance * distance. If a constant base-fitness
  is passed in, it's added to the result of the above calculation.  This
  can be used as an individual (token organism ) fitness measure, but could
  also be applied to global population statistics."
  ([benefit-per-foodspot cost-per-distance num-foodspots distance]
   (food-and-distance-fitness 0 benefit-per-foodspot cost-per-distance num-foodspots distance))
  ([base-fitness benefit-per-foodspot cost-per-distance num-foodspots distance]
   (+ base-fitness (- (* benefit-per-foodspot num-foodspots)
                      (* cost-per-distance distance)))))


;; There is some redundancy in that I need pop-size, but the mean and var
;; functions will calculate them as well.  However, as long as I'm not
;; passing in lists, this shouldn't have an impact on speed.
(defn gillespie-dev-stoch-fitness
  "Calculate (Gillespie 1977)'s \"developmental stochasticity\"
  fitness estimate mean - variance/N for an entire theoretical
  population--i.e. uses regular population variance, not the
  sample variance."
  [indiv-fitnesses]
  (let [pop-size (count indiv-fitnesses)
        fitn-mean (fstats/mean indiv-fitnesses)
        fitn-var (um/variance indiv-fitnesses fitn-mean)]
    (- fitn-mean (/ fitn-var pop-size))))
        

(defn sample-gillespie-dev-stoch-fitness
  "Calculate (Gillespie 1977)'s \"developmental stochasticity\"
  fitness estimate mean - variance/N using the sample variance."
  [indiv-fitnesses]
  (let [pop-size (count indiv-fitnesses)
        fitn-mean (fstats/mean indiv-fitnesses)
        fitn-var (um/sample-variance indiv-fitnesses fitn-mean)]
    (- fitn-mean (/ fitn-var pop-size))))
        
