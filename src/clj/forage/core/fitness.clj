;; Functions for calculating various fitness measures and estimates
(ns forage.core.fitness
  (:require [fastmath.stats :as fstats]
            ;[clojure.math :as math :refer [cos sin tan atan2 sqrt round]]
            [utils.math :as um :refer [variance sample-variance]]))

;; Note that currently, efficiency as a fitness measure is defined in
;; run.clj.  Should maybe be moved here?  Eh.  But maybe added here.

(defn cost-benefit-fitness
  "Returns a fitness value that is the benefit-per of foodspots
  found minus the cost-per of cost-units:
  benefit-per * benefit-units - cost-per * cost-units.
  Example:
  benefit-units = targets found, cost-units = distance traveled.
  If a constant base-fitness is passed in, it will be added to the
  result of the above calculation.  This function can be used to 
  calculate an individual (token organism ) fitness measure, but 
  could also be applied to global population statistics."
  ([benefit-per cost-per benefit-units cost-units]
   (- (* benefit-per benefit-units) (* cost-per cost-units)))
  ([base-fitness benefit-per cost-per benefit-units cost-units]
   (+ base-fitness 
      (cost-benefit-fitness benefit-per cost-per benefit-units cost-units))))


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
        
