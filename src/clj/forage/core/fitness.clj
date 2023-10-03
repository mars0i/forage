;; Functions for calculating various fitness measures and estimates
(ns forage.core.fitness
  (:require [fastmath.stats :as fstats]
            ;[clojure.math :as math :refer [cos sin tan atan2 sqrt round]]
            [utils.math :as um :refer [variance sample-variance]]))

;; The efficiency functions are very trivial functions. I define them for
;; the sake of clarity.
(defn efficiency
  "Returns the \"efficiency\" (Viswanathan et al. 1999) fitness, i.e. the
  number of targets found divided by the total distance traveled to find
  them.  This function can be used to calculate an individual (token
  organism ) fitness measure, but could also be applied to global
  population statistics."
  [n-found length]
  (/ n-found length))

(defn aggregate-efficiency
  "Returns the \"efficiency\" (Viswanathan et al. 1999) fitness, i.e. the
  number of targets found divided by the total distance traveled to find
  them, first summing the found-counts [often 0 or 1] and the lengths. This
  function can be used to calculate an individual (token organism ) fitness
  measure, but could also be applied to global population statistics.  If
  benefit-per and cost-per are provided, each the found sum is multiplied
  by benefit-per, and the length sum is multiplied by cost-per."
  ([found-counts lengths benefit-per cost-per]
   (efficiency (* benefit-per (reduce + found-counts))
               (* cost-per (reduce + lengths))))
  ([found-counts lengths]
   (aggregate-efficiency found-counts lengths 1 1)))

;; Note that adding a base fitness generally won't affect the ranking of
;; trait fitnesses.  cf. whatisfitness3.md in my notes.
(defn cost-benefit-fitness
  "Returns a fitness value that is the benefit-per of foodspots
  found minus the cost-per of cost-units:
  benefit-per * benefit-units - cost-per * cost-units.
  Example: benefit-units = targets found, cost-units = distance traveled.
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
        
