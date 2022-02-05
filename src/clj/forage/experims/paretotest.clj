(ns forage.paretotest
  (:require [utils.random :as r]
            [utils.math :as m]))

;; According to \citep{HormannEtAl:AutoNonuniformRandVarGen} theorem 2.1,
;; if you apply the CDF to a distribution to numbers so-distributed, the result
;; will be numbers uniformly-distributed, i.e. with the uniform CDF.
;;
;; So I assumed that if you apply a Pareto density function to numbers
;; generated according to that Pareto distribution, the result would be
;; uniformly-distributed numbers.  
;;
;; The experiments below seem to show that this is false, in that it's
;; not even the case that the mean of the resulting numbers need be near 1/2.

(def seed 123451234598765431)
(def rng (r/make-well19937 seed))

;; Sanity check: these means are near 0.5, as they should be:
(def uniform-nums (r/next-doubles rng))
(r/mean 10000 uniform-nums)
(r/mean 100000 uniform-nums)
(r/mean 1000000 uniform-nums)

;; These means are somewhere near 1/3:
(def dist (r/make-pareto rng 1 1)) ; mu=2, i.e. the optimal Levy dist
(def pareto-nums (r/next-doubles dist))
(def unif-from-pareto-nums (map (partial r/density dist) pareto-nums))
(r/mean 10000 unif-from-pareto-nums)
(r/mean 100000 unif-from-pareto-nums)
(r/mean 1000000 unif-from-pareto-nums)

;; These means are somewhere near 0.8:
(def dist (r/make-pareto rng 1 2)) ; mu=3, i.e. supposed to be Gaussian
(def pareto-nums (r/next-doubles dist))
(def unif-from-pareto-nums (map (partial r/density dist) pareto-nums))
(r/mean 10000 unif-from-pareto-nums)
(r/mean 100000 unif-from-pareto-nums)
(r/mean 1000000 unif-from-pareto-nums)

;; These means are smaller than 0.1:
(def dist (r/make-pareto rng 10 2)) ; change the scale parameter
(def pareto-nums (r/next-doubles dist))
(def unif-from-pareto-nums (map (partial r/density dist) pareto-nums))
(r/mean 10000 unif-from-pareto-nums)
(r/mean 100000 unif-from-pareto-nums)
(r/mean 1000000 unif-from-pareto-nums)
