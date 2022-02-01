;; This software is copyright 2016 by Marshall Abrams, and is distributed
;; under the Gnu General Public License version 3.0 as specified in the
;; the file LICENSE.

(ns utils.random
  (:import [ec.util MersenneTwisterFast]    ; https://cs.gmu.edu/~sean/research/mersenne/ec/util/MersenneTwisterFast.html
           [org.apache.commons.math3.random ; https://commons.apache.org/proper/commons-math
            MersenneTwister Well1024a Well19937c Well44497b]
           [org.apache.commons.math3.distribution 
            LevyDistribution NormalDistribution ParetoDistribution])
  (:require [clojure.math.numeric-tower :as nt]))

;; For the Apache commons PRNGs, most of the functions are documented in
;; AbstractRandomGenerator or RandomGenerator.  Most of the same functions
;; are also in Sean Luke's MersenneTwisterFast and MersenneTwister, which 
;; also include a few other signatures for the same names.   For example,
;; both Luke's MT and the Apache PRNGs include nextInt(), nextLong(), 
;; nextDouble(), and nextGaussian().

;; Note that sample code in the apache.commons.math3 manual page on
;; random number generation seems to be obsolete and won't compile
;; with 404'ed javadoc links.

;; TODO For uniform numbers, use a RandomDataGenerator to normalize to
;; a range.

(declare flush-rng make-int-seed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNG-CREATION FUNCTIONS

(defonce seed-increment (atom 0))

;; TODO Is this what I want??
(defn make-int-seed
  [] 
  (let [t (System/currentTimeMillis)
        r (rand-int Integer/MAX_VALUE)
        i (swap! seed-increment inc)]
    (int (+ i ; protect against millisecond redundancy
            (nt/round (* i (/ t r)))))))


;; NOTE: I've decided to flush some initial state from WELL generators,
;; and not only from MersenneTwisters, even though unlike MersenneTwisters, 
;; the WELL code doesn't seem to begin by simply taking numbers from whatever 
;; happens to be in the initial state after you initialized it.  This is 
;; very possibly overkill for WELL genertors, but maybe there's
;; some lower-quality initial effect in the WELL generators, too--I don't 
;; know--and it doesn't hurt much to throw away some numbers as long as 
;; you're not generating a lot of PRNGs, which generally wouldn' be good.

(defn flush-rng
  "Discard the first n numbers from a PRNG in order to flush out internal 
  state that's might not be as random as what the PRNG is capable of.
  cf.  https://listserv.gmu.edu/cgi-bin/wa?A1=ind1609&L=MASON-INTEREST-L#1 ."
  [n rng] (dotimes [_ n] (.nextInt rng)))

(def flush1024
  "Flush possible initial low-quality state from a PRNG with a 32-word
  internal state such as a Well1024a."
  (partial flush-rng 100))

(def flush19937
  "Flush possible initial low-quality state from a PRNG with a 624-word 
  internal state such as a WEll19937 or MT19937."
  (partial flush-rng 2000))

(def flush44497
  "Flush possible initial low-quality state from a PRNG with a 1391-word
  internal state such as a WELL44497."
  (partial flush-rng 6000))

;; NOTES ON WELL GENERATORS
;; See esp. table II on page 9 in the original article Panneton, L'Ecuyer,
;; and Matsumoto, "Improved Long-period Generators Based on Linear 
;; Recurrences Modulo 2", ACM Transactions on Mathematical Software 2006.
;; The period is 2^{number in name of generator} - 1
;; The word size and output size is always w = 32 bits.
;; The size of the internal state is r words:
;; r = 32 for Well1024a; r = 624 for Well19937's; r = 1391 for Well44497's.

(defn make-well1024
  "Make an Apache Commons WELL 1024a generator, flushing any possible 
  initial lack of entropy."
  ([] (make-well1024 (make-int-seed)))
  ([long-seed] 
   (let [rng (Well1024a. long-seed)]
     (flush1024 rng)
     rng))) 

(defn make-well19937
  "Make an Apache Commons WELL 19937c generator, flushing any possible 
  initial lack of entropy.  (Note that this is the default generator in
  Apache Commons used by distribution functions if no generator is passed.)"
  ([] (make-well19937 (make-int-seed)))
  ([long-seed] 
   (let [rng (Well19937c. long-seed)]
     (flush19937 rng)
     rng))) 

(defn make-well44497
  "Make an Apache Commons WELL 44497b generator, flushing any possible 
  initial lack of entropy."
  ([] (make-well44497 (make-int-seed)))
  ([long-seed] 
   (let [rng (Well44497b. long-seed)]
     (flush44497 rng)
     rng))) 

(defn make-twister
  "Make an instance of Apache Commons MersenneTwister generator, flushing
  any possible initial lack of entropy."
  ([] (make-twister (make-int-seed)))
  ([long-seed] 
   (let [rng (org.apache.commons.math3.random.MersenneTwister. long-seed)]
     (flush19937 rng)
     rng))) 

;; Note this won't work with Apache distribution classes
(defn make-luke-twister
  "Make an instance of Sean Luke's MersenneTwisterFast, flushing any possible 
  initial lack of entropy."
  ([] (make-luke-twister (make-int-seed)))
  ([long-seed] 
   (let [rng (ec.util.MersenneTwisterFast. long-seed)]
     (flush19937 rng)
     rng))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISTRIBUTION FUNCTIONS

;; Note some of the methods are only described in interface RealDistribution.
;; https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/distribution/AbstractRealDistribution.html

(defn make-gaussian
  "Returns an Apache Commons normal distribution with shift parameter mean
  and shape parameter sd (standard deviation).  Without mean and sd arguments,
  these are set to 0 an 1, respectively.  Without an initial PRNG argument,
  uses Well19937c with an internally generated seed."
  ([] (NormalDistribution.))
  ([mean sd] (NormalDistribution. mean sd))
  ([rng mean sd] (NormalDistribution. rng mean sd NormalDistribution/DEFAULT_INVERSE_ABSOLUTE_ACCURACY)))
  ;; Last arg above is inverse cumulative probability accuracy.
  ;; There's supposed to be a constructor just with rng, mean, and sd, but
  ;; it inexplicably doesn't seem to exist, despite javadoc and source code.

(defn make-levy
  "Returns an Apache Commons Lévy distribution with shift parameter mu and
  shape parameter c.  Without an initial PRNG argument, uses Well19937c with
  an internally generated seed.  (Note that this is just a specific
  version of the general Lévy distribution.)"
  ([mu c] (LevyDistribution. mu c))
  ([rng mu c] (LevyDistribution. rng mu c)))

(defn make-pareto
  "Returns an Apache Commons Pareto distribution with scale parameter
  k and shape parameter alpha.  Without an initial PRNG argument rng,
  uses Well19937c with an internally generated seed."
  ([k alpha] (ParetoDistribution. k alpha))
  ([rng k alpha] (ParetoDistribution. rng k alpha NormalDistribution/DEFAULT_INVERSE_ABSOLUTE_ACCURACY)))


;; Note $\alpha + 1 = \mu = 2$ (i.e. (\alpha=1$) is the theoretical
;; optimum for searches with sparse targets.
(defn make-powerlaw
  "Returns an Apache Commons Pareto distribution with scale parameter
  k and shape parameter alpha = mu - 1.  (i.e. this is a convenience 
  wrapper to make it easier to think about and avoid mistakes with contexts
  where densities are expressed in the mu form.) Without an initial PRNG 
  argument rng, uses Well19937c with an internally generated seed."
  ([k mu] (make-pareto k (dec mu)))
  ([rng k mu] (make-pareto rng k (dec mu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERATOR AND DISTRIBUTION ACCESS FUNCTIONS
;; These are collected together, sometimes in a protocol, because
;; a PRNG implements a uniform distribution, so in sense, a PRNG
;; and a distribution object have the same functionality, though the
;; methods might have different names.

(defn density
  "Return the density at x according to distribution dist."
  [dist x]
  (.density dist x))

(defn next-radian
  "Given a PRNG prng, return a uniformly distributed number between 0
  and pi, i.e. in [0,pi)."
  [rng]
  (* 2 Math/PI (.nextDouble rng)))

(defprotocol RandDist
  "Provides a common interface to some functionality shared by PRNG 
  and distribution classes.  next-double methods return the next
  double distributed according to the instance's class.  If arguments
  low and high are included the results will be truncated to fall within
  [low, high]."
  (next-double [this]
               [this low high]))

;; Apparently, the specializers have to be concrete classes; interfaces and 
;; abstract classes don't seem to work.  Too bad--it would save duplication.
;; (Note that when truncating, I test the high limit first because that's
;; the constraint that a distribution is most likely to violate in foond,
;; since 'and' short-circuits.)
(extend-protocol RandDist
  ; DISTRIBUTIONS:
  ParetoDistribution 
    (next-double
      ([this] (.sample this))
      ([this low high] (loop [x (.sample this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.sample this))))))
  NormalDistribution 
    (next-double
      ([this] (.sample this))
      ([this low high] (loop [x (.sample this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.sample this))))))
  LevyDistribution
    (next-double
      ([this] (.sample this))
      ([this low high] (loop [x (.sample this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.sample this))))))
  ; PRNGS:
  Well1024a 
    (next-double
      ([this] (.nextDouble this))
      ([this low high] (loop [x (.nextDouble this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.nextDouble this))))))
  Well19937c
    (next-double
      ([this] (.nextDouble this))
      ([this low high] (loop [x (.nextDouble this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.nextDouble this))))))
  Well44497b
    (next-double
      ([this] (.nextDouble this))
      ([this low high] (loop [x (.nextDouble this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.nextDouble this))))))
  org.apache.commons.math3.random.MersenneTwister 
    (next-double
      ([this] (.nextDouble this))
      ([this low high] (loop [x (.nextDouble this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.nextDouble this))))))
  ec.util.MersenneTwisterFast
    (next-double
      ([this] (.nextDouble this))
      ([this low high] (loop [x (.nextDouble this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.nextDouble this))))))
    )
