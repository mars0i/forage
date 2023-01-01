;; This software is copyright 2021 by Marshall Abrams, and is distributed
;; under the Gnu General Public License version 3.0 as specified in the
;; the file LICENSE.

;; TODO check to see if there's reflection happening
;; TODO check if it's doing what it's supposed to
;; 

;; Functions for generating and using random numbers.
;; See e.g.
;; https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/RandomSource.html
(ns utils.random
  (:import [org.apache.commons.math3.distribution ParetoDistribution] ; 3.6.1
           [org.apache.commons.rng UniformRandomProvider] ; 1.4
           [org.apache.commons.rng.simple RandomSource] ; 1.4
           [org.apache.commons.rng.core RandomProviderDefaultState] ; 1.4
           [org.apache.commons.rng.core.source32 AbstractWell Well19937c Well44497b] ; 1.4
           [org.apache.commons.rng.sampling ListSampler] ; 1.4
           [org.apache.commons.rng.sampling.distribution InverseTransformParetoSampler SamplerBase] ; 1.4
           ;[org.apache.commons.statistics.distribution ParetoDistribution] ; 1.4, I think, but not Mavenized yet
           [java.io
            ByteArrayOutputStream ObjectOutputStream FileOutputStream
            ByteArrayInputStream  ObjectInputStream  FileInputStream])
  (:require [clojure.math.numeric-tower :as nt]
            [clojure.java.io :as io]))

;; NOTE Apache Commons Math 3.6.1 is latest official, but I started using
;; the newer version, 1.4, because it allowed saving internal state of
;; a PRNG.  However, some functions aren't yet realeased with 1.4, so
;; I'm using 3.6.1 now, too.

(comment
  (def rng (make-well19937 42))
  (class rng)
  (isa? Well19937c AbstractWell)
  (isa? Well19937c UniformRandomProvider)
  (isa? Well19937c org.apache.commons.rng.core.BaseProvider)
  (isa? Well19937c org.apache.commons.rng.core.source32.IntProvider)
  (def state (.getStateInternal rng)) ; fails
  (class state)
  (.setStateInternal rng state)
  (.nextDouble rng)
)

;; (These are mostly wrappers for Java library stuff, and in some cases
;; one could just as easily use the Java methods directly with
;; (.javaMethod instance arguments)
;; However, I prefer to have a pure Clojure interface, partly so to
;; facility passing the methods as functions to e.g. 'map'.)
;; (Also, in future versions of Apache Commons--used a lot below--the 
;; randon number and distribution functionality is being moved elsewhere,
;; apparently.  Some is in a package RNG.  I'm not sure where the rest is.
;; So all the more reason to have wrappers.)

;; NOTES ON WELL GENERATORS
;; See esp. table II on page 9 in the original article Panneton, L'Ecuyer,
;; and Matsumoto, "Improved Long-period Generators Based on Linear 
;; Recurrences Modulo 2", ACM Transactions on Mathematical Software 2006.
;; The period is 2^{number in name of generator} - 1
;; The word size and output size is always w = 32 bits.
;; The size of the internal state is r words:
;; r = 32 for Well1024a; r = 624 for Well19937's; r = 1391 for Well44497's.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRNG-CREATION FUNCTIONS
;; MOSTLY FROM APACHE COMMONS 1.4

;; Similar to what Apache Commons PRNGs do if unseeded.
;; Note that the Apache Commons PRNGs will use all of a long seed--
;; it's split into two ints, and those are the first to entries in the
;; array of ints that is the real internal seed.  Luke's MersenneTwisterFast,
;; by contrast, will only use the first 32 bits of a long seed, as if
;; it was an int.
(defn my-make-seed
  "Return a long constructed from semi-arbitrary things such as the
  system time and the hash identity of a newly created Java object."
  [] 
  (let [t (System/currentTimeMillis)           ; long
        h (System/identityHashCode (Object.))] ; int
    (+ t h)))

(defn make-seed
  "Return a long constructed by an Apache Commons method."
  []
  (RandomSource/createLong))

(defn set-seed
  "Resets the seed of rng to seed."
  [rng seed]
  (.setSeed rng seed))

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

;; Re the additional nil argument to .create below, see 
;; https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/RandomSource.html#create(java.lang.Object,java.lang.Object...)
(defn make-well19937
  "Make an Apache Commons WELL 19937c generator, flushing any possible 
  initial lack of entropy.  (Note that this is the default generator in
  Apache Commons used by distribution functions if no generator is passed.)"
  ([] (make-well19937 (make-seed)))
  ([^long long-seed] 
   (let [^UniformRandomProvider rng
         (.create RandomSource/WELL_19937_C long-seed nil)]
     (flush19937 rng)
     rng)))

(defn make-well44497
  "Make an Apache Commons WELL 44497b generator, flushing any possible 
  initial lack of entropy."
  ([] (make-well44497 (make-seed)))
  ([^long long-seed] 
   (let [^UniformRandomProvider rng
         (.create RandomSource/WELL_44497_B long-seed nil)]
     (flush44497 rng)
     rng))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS FOR SAVING/RESTORING PRNG STATE

(defn get-state
  "Returns the internal state of a PRNG."
  [rng]
  (.saveState rng))

(defn set-state
  "Sets the internal state of a PRNG to a state derived from a PRNG
  of the same kind."
  [rng state]
  (.restoreState rng state))

;; Java i/o voodoo below is based on an example at:
;; https://commons.apache.org/proper/commons-rng/userguide/rng.html#a2._Usage_overview
;; It's buried in the "Usage overview" section.  Search for "Serializable".
;; I tried to do something more straightforward using more Clojure primitives,
;; but it didn't work.  Perhaps there are ways to simplify, but this works.

;; File size should be 2535 for Well19937, and 5603 for Well44497.
(defn write-state
  "Write state from a single PRNG to a file."
  [filename state]
  (let [byte-stream (ByteArrayOutputStream.)]
    (.writeObject (ObjectOutputStream. byte-stream)
                  (.getState state))
    (with-open [w (FileOutputStream. filename)]
      (.write w (.toByteArray byte-stream)))))

(defn read-state
  "Read state for a single PRNG from a file."
  [filename]
  (with-open [r (FileInputStream. filename)]
    (RandomProviderDefaultState.
      (.readObject (ObjectInputStream. r)))))

(comment
  ;; Test:
  (def oldrng (make-well19937 123456789))
  (write-state "yo.bin" (get-state oldrng))
  (def oldnums [(.nextDouble oldrng) (.nextDouble oldrng) (.nextDouble oldrng)])
  (def newrng (make-well19937)) ; seed is irrelevant. Wastefully flushing initial state.
  (set-state newrng (read-state "yo.bin"))
  (def newnums [(.nextDouble newrng) (.nextDouble newrng) (.nextDouble newrng)])
  (= oldnums newnums)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISTRIBUTION FUNCTIONS

;; Note some of the methods are only described in interface RealDistribution.
;; https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/distribution/AbstractRealDistribution.html

(defn make-pareto
  "Returns an Apache Commons 1.4 Pareto distribution with min-value (\"scale\")
  parameter k and shape parameter alpha."
  [rng k alpha]
  (InverseTransformParetoSampler/of rng k alpha))

(comment
  (def dist (InverseTransformParetoSampler/of rng 1 2))
  (.sample dist)
)

;; Note $\alpha + 1 = \mu = 2$ (i.e. (\alpha=1$) is the theoretical
;; optimum for searches with sparse targets.
(defn make-powerlaw
  "Returns an Apache Commons Pareto distribution with min-value (\"scale\")
  parameter k and shape parameter alpha = mu - 1.  (i.e. this is a
  convenience wrapper to make it easier to think about and avoid mistakes
  with contexts where densities are expressed in the mu form.) Without an
  initial PRNG argument rng, uses Well19937c with an internally generated
  seed."
  [rng k mu] (make-pareto rng k (dec mu)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERATOR AND DISTRIBUTION ACCESS FUNCTIONS
;; These are collected together, sometimes in a protocol, because
;; a PRNG implements a uniform distribution, so in sense, a PRNG
;; and a distribution object have the same functionality, though the
;; methods might have different names.

(defprotocol RandDist
  "Provides a common interface to some functionality shared by Math3 PRNG 
  and distribution classes.  If low and high are provided, numbers outside
  this range (inclusive) are rejected."
  (next-double 
    [this]
    [this low high]))


(defn next-int
  ([rng] (.next rng)))

;; Apparently, the specializers have to be concrete classes; interfaces and 
;; abstract classes don't seem to work.  Too bad--it would save duplication.
;; (Note that when truncating, I test the high limit first because that's
;; the constraint that a distribution is most likely to violate in my code,
;; and since 'and' short-circuits.)
(extend-protocol RandDist
  ; DISTRIBUTIONS:
  InverseTransformParetoSampler
    (next-double
      ([this] (.sample this))
      ([this low high] (loop [x (.sample this)]
                             (if (and (<= x high) (>= x low))
                               x
                               (recur (.sample this))))))

  ; PRNGS:
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
                               (recur (.nextDouble this)))))))

(defn next-double-fn
  "Rather than returning the result of '(next-double dist)' or 
  '(next-double dist low high)', returns a function of no argujents,
  which when called, returns the next double from dist, which may be a 
  PRNG, in which case it's a uniform distribution.  (This function might
  be useful e.g. for passing to 'repeatedly'.)"
  ([dist] (fn [] (next-double dist)))
  ([dist low high] (fn [] (next-double dist low high))))

;; Don't name this 'doubles'; that's a Clojure built-in.
;; Not including the (repeatedly n f) form, because that would make
;; multiple arities confusing.  I can use 'take' instead.
(defn next-doubles "Returns a lazy infinite sequence of random doubles from distribution dist, which may be a PRNG, in which case it's a uniform distribution."
  ([dist] (repeatedly (next-double-fn dist)))
  ([dist low high] (repeatedly (next-double-fn dist low high))))

(defn next-radian
  "Given a PRNG prng, return a uniformly distributed number between 0
  and pi, i.e. in [0,pi)."
  [rng]
  (* 2 Math/PI (next-double rng)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE THE FOLLOWING WERE WRITTEN FOR APACHE COMMONS MATH 3.6.1
;; Probably don't work with 1.4.
;; 

;; Notes on Apache Commons Math 3.6.1 methods:
;; cumulativeProbablity:
;;   cumulativeProbability(x) does what you think.
;;   cumulativeProbability(x, y) returns 
;;     cumulativeProbability(y) - cumulativeProbability(x).  However, 
;;     the name is deprecated, and in fact cumulativeProbability(x,y) just
;;     calls probability(x,y), which does the subtraction.
;; probability:
;;   probability(x,y) returns the probability of a value 
;;     falling in the interval (x,y], i.e. 
;;     cumulativeProbability(y) - cumulativeProbability(x).
;;   probability(x) just returns zero for continuous distributions,
;;     since the probability of a point is zero.
;; density:
;;   density(x) returns the value of the pdf at x, which is probably
;;     what you wanted if you called probablity(x).

(defn density
  "Return the density at x according to (Apache Commons Math3) distribution dist."
  [dist x]
  (.density dist x))

(defn probability
  "Return the probability that a value from Apache Commons Math3 dist falls
  within (low,high]."
  [dist low high]
   (.probability dist low high))

;; FIXME
;; Easiest to keep this as a separate definition that can be called
;; via arity-selection from cumulative.  This makes it easy to memoize
;; using a closure.  Note that it's still a slightly faster to use this
;; function directly rather than calling the multi-arity cumulative.
(def trunc-cumulative
  "Return the value of the cumulative probability distribution for Apache
  Commons Math3 dist at x, where values outside (low, high] have zero
  probability.  Memoizes the normalizing value, but only if the first three
  arguments are the same as on the previous call: dist must be identical? to
  its previous value, while low and high each must be = to their previous
  values."
  (let [memo$ (atom {})]
    (fn [dist low high x]
      (cond (<= x low) 0.0  ; Or throw exception? Return nil?
            (> x high) 1.0
            :else (let [args [dist low high]
                        tot-prob (or (@memo$ args)
                                     (let [newprob (apply probability args)]
                                       (reset! memo$ {args newprob})
                                       newprob))]
                    (/ (.cumulativeProbability dist x) tot-prob))))))

;; Worked with old 1.3, not 1.4 (?)
;(defn cumulative
;  "Return the value of the cumulative probability distribution at x for
;  (Apache Commons Math3) distribution dist.  If low and high are provided,
;  returns the the cumulative probability for the truncated distribution 
;  corresponding to for x in (low,high] but assigning zero probability to 
;  values outside of it."
;  ([dist x] (.cumulativeProbability dist x))
;  ([dist low high x] (trunc-cumulative dist low high x)))

(defn powerlaw-cumulative
  "Given an input value x, returns the cumulative probability of x for
  a power law distribution with exponent mu, minimum value minval, and
  if provided, maximum value maxval.  Also transforms values generated 
  by a power law distribution into uniformly distributed values."
  ([mu minval x] 
   (let [-alpha (- 1 mu)]
     (- 1 (/ (nt/expt x -alpha)
             (nt/expt minval -alpha)))))
  ([mu minval maxval x]
   (let [-alpha (- 1 mu)
        minval-pow (nt/expt minval -alpha)]
     (/ (- minval-pow (nt/expt x -alpha))
        (- minval-pow (nt/expt maxval -alpha))))))

(comment
  (powerlaw-cumulative 0.5285 16.18435699 2.1706 7.55453491) ; => 0.3989374083781279
)

(defn sample-from-coll
  "Returns num-samples elements randomly selected without replacement 
  from collection xs."
  [rng xs num-samples]
  (ListSampler/sample rng xs num-samples))

(comment
  (def rng (make-well19937 42))
  (sample-from-coll rng (range 50) 5)
)
