;; This software is copyright 2021 by Marshall Abrams, and is distributed
;; under the Gnu General Public License version 3.0 as specified in the
;; the file LICENSE.

;; Functions for generating and using random numbers.
;; See e.g.
;; https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/RandomSource.html
(ns utils.random
  (:import MRG32k3a 
           MRG32k3aParetoSampler ; Apache Commons 1.5 InverseTransformParetoSampler hacked for use with MRG32k3a.
           [org.apache.commons.math3.distribution ParetoDistribution] ; 3.6.1
           [org.apache.commons.rng UniformRandomProvider] ; 1.5
           [org.apache.commons.rng.simple RandomSource] ; 1.5
           [org.apache.commons.rng.core RandomProviderDefaultState] ; 1.5
           [org.apache.commons.rng.core.source32 AbstractWell Well44497b Well19937c Well1024a] ; 1.5 [more PRNGS in org.apache.commons.rng.core.source64]
             ;; (There's also a Well512a, but results for it aren't reported in the L'Ecuyer and Simard TestU01 paper.)
           [org.apache.commons.rng.sampling ListSampler] ; 1.5
           [org.apache.commons.rng.sampling.distribution InverseTransformParetoSampler SamplerBase] ; 1.5
           ;[org.apache.commons.statistics.distribution ParetoDistribution] ; 1.5, I think, but not Mavenized yet
           [org.apache.commons.rng.sampling PermutationSampler]
           [java.io
            ByteArrayOutputStream ObjectOutputStream FileOutputStream
            ByteArrayInputStream  ObjectInputStream  FileInputStream]
           [java.util ArrayList])
  (:require ;[clojure.math.numeric-tower :as nt] ; now using clojure.math/pow instead of nt/expt see https://clojureverse.org/t/article-blog-post-etc-about-clojure-math-vs-numeric-tower/9805/6?u=mars0i
            [clojure.math :as math]
            [clojure.java.io :as io]
            [fastmath.core :as fm]))

;(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)
(fm/use-primitive-operators)

;; I started using a newer Apache Commons version, 1.4 and 1.5, because it
;; allowed saving internal state of a PRNG.  However, some functions
;; might only be available in 3.6.1.


;; NOTE
;; I have notes in a file named howManyRandomNumbersDoIneed.md that
;; explains why MRG32k3a has a long enough period for me (while SplitMix 
;; *might* not be OK).  All of the WELL generators, including 1024a, have
;; a long enough period, but MRG32k3a is a better generator, as well as 
;; being faster.

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
;; MOSTLY FROM APACHE COMMONS 1.5

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
  "Resets the seed of rng to seed.  Apparently doesn't work with Apache 1.5 RNGs."
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
  [n ^UniformRandomProvider rng] (dotimes [_ n] (.nextInt rng)))

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

(defn flush32k3a
  "Flush possible initial low-quality state from a PRNG with a 384-bit
  internal state such as an MRG32k3a. i.e. discard the first n numbers from
  a PRNG in order to flush out internal state that's might not be as random
  as what the PRNG is capable of."
  [^MRG32k3a rng]
  (dotimes [_ 1000] (.nextDouble rng)))

;; Re the additional nil argument to .create below, see
;; https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/RandomSource.html#create(java.lang.Object,java.lang.Object...)

(defn make-well1024
  "Make an Apache Commons WELL 1024a generator, flushing any possible 
  initial lack of entropy.  (Note that this is the default generator in
  Apache Commons used by distribution functions if no generator is passed.)"
  ([] (make-well1024 (make-seed)))
  ([^long long-seed] 
   (let [^UniformRandomProvider rng
         (.create RandomSource/WELL_1024_A long-seed nil)]
     (flush1024 rng)
     rng)))

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

;; On whether to flush the initial state:
;; Vigna's documentation and implmentation of MRG32k3a doesn't say anything
;; about flushing the initial state.  I don't think L'Ecuyer does either,
;; but I need to check.  The internal state is six longs, i.e. 384 bits.
;; Given a long seed, Vigna's MRG32k3a feeds that into a SplitMix to initialize
;; the six longs.  So the initial state is as random as that is, which is
;; probably OK.  But it's not a MRG32k3a state.  To be on the safe side,
;; I am going to flush by default; this might not be good if one were
;; creating rngs often, but I don't.  Since the state is only 384 bits,
;; flushing for 2*384 = 768 seems like more than enough.
(defn make-mrg32k3a
  ([] (make-mrg32k3a (make-seed)))
  ([^long long-seed]
   (let [^MRG32k3a rng (MRG32k3a. long-seed)]
     (flush32k3a rng)
     rng)))

(comment
  (require '[criterium.core :as crit])

  (def rng19937 (make-well19937 seed))
  (time (crit/bench (next-double rng19937)))
  ; Is there a cost to indirection through another function, and a protocol?
  (def rng19937 (make-well19937 seed)) ; reset the seed
  (time (crit/bench (.nextDouble rng19937)))
  ;; NO, in fact going through the protocol vastly improves the speed (by
  ;; about 5X or 6X !).
  ;; The efficiency of next-double can be recovered using the explicit 
  ;; .nextDouble by type-hinting the call *right here*, but using
  ;; apparently next-double removes that need.


  ;; The following are roughly in order of descreasting average time, i.e.
  ;; increasing speed.  The improvement is incremental from one to the
  ;; next.  MRG32k3a (11ns) is twice as fast as WELL 44497 (23ns), and 
  ;; about 60% faster than WELL 1024 (18ns).  There was one run of it,
  ;; though, where WELL 44497b was faster than WELL 19937c.
  (time
    (let [seed (make-seed)
          rng44497 (make-well44497 seed)
          rng19937 (make-well19937 seed)
          rng1024  (make-well1024 seed)
          rngMRG   (make-mrg32k3a seed)]
      (println "seed =" seed)
      (println "WELL44497b:")
      (time (crit/bench (next-double rng44497)))
      (println "WELL19937c:")
      (time (crit/bench (next-double rng19937)))
      (println "WELL1024a:")
      (time (crit/bench (next-double rng1024)))
      (println "MRG32k3a:")
      (time (crit/bench (next-double rngMRG)))))

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS FOR SAVING/RESTORING PRNG STATE

;; Designed to work with any class that has a saveState method.
(defn get-state
  "Returns the internal state of a PRNG."
  [rng]
  (.saveState rng))

;; Designed to work with any class that has a restoreState method.
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
                  (if (instance? org.apache.commons.rng.core.RandomProviderDefaultState state)
                    (.getState state)
                    state)) ; kludgey but this won't happen often
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
  (def state (get-state oldrng))
  (write-state "yowell.bin" state)
  (def oldnums [(.nextDouble oldrng) (.nextDouble oldrng) (.nextDouble oldrng)])
  (def newrng (make-well19937))
  (set-state newrng (read-state "yo.bin"))
  (def newnums [(.nextDouble newrng) (.nextDouble newrng) (.nextDouble newrng)])
  (= oldnums newnums)

  (def newrng (make-mrg32k3a))
  (def state (get-state newrng))
  (take 5 (repeatedly #(next-double newrng)))
  (set-state newrng state)
  (write-state "yomrg.bin" state)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISTRIBUTION FUNCTIONS

;; Note some of the methods are only described in interface RealDistribution.
;; https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/distribution/AbstractRealDistribution.html

;; TODO put this stuff into a protocol??
(defn make-apache-pareto
  "Returns an Apache Commons 1.5 Pareto distribution with min-value
  (\"scale\") parameter k and shape parameter alpha."
  [rng k alpha]
  (InverseTransformParetoSampler/of rng k alpha))

;; TODO put this stuff into a protocol??
(def make-pareto 
  "Alias for make-apache-pareto.  Returns an Apache Commons Pareto
  distribution with min-value (\"scale\") parameter k and shape parameter
  alpha."
  make-apache-pareto)

;; TODO put this stuff into a protocol??
(defn make-mrg32k3a-pareto
  "Returns an MRG32k3aParetoSampler pareto distribution based on rng which
  should be an MRG32k3a, and with min-value (\"scale\") parameter k and
  shape parameter alpha."
  [rng k alpha]
  (MRG32k3aParetoSampler/of rng k alpha))

(comment
  ;; Commons 1.5:
  (def wellpareto (make-apache-pareto (make-well19937) 1.0 1.0))
  (make-apache-pareto (make-well44497) 1.0 1.0)
  (make-apache-pareto (make-well1024) 1.0 1.0)
  (make-apache-pareto (make-mrg32k3a) 1.0 1.0) ; fails
  ;; Comments 3.6.1:
  (ParetoDistribution. (make-well19937) 1.0 1.0) ; fails because it's the wrong Well19937c class
  (ParetoDistribution. (make-mrg32k3a) 1.0 1.0) ; fails
  (def yo (ParetoDistribution.  1.0 1.0)) ; uses a Well19937c: https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/distribution/ParetoDistribution.html#ParetoDistribution(double,%20double)
  (.sample yo)
  (next-double yo) ; fails

  (next-double wellpareto)
  (def wps (repeatedly #(next-double wellpareto)))
  (take 100 wps)

  ;; The output doesn't look right. Where are the large values?
  (def mrg (make-mrg32k3a 1234))
  (def mrgpareto (make-mrg32k3a-pareto mrg 1.0 1.0))
  (next-double mrgpareto)
  (def ps (repeatedly #(next-double mrgpareto)))
  (take 200 ps)
)

(defn pareto
  "Given a value x from a uniformly distributed random number
  generator, returns a value from a pareto distribution with
  min-value (\"scale\") parameter k and shape parameter alpha."
  [^double k ^double alpha ^double x]
  (- 1 (/ (fm/pow k alpha)
          (fm/pow x alpha))))

;; TODO put this stuff into a protocol??
;; Note $\alpha + 1 = \mu = 2$ (i.e. (\alpha=1$) is the theoretical
;; optimum for searches with sparse targets.
(defn make-apache-powerlaw
  "Returns an Apache Commons Pareto distribution with min-value (\"scale\")
  parameter k and shape parameter alpha = mu - 1.  (i.e. this is a
  convenience wrapper to make it easier to think about and avoid mistakes
  with contexts where densities are expressed in the mu form.)"
  [rng k ^double mu] (make-pareto rng k (dec mu)))

;; TODO put this stuff into a protocol??
(def make-powerlaw
  "Alias for make-apache-powerlaw.  Returns an Apache Commons Pareto
  distribution with min-value (\"scale\") parameter k and shape parameter
  alpha = mu - 1.  (i.e. this is a convenience wrapper to make it easier to
  think about and avoid mistakes with contexts where densities are
  expressed in the mu form.)"
  make-apache-powerlaw)

;; TODO put this stuff into a protocol??
(defn make-mrg32k3a-powerlaw
  "Returns an MRG32k3aParetoSampler pareto distribution based on rng which
  is an MRG32k3a, with min-value (\"scale\") parameter k and shape
  parameter alpha = mu - 1.  (i.e. this is a convenience wrapper to make it
  easier to think about and avoid mistakes with contexts where densities
  are expressed in the mu form.)"
  [rng k ^double mu] (make-mrg32k3a-pareto rng k (dec mu)))

(comment
  ;; The output doesn't look right. Where are the large values?
  (def mrg (make-mrg32k3a 1234))
  (def mrgpower (make-mrg32k3a-powerlaw mrg 1.0 2.0))
  (next-double mrgpower)
  (def ps (repeatedly #(next-double mrgpower)))
  (take 200 ps)
)

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
    [this low high])
  )


;(defn next-int
;  ([rng] (.next rng)))

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
    ([this ^double low ^double high]
     (loop [x (.sample this)]
       (if (and (<= x high) (>= x low))
         x
         (recur (.sample this))))))

  MRG32k3aParetoSampler ; hacked version of InverseTransformParetoSampler
  (next-double
    ([this] (.sample this))
    ([this ^double low ^double high]
     (loop [x (.sample this)]
       (if (and (<= x high) (>= x low))
         x
         (recur (.sample this))))))

  ; PRNGS:
  Well1024a
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))

  Well19937c
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))

  Well44497b
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))

  MRG32k3a
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
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
  (* 2 Math/PI ^double (next-double rng)))

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
    (fn [dist ^double low ^double high ^double x]
      (cond (<= x low) 0.0  ; Or throw exception? Return nil?
            (> x high) 1.0
            :else (let [args [dist low high]
                        tot-prob (or (@memo$ args)
                                     (let [newprob (apply probability args)]
                                       (reset! memo$ {args newprob})
                                       newprob))]
                    (/ ^double (.cumulativeProbability dist x) ^double tot-prob))))))

;; Worked with old 1.3, not 1.4/1.5 (?)
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
  if provided, maximum value maxval.  Also can be understood as 
  transforming values generated by a power law distribution into uniformly
  distributed values."
  ([^double mu ^double minval ^double x] 
   (let [-alpha (- 1 mu)]
     (- 1 (/ (math/pow x -alpha)
             (math/pow minval -alpha)))))
  ([^double mu ^double minval ^double maxval ^double x]
   (let [-alpha (- 1 mu)
        minval-pow (math/pow minval -alpha)]
     (/ (- minval-pow (math/pow x -alpha))
        (- minval-pow (math/pow maxval -alpha))))))

(comment
  (powerlaw-cumulative 0.5285 16.18435699 2.1706 7.55453491) ; => 0.3989374083781279
)

;; There are other sampling functions in random-utils
(defn sample-from-coll
  "Returns num-samples elements randomly selected without replacement from
  collection xs.  NOTE creates a Java ArrayList, which might possibly add
  overhead if this is done often on small collections."
  [rng num-samples xs]
  (ListSampler/sample rng (ArrayList. xs) num-samples))

(comment
  (def rng (make-well19937 42))
  (def al (ArrayList. (range 10)))
  (ListSampler/sample rng (range 42) 4)
  (def asample (sample-from-coll rng 4 (range 100)))
  (class asample)
  (nth asample 2)
)

;; cf. clojure.core.shuffle, which uses a similar idea:
;; https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L7274
;; but doesn't allow specifying an RNG.  However, the java.util.Collections/shuffle
;; method does allow specifying an RNG.  However, it's a java.util.Random,
;; which is different from the Apache RNGs.
(defn shuffle
  "Given a random number generator rng and a collection xs, returns a
  collection (a java.util.ArrayList) in which the' elements have been
  randomly shuffled."
  [^UniformRandomProvider rng xs]
  (let [al (ArrayList. xs)] ; new java.util.ArrayList will be shuffled in place
    (ListSampler/shuffle rng al) ; returns nil, but al is now shuffled
    al)) ; a java.util.ArrayList can be passed to nth, seq, etc.

(comment
  (def rng (make-well19937 42))
  ;; boxed ints, I think (what ListSampler/shuffle wants):
  (def al (java.util.ArrayList. (range 10)))
  (class al)
  al
  (aset al 4 42) ; fails
  (.set al 4 42) ; works
  al
  (ListSampler/shuffle rng al)
  (shuffle rng (range 10))
  (shuffle rng (into #{} (range 10)))
  (shuffle rng (vec (range 10)))
  ;; unboxed, ints, I think:
  (def ra (into-array Integer/TYPE (range 10)))
  (class ra)
  ra
  (aset ra 4 42)
  ra
)

;; FIXME
;; See also https://commons.apache.org/proper/commons-rng/commons-rng-sampling/apidocs/org/apache/commons/rng/sampling/ListSampler.html
;; (class (java.util.ArrayList. (range 5)))
;; (ListSampler/shuffle rng (java.util.ArrayList. (range 5)))
;; I'm getting nil (i.e. Null) from that and from
;; PermutationSampler/shuffle, even on literal java int lists created
;; using PermutationSampler/natural.
;;
;; There's also clojure.core/shuffle, which converts the arg into a
;; java.util.ArrayList, then shuffles with java.util.Collections/shuffle,
;; then converts to a Clojure vector.  It's not clear what it uses.
;;
;; Fun fact: (cons 'a nil) produces a PersistentList, while (cons 'a ())
;; produces a Cons.  Clojure The Essential Reference says that these are
;; kind of the same, but PersistentLists can be more efficient e.g. with
;; 'count' and 'reduce'.
(defn shuffle-EH
  [^UniformRandomProvider rng xs]
  (let [idxs (PermutationSampler/shuffle rng (range (count xs)))
        xs-vec (into-array xs)]
    ;; now use indexes to rearrange sequence:
    (reduce (fn [acc i] (cons (xs-vec i) acc))
            nil idxs))) ; re nil see fun fact above
