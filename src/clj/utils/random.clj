;; This software is copyright 2021 by Marshall Abrams, and is distributed
;; under the Gnu General Public License version 3.0 as specified in the
;; the file LICENSE.

;; Functions for generating and using random numbers.
;; See e.g.
;; https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/RandomSource.html
(ns utils.random
  (:import MRG32k3a ; Sebastiano Vigna's version
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
           [java.util ArrayList]
           [clojure.lang IFn$DD]
           )
  (:require ;[clojure.math.numeric-tower :as nt] ; now using clojure.math/pow instead of nt/expt see https://clojureverse.org/t/article-blog-post-etc-about-clojure-math-vs-numeric-tower/9805/6?u=mars0i
            [clojure.core :as cc] ; to override use functions that fastmath replaced with macros
            [clojure.math :as math]
            [clojure.java.io :as io]
            [utils.math :as um]
            [fastmath.core :as fm]
            ;[ham-fisted.hlet :as hfl]
            [uncomplicate.neanderthal
             [core :as nc]
             [native :as nn]
             [random :as nran]
             [real :as nreal]]
            [uncomplicate.fluokitten.core :as ktn]))

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
;; being faster.  This function automates the calculates in that file.
(defn vigna-log-prob
  "Returns the log base 2 of (nums-per-run^2 runs)/period, which is
  Sebastiano Vigna's estimate of the probability of overlap of random
  sequences of length nums-per-run in runs number of experiments, using a
  generator with the given period, on the assumption that the initial seed
  is chosen with uniform probability.  log-period should be the log base 2
  of the period.  The result should be, ideally, a negative number with
  large absolute value, indicating that the probability of overlap is very
  small. (See Vigna's \"On the probability of overlap of random
  subsequences of pseudorandom number generators\", _Information Processing
  Letters_ 2020,
  https://www.sciencedirect.com/science/article/abs/pii/S0020019020300260.)"
  [^long nums-per-run ^long runs ^long log-period]
  (let [log-len (um/log2 nums-per-run)    ; log 2 of number L of runs
        log-runs-sq (* 2 (um/log2 runs))] ; log 2 of square of number n of runs
    (- (+ log-len log-runs-sq)
       log-period)))

(comment
  ;; Estimate of probability of overlap for my spiral28 experiments
  ;; if I use a different seed for each walk and env configuration.
  ;; (There happen to be 28 of them, but that's not why it's called
  ;; spiral28.)
  (def spiral28-vigna (partial vigna-log-prob 100000000000 28))
  ;; MRG32k3a:
  (spiral28-vigna 191) ;=> -9.999999998003778E10
  ;; i.e. about 1/2^10000000000
  ;; ARS5 in Neanderthal:
  (spiral28-vigna 130) ;=> -83.8440811121238
  ;; i.e. about 1/2^84
  ;; cf https://dragan.rocks/articles/19/Billion-random-numbers-blink-eye-Clojure
)

;; SEE tips/neanderthal.clj for examples of how to do things with 
;; Neanderthal random number generating.


;; STRUCTURE AND FNS FOR EXTRACTING RANDOM NUMBERS USING NEANDERTHAL
;; An option is to not store the buflen since I can get it from Neanderthal easily using: (buf)
(defn make-nums
  [rng buflen]
  {:rng rng
   :len buflen
   :buf (nn/dv buflen)        ; Generate the random numbers later; see NeadnerthalRandonmNumbers1.md, Algorithm A.
   :nexti$ (atom buflen)}) ; Read index starts one past end of buffer; see NeadnerthalRandonmNumbers1.md, Algorithm A.

(defrecord NeanRandNums [rng len buf nexti$])

(defn make-nean-nums
  [rng buflen]
  (NeanRandNums. rng buflen (nn/dv buflen) (atom buflen)))


;; NOTE Neanderthal's copy! can't be used to copy from a vector to itself because
;; there's an explicit test for identity in the source code.  So this won't
;; work: (nc/copy! nv nv to-shift-start num-to-shift 0).  However, subvector
;; lets you provide side-effects on a vector via a window onto it.


;; THIS SHOULD WORK BOTH ON A BARE MAP AND ON A NeanRandNums (which is better for defprotocol).
;;
;; See NeadnerthalRandonmNumbers1.md, Algorithm A.
;; When number of randnums needed is > buflen, an option would be to
;; call this repeatedly until enough numbers have been generated.
(defn take-rand!
  "Returns a Neanderthal vector containing n unused random numbers from a
  random numbers structure nums created by make-nums.  New random numbers
  will be created to refill the buffer in nums as needed.  n must be <=
  length len of the buffer in nums."
  [^long n nums]
  (let [{:keys [buf ^long len nexti$]} nums ; nums is structure with Neandertal vec of rand numbers in a buffer buf of length len and an index nexti
        ^long nexti @nexti$
        dist-to-end (- len nexti)]
    (when (> n len) (throw (Exception. (str "take-rand!!: requested number of random numbers, " n ", is larger than buf size, " len "."))))
    (let [new-nexti (if (<= n dist-to-end)
                      nexti ; we can just pull the next n numbers from buffer
                      (do (nc/copy! (nc/subvector buf nexti dist-to-end) ; copy last rand numbers
                                    (nc/subvector buf 0 dist-to-end))    ; to the front
                          (nran/rand-uniform! (:rng nums) (nc/subvector buf dist-to-end nexti)) ; replace nums copied [dist-to-end is now idx of end of copied nums, nexti is len of rest of buf]
                          0))] ; now the unused numbers from end are at front
      (reset! nexti$ (+ new-nexti n)) ; since now nexti + n is < len, this will not be > len
      (nc/subvector buf new-nexti n))))

(comment
  ;; Usage examples:
  (def nums (make-nums (nran/rng-state nn/native-double 101) 200)) ; nums is a Clojure map
  (def nums (make-nean-nums (nran/rng-state nn/native-double 101) 200)) ; nums is a NeanRandNums
  (class nums)
  (:buf nums)        ; uncomplicate.neanderthal.internal.host.buffer_block.RealBlockVector
  (take-rand! 4 nums) ; uncomplicate.neanderthal.internal.host.buffer_block.RealBlockVector
  (take 4 (take-rand! 4 nums))    ; clojure.lang.LazySeq
  (map cc/inc (take-rand! 4 nums))    ; clojure.lang.LazySeq
  (for [x (take-rand! 4 nums)] x) ; clojure.lang.LazySeq
  (vec (take-rand! 4 nums)) ; fails
  (into [] (take-rand! 4 nums))  ; clojure.lang.PersistentVector
  (mapv cc/inc (take-rand! 4 nums)) ; clojure.lang.PersistentVector
  (cons 1.0 (take-rand! 4 nums))        ; clojure.lang.Cons
  (next (cons 1.0 (take-rand! 4 nums))) ; clojure.lang.Cons
  (rest (cons 1.0 (take-rand! 4 nums))) ; clojure.lang.LazySeq
  (conj (take-rand! 4 nums) 1.0) ; fails

  ;; How to access elements?
  (def nums (make-nean-nums (nran/rng-state nn/native-double 101) 200))
  (def fournums (take-rand! 4 nums))
  (nth fournums 0) ; fails
  (aget fournums 0) ; fails
  (first fournums) ; succeeds
  (second fournums) ; succeeds
  (class (nc/entry fournums 0))    ; succeeds, boxed
  (class (nreal/entry fournums 0)) ; succeeds, supposed to be unboxed

  ;; Fluokitten's fmap vs fmap! :

  (def nums (make-nums (nran/rng-state nn/native-double 22) 11))

  (into [] (:buf nums)) ; all zeros
  (def fiveunif1 (take-rand! 5 nums))
  (into [] (:buf nums)) ; now all random, with first five 
  (into [] fiveunif1)   ; also in fiveunif1
  (def fivepareto1 (ktn/fmap (partial uniform-to-pareto-precalc 1 1) fiveunif1))
  (into [] fivepareto1) ; fiveunif1 and (:buf nums) are unchanged: fmap is purely functional

  (def fiveunif2 (take-rand! 5 nums)) ; gets next five numbers from (:buf nums)
  (into [] fiveunif2)
  (into [] (:buf nums)) ; buf is unchanged at this point
  (def fivepareto2 (ktn/fmap! (partial uniform-to-pareto-precalc 1 1) fiveunif2))
  (into [] fivepareto2) 
  (into [] fiveunif2)   ; now has those Pareto-distributed numbers as well, because fmap! modifies (:buf nums)
  (into [] (:buf nums)) ; numbers 5 through 9 are now the Pareto-distributed numbers in fivepareto2

  (def fiveunif3 (take-rand! 5 nums)) ; This takes the last (uniformly distributed) number in (:buf nums)
  (into [] fiveunif3)
  (into [] (:buf nums))  ; and refills the buffer with uniformly distributed numbers to get the other four
  (def fivepareto3 (ktn/fmap! (partial uniform-to-pareto-precalc 1 1) fiveunif3))
  (into [] fivepareto3) ; contains Pareto-distributed numbers
  (into [] fiveunif3)   ; contains same Pareto-distributed numbers
  (into [] (:buf nums)) ; now the first five in (:buf nums) are Pareto-distributed
  (def fiveunif4 (take-rand! 5 nums)) ; but the next five come from after that point
  (into [] fiveunif4)   ; where there are still
  (into [] (:buf nums)) ; uniformly distributed numbers

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
  "Return a long constructed by an Apache Commons RandomSource/createLong
  method, which is reasonably thought to produce uniform random numbers.
  (Comments near source for make-seed explain where the seed comes from.)"
  []
  (RandomSource/createLong))
;; To see where Apache 1.5's RandomSource/createLong() comes from, see:
;; docs: https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/RandomSource.html#createLong()
;; source: https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/src-html/org/apache/commons/rng/simple/RandomSource.html#line.1018
;; public static long createLong() {
;;    return SeedFactory.createLong();
;; }
;; seed factory doc: https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/org/apache/commons/rng/simple/internal/SeedFactory.html
;; source:
;; https://commons.apache.org/proper/commons-rng/commons-rng-simple/apidocs/src-html/org/apache/commons/rng/simple/internal/SeedFactory.html#line.110
;; Then go to line 47ff to see that it uses an xoroshiro1024++, seeded by a
;; SplitMix64 that is seeded by a java.security.SecureRandom genator, where
;; the SplitMix64 output is checked to make sure that it has no zeros longs
;; in its seed array, because xoroshiro generators apparently have a zeroland 
;; problem.  The check for zero longs occurs in the method ensureNonZero on
;; line 407 (end of source file).  If a position in the long array is = 0,
;; the code endureNonZero loops until SplitMix64 produces a nonzero value,
;; and uses that instead of the zero.
;; (I don't know what the SecureRandom generator is doing, but I trust it--whatever
;; Java is doing there ought to be plenty good.)

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

(defrecord NeanderRNG [size-1 buf rng prev-index$])
(defn make-neander-ars5
  ([^long bufsize] (make-neander-ars5 bufsize (make-seed)))
  ([^long bufsize seed] (let [i$ (atom -1)
                              buf (nn/dv bufsize)
                              rng (nran/rng-state nn/native-double seed)]
                          (nran/rand-uniform! rng buf)
                          (->NeanderRNG (dec bufsize) buf rng i$))))

;; Let's try it with deftype instead of defrecord
(deftype NeanderRNGtype [size-1 buf rng prev-index$])
(defn make-neander-ars5-type
  ([^long bufsize] (make-neander-ars5-type bufsize (make-seed)))
  ([^long bufsize seed] (let [i$ (atom -1)
                              buf (nn/dv bufsize)
                              rng (nran/rng-state nn/native-double seed)]
                          (nran/rand-uniform! rng buf)
                          (->NeanderRNGtype (dec bufsize) buf rng i$))))

(defprotocol IndexSetter
  (set-index [this i])
  (get-index ^long [this]))

;; Let's try it with mutable deftype:
(deftype NeanderRNGmut [size-1 buf rng ^:volatile-mutable index] ; ^:unsynchronized-mutable makes set! generate an error.  Why?
  IndexSetter
  (set-index [this i] (set! index i))
  (get-index [this] index))


(defn make-neander-ars5-mut
  ([^long bufsize] (make-neander-ars5-mut bufsize (make-seed)))
  ([^long bufsize seed] (let [buf (nn/dv bufsize)
                              rng (nran/rng-state nn/native-double seed)]
                          (nran/rand-uniform! rng buf)
                          (->NeanderRNGmut (dec bufsize) buf rng -1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS FOR SAVING/RESTORING PRNG STATE

;; I don't put get-state and set-state in defprotocol; not worth it:
;; Some states are just an array of numbers, nothing fancier.

;; Designed to work with any class that has a saveState method.
(defn get-state
  "Returns the internal state of a PRNG."
  [rng]
  (.saveState rng))

;; Designed to work with any class that has a restoreState method.
(defn set-state
  "Sets the internal state of an Apache Commons PRNG to a state derived from a PRNG
  of the same kind."
  [rng state]
  (.restoreState rng state))

;; Java i/o voodoo below is based on an example at:
;; https://commons.apache.org/proper/commons-rng/userguide/rng.html#a2._Usage_overview
;; It's buried in the "Usage overview" section.  Search for "Serializable".
;; I tried to do something more straightforward using more Clojure primitives,
;; but it didn't work.  Perhaps there are ways to simplify, but this works.

;; File size should be 2535 for Well19937, and 5603 for Well44497.
(defn write-apache-state
  "Write state from a single Apache Commons PRNG to a file.  State should
  be an org.apache.commons.rng.core.RandomProviderDefaultState ."
  [filename ^RandomProviderDefaultState state]
  (let [byte-stream (ByteArrayOutputStream.)]
    (.writeObject (ObjectOutputStream. byte-stream) (.getState state))
    (with-open [w (FileOutputStream. filename)]
      (.write w (.toByteArray byte-stream)))))

(def write-state
  "Alias for write-apache-state.  Writes state from a single Apache Commons
  PRNG to a file."
  write-apache-state)

(defn read-apache-state
  "Read state for a single Apache Commons PRNG from a file. State should be
  an org.apache.commons.rng.core.RandomProviderDefaultState ."
  [filename]
  (with-open [r (FileInputStream. filename)]
    (RandomProviderDefaultState.
      (.readObject (ObjectInputStream. r)))))

(def read-state
  "Alias for write-apache-state.  Read state for a single PRNG from a
  file."
  read-apache-state)

(defn write-mrg32k3a-state
  [filename state]
  (let [byte-stream (ByteArrayOutputStream.)]
    (.writeObject (ObjectOutputStream. byte-stream) state)
    (with-open [w (FileOutputStream. filename)]
      (.write w (.toByteArray byte-stream)))))

(defn read-mrg32k3a-state
  "Read state for a single Apache Commons PRNG from a file. State should be
  an org.apache.commons.rng.core.RandomProviderDefaultState ."
  [filename]
  (with-open [r (FileInputStream. filename)]
    (longs (.readObject (ObjectInputStream. r)))))

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
  (class state)
  (take 5 (repeatedly #(next-double newrng)))
  (set-state newrng state)
  (write-mrg32k3a-state "yomrg.bin" state)
  (def rng2 (make-mrg32k3a))
  (def oldstate (read-mrg32k3a-state "yomrg.bin"))
  (class oldstate)
  (set-state rng2 state)
  (take 5 (repeatedly #(next-double rng2)))

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

;; Based on Hörmann, Leydold, and Derflinger, _Automatic Nonuniform Random
;; Variate Generation_ p. 15.
(defn uniform-to-pareto-precalc
  "Given a value u from a uniformly distributed random number generator,
  returns a value from a pareto distribution based on min-value (\"scale\")
  parameter k and shape parameter alpha. However, rather than passing alpha,
  this function expects 1/alpha.  This allows calculating this constant
  once so that it can be used repeatedly in a partial'ed application. (For
  what I call a powerlaw distribution with parameter mu, mu = alpha + 1, or
  alpha = mu - 1.)"
  [^double k ^double alpha-reciprocal ^double u]
  (/ k (fm/pow (- 1 u)
               alpha-reciprocal)))

(defn uniform-to-pareto
  "Given a value u from a uniformly distributed random number generator,
  returns a value from a pareto distribution with min-value (\"scale\")
  parameter k and shape parameter alpha. (For what I call a powerlaw
  distribution with parameter mu, mu = alpha + 1, or alpha = mu - 1.)"
  [^double k ^double alpha ^double u]
  (uniform-to-pareto-precalc k (/ alpha) u))

(defn uniform-to-powerlaw
  "Given a value u from a uniformly distributed random number generator,
  returns a value from a powerlaw distribution with min-value (\"scale\")
  parameter k and shape parameter mu (or from a Pareto distribution with
  alpha = mu - 1)."
  [^double k ^double mu ^double u]
  (uniform-to-pareto-precalc k (/ (dec mu)) u))

(defn make-unif-to-powerlaw
  "Returns a function such that, given a value u from a uniformly
  distributed random number generator, returns a value from a powerlaw
  distribution with min-value (\"scale\") parameter k and shape parameter m
  (or from a Pareto distribution with alpha = mu - 1)."
  ([^double k ^double mu]
   (let [alpha-reciprocal (/ (dec mu))]
     (fn [^double u] (/ k (fm/pow (- 1 u) alpha-reciprocal)))))
  ([^double k ^double mu ^double max]
   "How to implement truncated powerlaw?"))


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
  "Provides a common interface to some functionality shared by PRNG 
  and distribution classes.  If low and high are provided, numbers outside
  this range (inclusive) are rejected."
  (next-double 
    [this]
    [this low high]
    "Gets the next double from a PRNG or distribution object.")
  (write-from-rng
    [this filename]
    "Writes state of PRNG this to filename, overwriting any existing file.")
  (read-to-rng
    [this filename]
    "Reads a PRNG state from filename and sets the state of PRNG this to it."))

(comment
  (clojure.repl/doc RandDist)
  (clojure.repl/doc next-double)
  (clojure.repl/doc write-from-rng)
  (clojure.repl/doc read-to-rng)
)

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
  (write-from-rng [this filename]
    (throw (Exception. "write-from-rng isn't implemented for InverseTransformParetoSampler.  Call it on the underlying rng instead.")))
  (read-to-rng [this filename]
    (throw (Exception. "read-to-rng isn't implemented for InverseTransformParetoSampler.  Call it on the underlying rng instead.")))

  MRG32k3aParetoSampler ; hacked version of InverseTransformParetoSampler
  (next-double
    ([this] (.sample this))
    ([this ^double low ^double high]
     (loop [x (.sample this)]
       (if (and (<= x high) (>= x low))
         x
         (recur (.sample this))))))
  (write-from-rng [this filename]
    (throw (Exception. "write-from-rng isn't implemented for MRG3k3aParetoSampler  Call it on the underlying rng instead.")))
  (read-to-rng [this filename]
    (throw (Exception. "read-to-rng isn't implemented for MRG3k3aParetoSampler  Call it on the underlying rng instead.")))

  ; PRNGS:
  Well1024a
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))
  (write-from-rng [this filename]
    (write-apache-state filename (get-state this)))
  (read-to-rng [this filename]
    (set-state this (read-apache-state filename)))

  Well19937c
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))
  (write-from-rng [this filename]
    (write-apache-state filename (get-state this)))
  (read-to-rng [this filename]
    (set-state this (read-apache-state filename)))

  Well44497b
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))
  (write-from-rng [this filename]
    (write-apache-state filename (get-state this)))
  (read-to-rng [this filename]
    (set-state this (read-apache-state filename)))

  MRG32k3a
  (next-double
    ([this] (.nextDouble this))
    ([this ^double low ^double high]
     (loop [x (.nextDouble this)]
                       (if (and (<= x high) (>= x low))
                         x
                         (recur (.nextDouble this))))))
  (write-from-rng [this filename]
    (write-mrg32k3a-state filename (get-state this)))
  (read-to-rng [this filename]
    (set-state this (read-mrg32k3a-state filename)))

  NeanRandNums ; USING next-double WITH NEANDERTHAL RANDOM NUMBER GENERATION IS SLOW
  (next-double
    ([this] (nreal/entry (take-rand! 1 this) 0))
    ([this ^double low ^double high]
     (loop [x (nreal/entry (take-rand! 1 this) 0)]
       (if (and (<= x high) (>= x low))
         x
         (recur (nreal/entry (take-rand! 1 this) 0))))))
  (write-from-rng [this filename] "write-from-rng not yet implemented for NeanRandNums\n")
  (read-to-rng    [this filename] "read-from-rng not yet implemented for NeanRandNums\n")

)


(comment
  (def ars5 (make-ars5 5 42))
  (next-double ars5)
  (next-double ars5 0.3 0.5)

  (def well1 (make-well44497))
  (write-from-rng well1 "yowell.bin")
  (take 8 (repeatedly #(next-double well1)))
  (def well2 (make-well44497))
  (read-to-rng well2 "yowell.bin")
  (take 8 (repeatedly #(next-double well2)))

  (def wella (make-well19937))
  (write-from-rng wella "yowell.bin")
  (take 8 (repeatedly #(next-double wella)))
  (def wellb (make-well19937))
  (read-to-rng wellb "yowell.bin")
  (take 8 (repeatedly #(next-double wellb)))

  (def mrg1 (make-mrg32k3a))
  (write-from-rng mrg1 "yomrg.bin")
  (take 8 (repeatedly #(next-double mrg1)))
  (def mrg2 (make-mrg32k3a))
  (read-to-rng mrg2 "yomrg.bin")
  (take 8 (repeatedly #(next-double mrg2)))
)


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

(defn unif-to-radian
  [^double u]
  (* 2 Math/PI u))

(defn next-radian
  "Given a PRNG rng, return a uniformly distributed number between 0
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
  [^ParetoDistribution dist x]
  (.density dist x))

(defn probability
  "Return the probability that a value from Apache Commons Math3 dist falls
  within (low,high]."
  [^ParetoDistribution dist low high]
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
    (fn [^ParetoDistribution dist ^double low ^double high ^double x]
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
  [rng num-samples ^java.util.Collection xs]
  (ListSampler/sample rng (ArrayList. xs) num-samples))

;; cf. clojure.core.shuffle, which uses a similar idea:
;; https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L7274
;; but doesn't allow specifying an RNG.  However, the java.util.Collections/shuffle
;; method does allow specifying an RNG.  However, it's a java.util.Random,
;; which is different from the Apache RNGs.
(defn shuffle
  "Given a random number generator rng and a collection xs, returns a
  collection (a java.util.ArrayList) in which the' elements have been
  randomly shuffled."
  [^UniformRandomProvider rng ^java.util.Collection xs]
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BENCHMARKING DIFFERENT PRNGS
(comment
  (require '[criterium.core :as crit])

  ;; COMPARISON OF DIFFERENT JAVA PRNGS:

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
          ;rng44497 (make-well44497 seed)
          ;rng19937 (make-well19937 seed)
          ;rng1024  (make-well1024 seed)
          rngMRG   (make-mrg32k3a seed)
         ]
      (println "seed =" seed)
      ;(println "WELL44497b:")
      ;(time (crit/bench (next-double rng44497)))
      ;(println "WELL19937c:")
      ;(time (crit/bench (next-double rng19937)))
      ;(println "WELL1024a:")
      ;(time (crit/bench (next-double rng1024)))
      (println "MRG32k3a:")
      (time (crit/bench (next-double rngMRG))))
  )

  ;; What if we make Neanderthal's random-uniform! fill a SINGLETON VECTOR?
  ;; Then Vigna's MRG32k3a beats Neanderthal/MKL's ARS5 by far:
  (def nvec1 (nn/dv 1))
  (time (let [seed (make-seed)
              rngMRG   (make-mrg32k3a seed)
              rngARS5  (nran/rng-state nn/native-double seed)]
      (println "MRG32k3a:")
      (time (crit/quick-bench (next-double rngMRG))) ; MPB quick-bench 8 ns, 10 ns
      (println "Neanderthal/MKL ARS5:")
      (time (crit/quick-bench (nran/rand-uniform! rngARS5 nvec1))))) ; MPB quick-bench 111 ns, 108 ns

  ;; Now let's TRY IT WITH A MILLION NUMBERS AT ONCE.  Note Neanderthal will fill
  ;; a Neanderthal vector, while the MRG32k3a numbers below will be thrown away immediately.
  (defn millionize
    [rng]
    (dotimes [_ 1000000]
      (next-double rng)))
  ; (time (millionize (make-mrg32k3a 42)))
  ;(defn millionize!
  ;  [rng array]
  ;  (dotimes [i 1000000]
  ;    (aset array i (next-double rng))))
  ;(def ra (double-array 1000000))
  ;(time (millionize! (make-mrg32k3a 42) ra)) ; ohh... this is really slow.

  (def twentyK 20000)
  (def thirtyK 30000)
  (def fortyK 40000)
  (def fiftyK 50000)
  (def hundredK 100000)
  (def million 1000000)

  ;; USING next-double WITH NEANDERTHAL RANDOM NUMBER GENERATION IS A BAD IDEA:
  (time (let [seed (make-seed)
              rngMRG   (make-mrg32k3a seed)
              ars5nums (make-nean-nums (nran/rng-state nn/native-double seed) million)]
          (println "MRG32k3a:")
          (time (crit/quick-bench (next-double rngMRG)))
          (println "\nNeanderthal/MKL ARS5:")
          (time (crit/quick-bench (next-double ars5nums)))
  ))
  ;; The MRG32k3a version is 10X faster than the Neanderthal ARS5 version,
  ;; even though it batch-generates random numbers.

  ;(def nvec1M (nn/dv million))
  ;(def nvec100K (nn/dv hundredK))

  ;; Without converting ARS5 buffer to another Clojure sequence format.
  ;; Note that the ARS5 buffer is refilled on each of quick-bench's
  ;; internal iterations, but no copying occurs because the count requested
  ;; is equal to the buf size.
  (time (let [seed (make-seed)
              mrgrng   (make-mrg32k3a seed)
              ars5nums (make-nums (nran/rng-state nn/native-double seed) hundredK)
              ;num-to-take hundredK
              num-to-take 1  ; Neanderthal wins, but not by a lot
             ]
          (println "\nNeanderthal/MKL ARS5:")
          (time (crit/quick-bench (take-rand! num-to-take ars5nums))) ; MBA 37, 38, 43, 47 μs (microsecs)
          (println "\nMRG32k3a doall repeatedly:")
          (time (crit/quick-bench (doall (repeatedly num-to-take #(next-double mrgrng))))) ; MBA 11, 12 ms (millisecs)
          (println "\nMRG32k3a vec repeatedly:")
          (time (crit/quick-bench (vec (repeatedly num-to-take #(next-double mrgrng))))) ; MBA 12, 14, 15 ms
          (println "\nMRG32k3a into [] repeatedly:")
          (time (crit/quick-bench (into [] (repeatedly num-to-take #(next-double mrgrng))))) ; MB 17, 18, 21 ms
          (println "\nMRG32k3a mapv:")
          (time (crit/quick-bench (mapv (fn [_] (next-double mrgrng)) (repeat num-to-take nil)))) ; 3.2, 3.3 ms  <- Interesting, but not as good as Neanderthal
          (println "\nMRG32k3a doall map:")
          (time (crit/quick-bench (doall (map (fn [_] (next-double mrgrng)) (repeat num-to-take nil)))))  ; 13 ms
  ))
  ;; REMARKS:
  ;; Neanderthal/ARS5 clearly wins by a lot in this sort of comparison.
  ;; (And as an aside, kind of shocking that mapv with MRG32K3a is an order of magnitude better than other MRG32k3a methods.)


  ;; Compare Neanderthal time with and without internal copying.
  ;; Since 20K maps evenly into 100K, taking 20K repeatedly from a 100K
  ;; buffer causes no copying, though it does cause refilling.
  ;; But taking 20K repeatedly from a 50K buffer causes copying as well
  ;; as refilling. (See definition of take-rand!.)
  (time (let [seed (make-seed)
              nums50K (make-nums (nran/rng-state nn/native-double seed) fiftyK)
              nums100K (make-nums (nran/rng-state nn/native-double seed) hundredK)]

          (time (crit/quick-bench (take-rand! twentyK nums50K)))  ; 11, 12 μs
          (time (crit/quick-bench (take-rand! twentyK nums100K))) ; 7.5, 8 μs

          (time (crit/quick-bench (take-rand! thirtyK nums50K)))  ; 19, 20 μs
          (time (crit/quick-bench (take-rand! thirtyK nums100K))) ; 15, 16 μs

          (time (crit/quick-bench (take-rand! fortyK nums50K)))   ; 23 μs
          (time (crit/quick-bench (take-rand! fortyK nums100K)))  ; 23, 24 μs
  ))
  ;; REMARKS:
  ;; Taking more numbers *should* take more time in quick-bench, and this
  ;; was so.  In the second test, there was no copying; in all the others
  ;; there was. However, in the fourth test, there was less copying than
  ;; in its predecessor, which explains why the time was better in the
  ;; fourth.  Interesting that the time in the last two was about the same.
  ;; Maybe because the amount of copying was more similar.

  (time (let [seed (make-seed)
              nums100K (make-nums (nran/rng-state nn/native-double seed) hundredK)
              nums1M (make-nums (nran/rng-state nn/native-double seed) million)]

          (time (crit/quick-bench (take-rand! hundredK nums100K))) ; MBA 40, 43 μs; MBP 31 μs 
          (time (crit/quick-bench (take-rand! hundredK nums1M)))   ; MBA 49, 50, 53 μs; MBP 31 μs 

          (time (crit/quick-bench (take-rand! million nums1M))) ; MBA 520 μs; MBP 314 μs 
  ))
  ;; REMARKS:
  ;; Interesting that the second one is slower than the first on the MBA. That's not what I expected.
  ;; I don't know why.  Hmm.  Maybe it's about caching??  The third seems analogous.
  ;; But on the MBP they are the same.  That's consistent with the caching hypothesis.


  ;; WHAT IS THE EFFECT OF DERIVING POWER-LAW NUMBERS FROM ARS5 IN A SIMPLISTIC MANNER?
  (require '[criterium.core :as crit])
  (time (let [seed (make-seed) 
              ;seed -2487152234454198998
              hundredK 100000
              mrgrng (make-mrg32k3a seed)
              mrgpow (make-mrg32k3a-powerlaw mrgrng 1 2)
              ;; AFAICS these are equally fast, as I'd have guessed:
              ;ars5nums (make-nums (nran/rng-state nn/native-double seed) hundredK)
              ars5nums (make-nean-nums (nran/rng-state nn/native-double seed) hundredK)
              powerlaw-fn (make-unif-to-powerlaw 1 2)
              partialed-pareto-fn (partial uniform-to-pareto-precalc 1 1)
              curried-pareto-fn (ktn/curry uniform-to-pareto-precalc)]
          (println "seed:" seed)

          ;; Slow:
          ;(println "MRG32k3a with MRG32K3aParetoSampler:")
          ;(time (crit/quick-bench (doall (repeatedly hundredK #(next-double mrgpow)))))

          ;; Slow:
          ;(println "\nNeanderthal/MKL ARS5 with partial and Clojure map:")
          ;(time (crit/quick-bench (mapv partialed-pareto-fn (take-rand! hundredK ars5nums))))

          ;; Slow:
          ;(println "\nNeanderthal/MKL ARS5 with fn and Clojure map:")
          ;(time (crit/quick-bench (mapv (fn ^double [^double x] (uniform-to-pareto-precalc 1 1 x))
          ;                              (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with make-unif-to-powerlaw and Fluokitten fmap:")
          (time (crit/quick-bench (ktn/fmap powerlaw-fn (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with make-unif-to-powerlaw and Fluokitten fmap!:")
          (time (crit/quick-bench (ktn/fmap! powerlaw-fn (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with fn and Fluokitten fmap:")
          (time (crit/quick-bench (ktn/fmap (fn ^double [^double x] (uniform-to-pareto-precalc 1 1 x)) 
                                           (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with fn and Fluokitten fmap!:")
          (time (crit/quick-bench (ktn/fmap! (fn ^double [^double x] (uniform-to-pareto-precalc 1 1 x))
                                            (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with partial and Fluokitten fmap:")
          (time (crit/quick-bench (ktn/fmap partialed-pareto-fn (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with partial and Fluokitten fmap!:") 
          (time (crit/quick-bench (ktn/fmap! partialed-pareto-fn (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with Fluokitten curry and fmap:")
          (time (crit/quick-bench (ktn/fmap (curried-pareto-fn 1 1) (take-rand! hundredK ars5nums))))

          (println "\nNeanderthal/MKL ARS5 with Fluokitten curry and fmap!:") 
          (time (crit/quick-bench (ktn/fmap! (curried-pareto-fn 1 1) (take-rand! hundredK ars5nums))))

          ;; Slow:
          ;(println "\nNeanderthal/MKL ARS5 with Fluokitten fmap! using sample method from MRG32k3aParetoSampler:")
          ;(time (crit/quick-bench (ktn/fmap! (fn [u] (.sample mrgpow u)) (take-rand! hundredK ars5nums))))

          ;; Slow:
          ;(println "\nNeanderthal/MKL ARS5 with Fluokitten fmap! using sample method from MRG32k3aParetoSampler, type hints:")
          ;(time (crit/quick-bench (ktn/fmap! (fn ^double [^double u] (.sample mrgpow u)) (take-rand! hundredK ars5nums))))

          ;; Irrelevant:
          ;(println "\nNeanderthal/MKL ARS5 uniform random, fmap!-ing identity:")
          ;(time (crit/quick-bench (ktn/fmap! identity (take-rand! hundredK ars5nums))))

          ;; Irrelevant:
          ;(println "\nNeanderthal/MKL ARS5 uniform random, fmap!-ing type-hinted identity:")
          ;(time (crit/quick-bench (ktn/fmap! (fn ^double [^double x] x) (take-rand! hundredK ars5nums))))

          ;; Irrelevant:
          ;(println "\nNeanderthal/MKL ARS5 uniform random--no mapping:")
          ;(time (crit/quick-bench (take-rand! hundredK ars5nums)))

  ))
  ;; Note all runs use my uniform-to-pareto-precalc function unless noted.
  ;; 
  ;; TAKEAWAY FROM MBA EXPERIMENTS:
  ;; Using quick-bench on MBA:
  ;; Using fmap or fmap! rather than map to convert uniform numbers to
  ;; Pareto numbers  is a lot faster with the Fluokitten map functions,
  ;; but only by 4X, 5X, almost 6X at best.
  ;; Just using fmap! with any Clojure function, including identity, is
  ;; what adds most of the additional time.  (So it's not that my pareto
  ;; conversion is slow.)
  ;;
  
  ;; On MBP, with quick-bench the profile is somewhat different:
  ;;
  ;; 11 ms        MRG32k3a with MRG32K3aParetoSampler
  ;; 5-8 ms       Neanderthal/MKL ARS5 with partial and Clojure map
  ;; 5-8 ms       Neanderthal/MKL ARS5 with fn and Clojure map
  ;; ~500 μs      Neanderthal/MKL ARS5 with Fluokitten partial and fmap
  ;; 230-1700 μs  Neanderthal/MKL ARS5 with Fluokitten partial and fmap!
  ;; 480-1500 μs  Neanderthal/MKL ARS5 with Fluokitten fn and fmap
  ;; 236-1400 μs  Neanderthal/MKL ARS5 with Fluokitten fn and fmap!
  ;; 1.3-1.4ms    Neanderthal/MKL ARS5 with make-unif-to-powerlaw and Fluokitten fmap
  ;; 1.3-1.5ms    Neanderthal/MKL ARS5 with make-unif-to-powerlaw and Fluokitten fmap!
  ;; 67 ms        Neanderthal/MKL ARS5 with Fluokitten fmap! using sample method from MRG32k3aParetoSampler
  ;; 45 ms        Neanderthal/MKL ARS5 with Fluokitten fmap! using sample method from MRG32k3aParetoSampler, type hints
  ;; 1.3 ms       Neanderthal/MKL ARS5 uniform random, fmap!-ing identity
  ;; 1.3 ms       Neanderthal/MKL ARS5 uniform random, fmap!-ing type-hinted identity
  ;; 33-35 μs     Neanderthal/MKL ARS5 uniform random--no Pareto mapping
  ;;
  ;; Summary:
  ;;
  ;; Using ARS5 with my pareto function and Clojure map is up to twice as
  ;; fast as using MRG32k3a with the associated Pareto sampler.
  ;; 
  ;; But, using Fluokitten's fmap functions is sometimes an order of
  ;; magnitude faster than using Clojure's map.  But not consistently.
  ;; Not in the full bench runs below.  So maybe the norm is 4 or 5X
  ;; faster.
  ;;
  ;; Q: WHAT's GOING ON WITH THE WIDE VARIATION IN TIMES FOR THE fmap/fmap! RUNS?
  ;; It happens with both quick-bench and bench (see below).
  ;; 
  ;; A: I don't think it's an artifact of seed choice--I tried reruns with
  ;; the same seed and got the variation between fast an slow.   The fast runs
  ;; mostly occur consistently in the first time after I've restarted Clojure.
  ;; Is it GC? But does that theory make sense?  The effect is just as strong 
  ;; with bench as quick-bench, but if it was GC you'd expect some increase
  ;; in the later tests with bench compared to quick-bench.
  ;;
  ;; You'd think that using the Java method in MRG32k3aParetoSampler would
  ;; be at least as fast as my uniform-to-pareto-precalc, but instead it's
  ;; much slower than any other method, even using fmap/fmap!
  ;; Hmm maybe because it's boxing the numbers?  And using fastmath and
  ;; type hints, that's not happening in Clojure?
  ;; 
  ;; fmap! seems to be consistently about twice as fast as fmap in the fast
  ;; runs, but they're very close in the slow runs (including the bench run).
  ;; 
  ;; Later I inclucded the new make-unif-to-powerlaw generated function,
  ;; and it was at least as fast as the other uses of fmap/fmap!.
  ;;
  ;; Using bench rather than quick-bench:
  ;;
  ;; 10-11 ms     MRG32k3a with MRG32K3aParetoSampler
  ;; 5.5-5.6 ms   Neanderthal/MKL ARS5 with partial and Clojure map
  ;; 5.3-7.6 ms   Neanderthal/MKL ARS5 with fn and Clojure map
  ;; 496μs-1.6ms  Neanderthal/MKL ARS5 with Fluokitten partial and fmap
  ;; 220μs-1.6 ms Neanderthal/MKL ARS5 with Fluokitten partial and fmap!
  ;; 502μs-1.5 ms Neanderthal/MKL ARS5 with Fluokitten fn and fmap
  ;; 221μs-1.4 ms Neanderthal/MKL ARS5 with Fluokitten fn and fmap!
  ;; 43-63 ms     Neanderthal/MKL ARS5 with Fluokitten fmap! using sample method from MRG32k3aParetoSampler
  ;; 57-62 ms     Neanderthal/MKL ARS5 with Fluokitten fmap! using sample method from MRG32k3aParetoSampler, type hints
  ;; 1.2-1.3 ms   Neanderthal/MKL ARS5 uniform random, fmap!-ing identity
  ;; 1.0-1.4 ms   Neanderthal/MKL ARS5 uniform random, fmap!-ing type-hinted identity
  ;; 32-33 μs     Neanderthal/MKL ARS5 uniform random--no mapping



)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BENCHMARKING PRNGs IN WALK GENERATION
(comment
  (require '[criterium.core :as crit])
  (require '[forage.core.walks :as w])

  (time (let [seed (make-seed) 
              ;rng1024 (make-well1024 seed)
              ;pow1024 (make-powerlaw rng1024 1 2)
              mrgrng (make-mrg32k3a seed)
              mrgpow (make-mrg32k3a-powerlaw mrgrng 1 2)
              ars5nums (make-nums (nran/rng-state nn/native-double seed) hundredK)
              powerlaw-fn (make-unif-to-powerlaw 1 2)]

  ))
 

  (def stepfn19937 (w/step-vector-fn rng19937 pow19937 1 25000))
  (def stepfn1024 (w/step-vector-fn rng1024 pow1024 1 25000))
  (def stepfnmrg (w/step-vector-fn rngmrg powmrg 1 25000))

                                    ; Two bench runs each:
  (time (crit/bench (stepfn19937))) ; 82 ns, 79 ns
  (time (crit/bench (stepfn1024)))  ; 71 ns, 75 ns
  (time (crit/bench (stepfnmrg)))   ; 54 ns, 57 ns
  ;;  i.e. Well19937 takes 40-50% longer relative to MRG32k3a
  ;;  i.e. Well1024 takes 30% longer relative to MRG32k3a

  (def mu2_19937 (w/make-levy-vecs rng19937 pow19937 1 25000))
  (def mu2_1024 (w/make-levy-vecs rng1024 pow1024 1 25000))
  (def mu2_mrg (w/make-levy-vecs rngmrg powmrg 1 25000))

  (time (crit/quick-bench (doall (take 25000 mu2_19937)))) ; 1.37 ms
  (time (crit/quick-bench (doall (take 25000 mu2_1024))))  ; 1.42 ms
  (time (crit/quick-bench (doall (take 25000 mu2_mrg))))   ; 1.39 ms

  (time (crit/bench (doall (take 25000 mu2_19937)))) ; 1.40 ms, 1.39 ms
  (time (crit/bench (doall (take 25000 mu2_1024))))  ; 1.26 ms, 1.34 ms
  (time (crit/bench (doall (take 25000 mu2_mrg))))   ; 1.17 ms, ; 1.35 ms
  ;; So Well19937 takes about 20% longer than MRG32k3a, sometimes.
  ;; But they are clearly comparable, at least.
  ;; So the PRNG differences are getting washed out by the rest of the
  ;; computation.  (But I think MRG32k3a is still better than the WELL 
  ;; generators.)

)
