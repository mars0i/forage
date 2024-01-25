

  ;; I think they idea of a factory is that it makes numbers or allocates
  ;; space in the way appropriate for the CPU ("native") or for a GPU (e.g. CUDA):
  ;; Using an existing factory:
  (def rng (nr/rng-state nn/native-double (make-seed)))

  (def randvec (nn/dv 250000000)) ; length 250 million is OK, but much more is an error.
  (def randvec (nn/dv   1000000)) ; make a Neanderthal vector for a million floats
  (def randvec (nn/dv   100000)) ; make a Neanderthal vector for a 100K floats
  (time (nr/rand-uniform! rng randvec))  ; populate the Neanderthal vector with random numbers

  ;; Simple illustrations:
  (nc/entry randvec 999999) ; index into the Neanderthal vector
  (class randvec)
  (class (take 20 randvec))
  (realized? (drop 20 randvec))
  (def k (time (doall (take 20 randvec))))
  (drop (- 1000000 20) randvec) ; the result is a lazy seq. Feels slow.

  (first randvec) ; and first and second work
  (nth randvec 0) ; nth generates an error, howewver 
  (nc/entry randvec 999999) ; built-in Neanderthal access method
  (get randvec 999999) ; get doesn't error, but you get back a nil
  (randvec 999999) ; you can use map-style indexing though, and it's fast

  (def cvec (into [] randvec)) ; converts into a Clojure vector
  (vec nvec) ; doesn't work

  ;; The vector returned by rand-uniform! is the same one given as input:
  (def randvec (nn/dv 1000000))
  (def nvec (nr/rand-uniform! rng randvec))
  (identical? nvec randvec)

  (nvec) ; if you call the vector with no arguments, it returns its length (a misfeature imo)

  (def nvec (nr/rand-uniform! rng (nn/dv 20)))
  (vec nvec)

  ;; UNFORTUNATELY, THIS DOESN'T WORK WITH take:
  (take 10 nvec) ; if > 10 entries no error--just returns whatever is available.  Ack!

